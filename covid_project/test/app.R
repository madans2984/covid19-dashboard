#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)


# Define UI for application that draws a histogram
ui <- fluidPage(
    textInput("zipcode", "Zipcode"),
    textInput("date", "Date (YYYY-MM-DD)"),
    dataTableOutput("out")
)
# Define server logic required to filter and display covid cases for the county of a specific zip code
server <- function(input, output, session) {

    # Loading zip code to county/FIPS data  
    zip2fips <- "ZIP-COUNTY-FIPS_2018-03.csv"
    df_fips <- read_csv(zip2fips)
    
    # Loading population data
    df_pop <- read_csv("ACSDT5Y2018.B01003_data_with_overlays_2021-10-05T112340.csv", skip = 1)
    
    # Loading NYT covid cases data. Every time this is run it will update with the latest statistics
    url_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
    filename_nyt <- "nyt_counties.csv"
    curl::curl_download(
        url_counties,
        destfile = filename_nyt
    )
    df_covid <- read_csv(filename_nyt)
    
    # Creating a fips column in the population data by extracting the last 5 digits of the id.
    # The fips code is what is connecting all three of our data sets.
    df_pop <- 
        df_pop %>%
        separate(
            col = "id",
            into = c(NA, "fips"),
            sep = -5
        )
    
    # Joining covid stats from NYT with population data using the fips column.
    df_covid <- 
        df_covid %>%
        left_join(
            df_pop,
            by = "fips"
        )
    
    # Selecting the relevant statistics to make the data set easier to work with. 
    df_data <-
        df_covid %>%
        select(
            date,
            county,
            state,
            fips,
            cases,
            deaths,
            population = `Estimate!!Total`
        )
    
    # Using the population statistics to create cases/deaths per 100k stats.
    df_data <- 
        df_data %>%
        mutate(
            cases_per100k = cases / population * 100000
        ) %>%
        mutate(
            deaths_per100k = deaths / population * 100000
        )
    
    # The function below takes the zip code and date as text inputs and then returns a data table which has relevant covid statistics at the county-level.
    zip2fip <- function(zip, Date, output) {
      
        # Taking the zip code input and extracting the fips code
        temp_zip <-
            df_fips %>%
            filter(str_detect(`ZIP`, zip)) %>%
            pull(STCOUNTYFP)
        
        # Taking the normalized data and filtering by the fips and date.
        # Selecting date, cases, deaths, cases_per100k, and deaths_per100k
        df_data %>%
            filter(fips == temp_zip) %>%
            filter(str_detect(`date`, Date)) %>%
            select(
                date,
                cases,
                deaths,
                cases_per100k,
                deaths_per100k
            )
        
    }
    
    # Actually function call using data entered by user
    ans <- reactive(
        zip2fip(input$zipcode, input$date)
    )
    
    # Rendering data table
    output$out <- renderDataTable(ans())
    
}
# Run the application 
shinyApp(ui = ui, server = server)

