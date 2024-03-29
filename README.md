# TLDR
<p> This project was for an indepedent study focusing on docker and statistical analysis. To run the file navigate to the correct folder and run the command below. </p>
<p> docker run --rm -p 3838:3838 covid-dashboard </p>
<p> It should open up the site in a new tab but if it doesn't there will be a link at the towards end of the output which looks like this and should take you to the site. </p>
<p> Listening on http://0.0.0.0:3838 </p>

<p> UPDATE: The NYT has stopped collecting seperate counts as of May 13, 2022 due to those figures no longer being commonly reported. </p>
	
# Project 1: COVID-19 Statistics Dashboard
## By Shree Madan
### 1. Introduction
In this project, I worked to develop a COVID-19 statistics dashboard which would populate county-level statistics based on zip code and an inputted date. There were three aspects to this project: the filtering of various datasets for the statistics, building a Shiny app, and using Docker to create a container to run the app on any machine easily. In Docker, we are able to easily install any software that’s needed to run the application. Without it, a user would need to install R as well as several packages and then run the script; however in Docker you just need to open up the container. These components work together to create a dashboard which can run with limited set up on any computer. The code was mostly done in R and using Dockerfile directives. </p>
### 2. Filtering in R 
The filtering done in this project mostly came from Challenge 06 of the Data Science class. However, one additional feature which was added was being able to go from zip code to fips code which is a county-level code. To do this, when the input zip code was taken in, I filtered the dataset containing zip codes and fips codes and assigned that to a separate variable. That variable was then used to filter the larger dataset containing the covid statistics. This is the data table which is displayed in the user interface. Furthermore, we can filter it further by date as well.
### 3. Shiny App
There are two main components to the Shiny app: the user interface and the server logic. The user interface takes in two inputs, a zip code and a date, and renders a data table as the end result. Both the inputs are reactive meaning they update in response to the user input. So if they are left empty, there will be no filters and the entire data table will render. If only one is filled out, it will only filter against that input rather than both. 

One thing to note is that with the date input, since it’s a string it can actually do partial dates such as a year or a specific month. Rather than attempt to make it so a full date or zip code needed to be put in, I chose to leave it like this because it provides greater flexibility with the data. For example, if someone wants to look at the bigger picture of the last week, it’s easier to do so.

The server logic is a combination of the data filtering from the COVID challenge previously mentioned and the function. Most of the filtering comes before the function in order to get the dataset clean. Once that’s complete, it’s then called into a function called “zip2fip” which filters the dataset using the converted zip code and the date. This part is the function in the server logic and is a Shiny-specific function that takes the input and wraps the filtering of the cleaned dataset returning specific statistics to display.
### 4. Docker Container
#### 4.1 Use-Case for Docker
Using a docker container was an extremely important part of this project. In the container, I’m able to ensure that any packages which are needed to run the dashboard are guaranteed to be available regardless of the person’s machine meaning this project should be able to run on any machine without a problem. This includes a level of version control because I have specified the exact versions needed (version pinning). While most of my code is done in R it’s not necessary for someone to have R installed on their machine in order to run the code. Also, the way I have written the code means that any time someone runs the container, the associated files and datasets will pull the latest data regarding COVID-19 infections from the New York Times which is vital for providing the most relevant information.
#### 4.2 Container Setup
To set up the container, I wrote a Dockerfile which ran R and shiny. Within this file I also had directives which downloaded the necessary packages for my app to run. Within the container, I also had a command which copied the files containing the source code for the app as well as assigning it to a port which can be accessed from the browser.

	FROM rocker/shiny:4.1.1
	RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \
	libssl-dev
	RUN Rscript -e 'install.packages("shiny")'
	RUN Rscript -e 'install.packages("dplyr")'
	RUN Rscript -e 'install.packages("readr")'
	RUN Rscript -e 'install.packages("tidyr")'
	RUN Rscript -e 'install.packages("stringr")'
	RUN Rscript -e 'install.packages("curl")'
	COPY /test ./app
	EXPOSE 3838
	CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]

To break down the Dockerfile, there were a few different aspects that I needed to include. Right off the bat, the container needed to run in an R Shiny environment which is what line 1 is setting up. Here the version is specified at 4.1.1 which is the version currently installed on my machine. Lines 2-4 are simply making sure any package dependencies are up-to-date before lines 5-10 install individual packages which are used in the Shiny app. One thing to note is that many of these packages are included in the tidyverse package which is often the go-to when using R. However, the number of packages within tidyverse can cause your machine to significantly slow down so it was much more efficient to go in and individually specify the packages. Line 11 then copies over the files needed from a folder within the same directory as the Dockerfile.  When running a Shiny app on a Linux machine, it will run on a particular port. For the sake of simplicity, we specify the port in Line 12 as 3838 so that finding the port and going back to that particular site will be relatively simple. Finally, in Line 13 the container is told what is the first command it should run as soon as the container is opened, which is the runApp command to port 3838.
### 5. Reflection
I had a few key takeaways from this project. First, I went into this with the mindset that learning Docker would be like learning any other programming language; however, I realise now that it’s actually very different. Docker is an assembly language where there are very limited commands that do a lot of things. This was my first experience using assembly language so it took me time to understand how to use Docker to do what I intended it to. Within assembly languages, developers must be much more intentional about each line of code and from my experience must be more creative since there is definitely a far smaller arsenal of commands to use. It was also an interesting process to debug in Docker because often there wouldn’t be an error message or very much documentation of others having the same problem. For example, when determining the packages needed, I reflexively ran the container in the background which meant that I didn’t have any error messages. When I finally identified this as a problem, it was a much simpler debugging process since I was able to see exactly where the app was stopping and oftentimes why it was stopping as well. Debugging definitely pushed the limits of my problem-solving abilities and forced me to think creatively. I also learnt a lot using Shiny. While I have some experience with R, using the Shiny package was completely different and required an almost different set of skills. 

There are a few different ways I would improve this dashboard. First, all NYT COVID statistics are cumulative from January 2020 and I’d like to find a way to turn those statistics into daily statistics instead. Additionally I think it would be interesting to include some kind of graphical element showing trends over the last 7 days or creating a sparkline for a specified period of time. While the current dashboard focuses on cases and deaths, I’d like to expand it to also include vaccinations rates since that has definitely also become an important topic over the last year.
