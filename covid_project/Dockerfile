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
