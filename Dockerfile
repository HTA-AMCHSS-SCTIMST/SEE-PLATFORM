FROM rocker/shiny:latest
RUN apt-get update && apt-get install -y \
    libssl-dev libsasl2-dev libcurl4-openssl-dev libxml2-dev
WORKDIR /srv/shiny-server/app
COPY . /srv/shiny-server/app
RUN R -e "install.packages(c('shiny', 'mongolite', 'jsonlite', 'ggplot2', 'plotly', 'SHELF', 'htmltools', 'bslib', 'httr2', 'openssl'), repos='https://cloud.r-project.org/')"
EXPOSE 3838
CMD ["sh", "-c", "R -e \"shiny::runApp('/srv/shiny-server/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '3838')))\""]