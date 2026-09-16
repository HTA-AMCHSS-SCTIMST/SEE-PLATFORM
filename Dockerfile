# Use a newer, more stable base image
FROM rocker/shiny:4.4.0

# Install system dependencies for R packages (mongolite, openssl, etc.)
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . /app

# Explicitly update shiny and install required packages to avoid version check errors
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('mongolite', 'jsonlite', 'ggplot2', 'plotly', 'httr2', 'openssl'), repos='https://cloud.r-project.org')"

# Expose the port Shiny runs on
EXPOSE 3838

# Start the app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
