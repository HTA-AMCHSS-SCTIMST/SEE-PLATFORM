# Use the stable R 4.4.0 image
FROM rocker/shiny:4.4.0

# Install ALL required system dependencies for mongolite and others
# We add libssl and libcurl explicitly to ensure the MongoDB C-driver compiles
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . /app

# Install mongolite separately first to ensure it's linked correctly
# We use dependencies = TRUE to make sure everything it needs is present
RUN R -e "install.packages('mongolite', repos='https://cloud.r-project.org', dependencies = TRUE)"

# Install the remaining required packages
RUN R -e "install.packages(c('jsonlite', 'ggplot2', 'plotly', 'httr2', 'openssl'), repos='https://cloud.r-project.org')"

# Expose the port Shiny runs on
EXPOSE 3838

# Start the app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
