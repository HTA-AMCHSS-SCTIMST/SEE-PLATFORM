# Use the stable R 4.4.0 image
FROM rocker/shiny:4.4.0

# Install ALL possible system dependencies for mongolite, openssl, and curl
# We add libmongoc-dev and libbson-dev to be absolutely sure
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libmongoc-dev \
    libbson-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY . /app

# 1. Install the packages
RUN R -e "install.packages(c('mongolite', 'jsonlite', 'ggplot2', 'plotly', 'httr2', 'openssl', 'shiny'), repos='https://cloud.r-project.org', dependencies = TRUE)"

# 2. VERIFICATION STEP: This will force the build to FAIL here if mongolite cannot be loaded.
# This prevents the app from deploying if the installation is broken.
RUN R -e "if (!requireNamespace('mongolite', quietly = TRUE)) stop('mongolite failed to install correctly!')"

# Expose the port Shiny runs on
EXPOSE 3838

# Start the app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
