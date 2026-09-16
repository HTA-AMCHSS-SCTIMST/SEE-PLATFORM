# Use the stable R 4.4.0 image
FROM rocker/shiny:4.4.0

# Install essential system dependencies
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

# 0. Pin xfun to version 0.55 to bypass the 'attr' export conflict
RUN R -e "install.packages('remotes', repos='https://packagemanager.posit.co/cran/__linux__/bookworm/latest')"
RUN R -e "remotes::install_version('xfun', version='0.55', repos='https://packagemanager.posit.co/cran/__linux__/bookworm/latest')"

# 1. Install the rest of the packages
RUN R -e "install.packages(c('mongolite', 'jsonlite', 'ggplot2', 'plotly', 'httr2', 'openssl', 'shiny'), repos='https://packagemanager.posit.co/cran/__linux__/bookworm/latest', dependencies = TRUE)"

# 2. VERIFICATION STEP: Forces the build to fail if mongolite isn't loaded correctly
RUN R -e "if (!requireNamespace('mongolite', quietly = TRUE)) stop('mongolite failed to install correctly!')"

# Expose the port Shiny runs on
EXPOSE 3838

# Start the app
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]
