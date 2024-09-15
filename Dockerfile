# Use the official R image as the base image
FROM r-base:latest

# Install RShiny and other necessary packages
RUN R -e "install.packages(c('shiny', 'leaflet', 'plotly', 'shinyjs', 'shinyBS', 'leaflet.providers', 'leaflet.extras', 'osmdata', 'sp', 'dplyr', 'tidyr', 'magrittr', 'lubridate', 'xts', 'jsonlite', 'urltools', 'utils', 'rvest', 'stringr', 'xml2', 'selectr', 'purrr', 'RColorBrewer', 'DT', 'terra', 'data.table', 'sf', 'shinycustomloader', 'sfarrow'), repos='https://cran.rstudio.com/')"

# Copy the app to the image
COPY . /srv/shiny-server/

# Expose the port the app runs on
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
