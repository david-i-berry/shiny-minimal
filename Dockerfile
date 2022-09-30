# Docker image, pin for reproducibility
FROM rocker/shiny:4.2.1

# system dependencies
RUN apt-get -y update && apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev

# R packages
RUN R -e 'install.packages(c("shiny", "leaflet", "jsonlite", "sf", "geojsonsf", "httr"))'

# Copy app and data files to server
COPY ./app/* /srv/shiny-server/

# Now run app
CMD ["/usr/bin/shiny-server"]