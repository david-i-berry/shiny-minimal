# Docker image, pin for reproducibility
FROM rocker/shiny:4.2.1

# R packages
RUN R -e 'install.packages(c("shiny", "leaflet"))'

# Copy app and data files to server
COPY ./app/* /srv/shiny-server/

# Now run app
CMD ["/usr/bin/shiny-server"]