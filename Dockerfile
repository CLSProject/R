FROM rocker/shiny:latest

# Install necessary R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'shinycssloaders', 'clusterProfiler', 'org.Hs.eg.db', 'BiocManager', 'KEGGREST', 'dplyr', 'this.path', 'stats'), repos='http://cran.rstudio.com/')"

# Copy the application files to the image
COPY ./ /srv/shiny-server/

# Expose the port for the Shiny app
EXPOSE 3838

# Set the command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/gui/GUI_Layout.R', host='0.0.0.0', port=3838)"]