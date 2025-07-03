FROM rocker/shiny:latest

# Install necessary R packages
RUN R -e "install.packages(c('pheatmap', 'gplots', 'RColorBrewer', 'dendextend', 'shiny', 'shinyjs', 'shinycssloaders', 'BiocManager', 'dplyr', 'this.path', 'stats'), repos='http://cran.rstudio.com/')"

RUN R -e "BiocManager::install(c('clusterProfiler', 'KEGGREST', 'org.Hs.eg.db'))"
# Copy the application files to the image
COPY ./app /srv/app/

WORKDIR /srv/app

# Expose the port for the Shiny app
EXPOSE 3838

# Set the command to run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/app/view/app.R', host='0.0.0.0', port=3838)"]