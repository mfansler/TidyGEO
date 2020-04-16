FROM rocker/shiny-verse:3.6.1

# Install necessary R packages and prepare Shiny server dir.
RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    lbzip2 libv8-dev libjpeg-dev libgeos-dev libudunits2-0 libudunits2-dev \
    libgdal-dev openjdk-8-jdk liblzma-dev libbz2-dev libicu-dev libssl-dev \
  && R CMD javareconf \
  && install2.r --error --deps TRUE \
    assertive \
    data.table \
    DT \
    feather \
    fs \
    httr \
    janitor \
    later \
    plotly \
    Rcpp \
    RColorBrewer \
    RCurl \
    rJava \
    rhandsontable \
    rlang \
    shinyBS \
    shinycssloaders \
    shinydashboard \
    shinyjs \
    shinyWidgets \
    xlsx \
    xml2 \
    yaml \
 && R -e "devtools::install_github('AnalytixWare/ShinySky')" \
 && R -e "BiocManager::install('GEOquery')" \
 && rm -rf /srv/shiny-server/* \
 && mkdir /srv/shiny-server/TidyGEO \
 && ln -s /srv/shiny-server/TidyGEO /TidyGEO

COPY . /srv/shiny-server/TidyGEO/

WORKDIR /srv/shiny-server

RUN Rscript TidyGEO/Combine_Series_Platforms.R
RUN Rscript TidyGEO/generate_rscript_functions.R

USER shiny
