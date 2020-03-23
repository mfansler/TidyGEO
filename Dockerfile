FROM rocker/shiny-verse:3.6.1

# Install necessary R packages and prepare Shiny server dir.
RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    lbzip2 \
    libv8-dev \
    openjdk-8-jdk liblzma-dev libbz2-dev libicu-dev libssl-dev \
  && R CMD javareconf \
  && install2.r --error --deps TRUE \
    assertive \
    data.table \
    DT \
    feather \
    fs \
    later \
    Rcpp \
    RColorBrewer \
    RCurl \
    rJava \
    rhandsontable \
    shinyBS \
    shinycssloaders \
    shinyFiles \
    shinyjs \
    xlsx \
    yaml \
 && R -e "devtools::install_github('AnalytixWare/ShinySky')" \
 && R -e "BiocManager::install('GEOquery')" \
 && rm -rf /srv/shiny-server/* \
 && mkdir /srv/shiny-server/TidyGEO

#COPY app.R /srv/shiny-server/TidyGEO/
#COPY www/ /srv/shiny-server/TidyGEO/www/

USER shiny
