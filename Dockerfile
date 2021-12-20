FROM rocker/geospatial:3.6.2

RUN apt-get --allow-releaseinfo-change update   
RUN apt-get update && apt-get upgrade -y && rm -rf /var/lib/apt/lists/*

ENV PASSWORD=IcantB3li3v3itsN)TbutterCanYou

RUN export ADD=shiny && bash /etc/cont-init.d/add && rm -rf /srv/shiny-apps/*

RUN install2.r --error \
    baytrends \
    shinyBS \
    DT \
    ggplot2 \
    rgdal \
    sp \
    ggsn \
    classInt \
    dplyr \
    RColorBrewer \
    knitr \
    lubridate \
    rmarkdown && install2.r -r "https://cloud.R-project.org" --error aws.s3

COPY /inst/shiny-examples/baytrendsmap /srv/shiny-server/baytrendsmap

run chmod 777 -R /srv/shiny-server/baytrendsmap/map

CMD ["/init"]