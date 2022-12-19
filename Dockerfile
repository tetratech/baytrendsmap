FROM rocker/geospatial:4.2.1

RUN apt-get update && apt-get upgrade -y && rm -rf /var/lib/apt/lists/*

ENV PASSWORD=IcantB3li3v3itsN)TbutterCanYou

#RUN export ADD=shiny && bash /etc/cont-init.d/add && rm -rf /srv/shiny-apps/*
RUN /rocker_scripts/install_shiny_server.sh 


RUN install2.r --error \
    baytrends \
    shinyBS \
    shinyjs \
    shinyalert \
    DT \
    ggplot2 \
    sf \
    ggsn \
    grid \
    classInt \
    dplyr \
    RColorBrewer \
    knitr \
    lubridate \
    cowplot \
    leaflet \
    rmarkdown && install2.r -r "https://cloud.R-project.org" --error aws.s3
    
COPY /inst/shiny-examples/baytrendsmap /srv/shiny-server/baytrendsmap

run chmod 777 -R /srv/shiny-server/baytrendsmap/map

CMD ["/init"]