# Prepare data for example for AZ, GIS, state outline
#
# Erik.Leppo@tetratech.com
# 20180612
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
library(rgdal)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZ_State"
shp <- readOGR(dsn=file.path(wd, "data-raw", "GIS", "state"), layer=myFile)

summary(shp)

# aea
# proj4string :
#   [+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m
#    +no_defs +ellps=GRS80 +towgs84=0,0,0]



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_GIS_AZ_Outline <- shp
devtools::use_data(data_GIS_AZ_Outline, overwrite = TRUE)


