# Prepare data for use in the package
#
# Erik.Leppo@tetratech.com
# 2019-09-04
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save GIS shapefile as RDA
# saves space and should load quicker
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
library(rgdal)
library(ggplot2)

# 1. Get data and process #####
fn_shp <- file.path("data-raw", "data", "cbseg")
ogr_shp <- rgdal::readOGR(dsn=fn_shp, layer="cbseg2003Combined2-latlong")
fort_shp <- ggplot2::fortify(ogr_shp)
# 4.23 MB

# 2. Save as RDA for use in package####
data_GIS_cbpseg <- fort_shp
usethis::use_data(data_GIS_cbpseg, overwrite = TRUE)
# 3.61 MB

