# Shiny Global File

# Packages
library(shiny)
library(baytrends)
library(shinyBS)
library(DT)
library(ggplot2)
library(rgdal)
library(ggsn)
library(classInt)
library(dplyr)
# library(leaflet)
# library(dplyr)
# library(dataRetrieval)
# library(data.table)
# library(stringr)
# library(plotly)
# library(shinyjs) # for testing, comment out in final version

# Drop-down boxes
#MMIs <- c("BIBI_genus", "BIBI_family", "FIBI")
#MMIs <- c("BIBI_MBSS", "BIBI_MSW", "FIBI")
#MMIs <- c("MBSS.2005.Fish", "MBSS.2005.Bugs", "MSW.1999.Bugs")
#Community <- c("fish", "bugs", "bugs")
#col_Strata <- c("FIBISTRATA", "STRATA_R", "STRATA_R")

# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 10*1024^2)

# Pick Lists
pick_gamDiff <- paste0("gamDiff.", c("bl.mn.obs", "cr.mn.obs", "abs.chg.obs", "pct.chg", "chg.pval"))
pick_gamDiff_Desc <- c("Baseline mean", "Current mean", "Absolute change", "Percent change (%)", "p-value")
pick_classInt <- c("sd", "quantile", "equal", "pretty")
pick_color <- c("PuOr", "BuPu", "OrRd PuBu", "PdPu")
pick_ext <- c("jpg", "tiff", "png", "pdf")

# Map, Shapefile
fn_shp <- file.path("data", "cbseg")
ogr_shp <- readOGR(dsn=fn_shp, layer="cbseg2003Combined2-latlong")
fort_shp <- fortify(ogr_shp)

# Map, Labels
lab_Sus <- c("Susquehanna", -76.104041, 39.578617)
lab_Pat <- c("Patuxent", -76.693943, 38.955513)
lab_Cho <- c("Choptank", -75.881705, 38.779140)
lab_Pot <- c("Potomac", -77.2856298, 38.423433)
lab_Rap <- c("Rappahannock", -76.891649, 37.993317)
lab_Yor <- c("York", -76.731066, 37.550827)
lab_Jam <- c("James", -77.366298, 37.426916)

