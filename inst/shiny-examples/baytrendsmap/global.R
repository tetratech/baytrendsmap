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


pick_gamDiff <- paste0("gamDiff.", c("bl.mn.obs", "cr.mn.obs", "abs.chg.obs", "pct.chg", "chg.pval"))
pick_gamDiff_Desc <- c("Baseline mean", "Current mean", "Absolute change", "Percent change (%)", "p-value")
pick_classInt <- c("sd", "quantile", "equal", "pretty")
pick_color <- c("PuOr", "BuPu", "OrRd PuBu", "PdPu")
pick_ext <- c("jpg", "tiff", "png", "pdf")
