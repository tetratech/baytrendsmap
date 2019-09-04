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
library(RColorBrewer)
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
pick_pal <- c("PuOr", "BuPu", "OrRd", "PuBu", "RdPu") #RColorBrewer
pick_ext <- c("jpg", "tiff", "png", "pdf")

# Map, Shapefile
# fn_shp <- file.path("data", "cbseg")
# ogr_shp <- rgdal::readOGR(dsn=fn_shp, layer="cbseg2003Combined2-latlong")
# fort_shp <- ggplot2::fortify(ogr_shp)
#fort_shp <- load(file.path(getwd(), "data", "data_GIS_cbpseg.rda"))
fort_shp <- data_GIS_cbpseg

# Map, Labels
lab_Sus <- c("Susquehanna", -76.172, 39.659)
lab_Pat <- c("Patuxent", -76.700, 38.945)
lab_Cho <- c("Choptank", -75.942, 38.750)
lab_Pot <- c("Potomac", -77.250, 38.500)
lab_Rap <- c("Rappahannock", -76.950, 37.993)
lab_Yor <- c("York", -76.731, 37.551)
lab_Jam <- c("James", -77.380, 37.500)

# Map, base
map_base <- ggplot() + geom_polygon(data = fort_shp
                                            , aes(long, lat, group=group, fill=hole), colour = "grey59") +
  scale_fill_manual(values = c("lightskyblue", "grey92"), guide=FALSE) +
  theme_void() + # no grid or box for lat-long
  #theme(legend.position = "none") + # remove legend
  scalebar(fort_shp, dist=25, dist_unit = "km", transform=TRUE, model = "WGS84") + 
  north(fort_shp, symbol = 3) #+ 
  # annotate(geom = "text", x = as.numeric(lab_Sus[2]), y=as.numeric(lab_Sus[3]), label=lab_Sus[1]) +
  # annotate(geom = "text", x = as.numeric(lab_Pat[2]), y=as.numeric(lab_Pat[3]), label=lab_Pat[1]) +
  # annotate(geom = "text", x = as.numeric(lab_Cho[2]), y=as.numeric(lab_Cho[3]), label=lab_Cho[1], hjust=0) +
  # annotate(geom = "text", x = as.numeric(lab_Pot[2]), y=as.numeric(lab_Pot[3]), label=lab_Pot[1], hjust=0) +
  # annotate(geom = "text", x = as.numeric(lab_Rap[2]), y=as.numeric(lab_Rap[3]), label=lab_Rap[1]) +
  # annotate(geom = "text", x = as.numeric(lab_Yor[2]), y=as.numeric(lab_Yor[3]), label=lab_Yor[1], hjust=0) +
  # annotate(geom = "text", x = as.numeric(lab_Jam[2]), y=as.numeric(lab_Jam[3]), label=lab_Jam[1], hjust=0)

# Testing (Comment out in final version)
boo_test <- FALSE
if(boo_test==TRUE){
  # data
  df_test <- read.csv(file.path(system.file(package="baytrendsmap"), "extdata", "MD17v104chnggam_DOselect.csv"))
  # 
  tst_rand_15 <- floor(runif(1, min=1, max=5))
  tst_rand_14 <- floor(runif(1, min=1, max=4))
  tst_gamDiff <- pick_gamDiff[1] #[tst_rand_15]
  tst_gamDiff_Desc <- pick_gamDiff_Desc[1] #[tst_rand_15]
  tst_classInt <- pick_classInt[tst_rand_14]
  tst_pal <- pick_pal[tst_rand_14]
  tst_ext <- pick_ext[tst_rand_14]
  tst_numclasses <- floor(runif(1, min=3, max=8))
  
  map_tst <- map_base
  
  # River Names
  map_tst <- map_tst + annotate(geom = "text", x = as.numeric(lab_Sus[2]), y=as.numeric(lab_Sus[3]), label=lab_Sus[1]) +
    annotate(geom = "text", x = as.numeric(lab_Pat[2]), y=as.numeric(lab_Pat[3]), label=lab_Pat[1]) +
    annotate(geom = "text", x = as.numeric(lab_Cho[2]), y=as.numeric(lab_Cho[3]), label=lab_Cho[1]) +
    annotate(geom = "text", x = as.numeric(lab_Pot[2]), y=as.numeric(lab_Pot[3]), label=lab_Pot[1]) +
    annotate(geom = "text", x = as.numeric(lab_Rap[2]), y=as.numeric(lab_Rap[3]), label=lab_Rap[1]) +
    annotate(geom = "text", x = as.numeric(lab_Yor[2]), y=as.numeric(lab_Yor[3]), label=lab_Yor[1]) +
    annotate(geom = "text", x = as.numeric(lab_Jam[2]), y=as.numeric(lab_Jam[3]), label=lab_Jam[1])
  
  # Map Title
  str_title <- paste(tst_gamDiff_Desc, format(Sys.time(), "%Y%m%d_%H%M%S"), sep = "; ")
  map_tst <- map_tst + labs(title=str_title)
  
  # 
  tst_data <- df_test[, tst_gamDiff]
  
  tst_gamDiff
  tst_gamDiff_Desc
  tst_classInt
  tst_pal
  tst_ext
  tst_numclasses
  summary(tst_data)
  
  tst_cI <- classInt::classIntervals(tst_data, tst_numclasses, tst_classInt)
  tst_cI
  str(tst_cI)
  
  
  col_test <- display.brewer.pal(n=tst_numclasses, name=tst_pal)
  
  # # Add coordinates so df becomes a spatial points data frame
  # sp::coordinates(df_test) <- ~longitude+latitude
  # # add project for WGS84
  # sp::proj4string(df_test) <- sp::CRS("+proj=longlat +datum=WGS84")
  # # transform to same as base layer
  # df_test <- sp::spTransform(df_test, sp::CRS(sp::proj4string(ogr_shp)))
  # fortify
  fort_df_test <- ggplot2::fortify(df_test)
  
  
  fort_df_test$mapColor <- cut(fort_df_test[, tst_gamDiff]
                               , breaks = tst_cI$brks
                               , labels =brewer.pal(tst_numclasses, tst_pal))
  
  # Add points
  map_tst <- map_tst + geom_point(data=fort_df_test
                                  , aes_string(x="longitude", y="latitude", color="mapColor")
                                  , size = 4
                                  , na.rm=TRUE)
  # Modify Legend parts
  map_tst <- map_tst + scale_color_discrete(name=tst_gamDiff_Desc
                                 , labels = paste(c(">", rep("< ", length(tst_cI$brks)-1)), round(tst_cI$brks, 2)))
  
  
  map_tst + theme(legend.position = "bottom", legend.box = "horizontal", legend.title=element_text(face="bold"))
  
  
  
  # map_tst + guides(colors=col_test)
  # 
  # 
  # map_tst + scale_color_manual(aes=c("mapColor")
  #                                 , breaks = tst_cI$brks
  #                                 , name = tst_gamDiff_Desc
  #                                 , values=brewer.pal(tst_numclasses, tst_pal)
  #                                 , labels = 1:length(tst_cI$brks)
  #                                 )
  # 
  # 

  # 
}##IF~boo_test~END














