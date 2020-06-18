# Shiny Global File

#pkgver <- packageVersion("baytrendsmap")
pkgver <- "1.0.0.9026"

# Packages
suppressMessages(library(shiny, quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library(baytrends, quietly = TRUE, warn.conflicts = FALSE))
library(shinyBS)
suppressMessages(library(DT, quietly = TRUE, warn.conflicts = FALSE))
library(ggplot2)
suppressMessages(library(rgdal, quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library(ggsn, quietly = TRUE, warn.conflicts = FALSE))
library(classInt)
suppressMessages(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
library(RColorBrewer)
# library(leaflet)
# library(plotly)
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
options(shiny.maxRequestSize = 100*1024^2)

# Pick Lists
pick_gamDiff <- paste0("gamDiff.", c("bl.mn.obs", "cr.mn.obs", "abs.chg.obs", "pct.chg", "chg.pval"))
pick_gamDiff_Desc <- c("Baseline mean", "Current mean", "Absolute change", "Percent change (%)", "p-value")
pick_classInt <- c("quantile", "equal", "pretty") #c("sd", "quantile", "equal", "pretty")
pick_pal <- c("PuOr", "BuPu", "OrRd", "PuBu", "RdPu") #RColorBrewer
pick_ext <- c("jpg", "tiff", "png", "pdf")
pick_zoomregion <- c("none", "points", "Choptank", "James", "Patuxent", "Potomac"
                     , "Rappahannock", "Susquehanna", "York")
pick_files_radio <- c("Non-linear Trend (Full Period)"
                      , "Non-linear Trend (2009-2010 to 2017-2018)"
                      , "Non-linear Trend with Flow Adjustment (Full Period)"
                      , "Non-linear Trend with Flow Adjustment (2009-2010 to 2017-2018)")
pick_files_names <- c("Trend_NL_FA_F_Full.csv"
                      , "Trend_NL_FA_F_Short.csv"
                      , "Trend_NL_FA_T_Full.csv"
                      , "Trend_NL_FA_T_Short.csv")
# pick_mapLayer <- c("CHLA|Surface|Jan-Dec","CHLA|Surface|Jul-Sep",   "CHLA|Surface|Mar-May"  
#                    , "DO|Bottom|Jun-Sep", "DO|Surface|Jun-Sep",    "SECCHI|Surface|Apr-Oct"
#                    , "SECCHI|Surface|Jan-Dec", "SECCHI|Surface|Jul-Sep", "SECCHI|Surface|Jun-Sep"
#                    , "SECCHI|Surface|Mar-May", "TN|Bottom|Jan-Dec",    "TN|Surface|Jan-Dec"    
#                    , "TP|Bottom|Jan-Dec", "TP|Surface|Jan-Dec",    "WTEMP|Bottom|Apr-Oct"  
#                    , "WTEMP|Bottom|Jan-Dec", "WTEMP|Bottom|Jul-Sep",  "WTEMP|Bottom|Jun-Sep"  
#                    , "WTEMP|Bottom|Mar-May", "WTEMP|Surface|Apr-Oct", "WTEMP|Surface|Jan-Dec" 
#                    , "WTEMP|Surface|Jul-Sep", "WTEMP|Surface|Jun-Sep",  "WTEMP|Surface|Mar-May" )

# Map, Shapefile
fn_shp <- file.path("data", "cbseg")
ogr_shp <- rgdal::readOGR(dsn=fn_shp, layer="cbseg2003Combined2-latlong", verbose = FALSE)
fort_shp <- suppressMessages(ggplot2::fortify(ogr_shp))
#fort_shp <- load(file.path(getwd(), "data", "data_GIS_cbpseg.rda"))
#fort_shp <- load(file.path("data", "data_GIS_cbpseg.rda"))

# Map, Labels (name, x, y)
lab_Sus <- c("Susquehanna", -76.172, 39.659)
lab_Pat <- c("Patuxent", -76.700, 38.945)
lab_Cho <- c("Choptank", -75.942, 38.750)
lab_Pot <- c("Potomac", -77.250, 38.500)
lab_Rap <- c("Rappahannock", -76.950, 37.993)
lab_Yor <- c("York", -76.731, 37.551)
lab_Jam <- c("James", -77.380, 37.500)

# Bounding Box
#           EXT_MIN_X, EXT_MIN_Y, EXT_MAX_X, EXT_MAX_Y
bbox_Sus <- c(-76.1467, 39.4016, -75.9790, 39.6090)
bbox_Pat <- c(-76.8621, 38.2459, -76.4035, 38.8751)
bbox_Cho <- c(-76.3636, 38.5184, -75.7926, 39.0376)
bbox_Pot <- c(-77.7047, 37.6438, -76.2050, 39.0159)
bbox_Rap <- c(-77.4901, 37.4703, -76.2413, 38.4143)
bbox_Yor <- c(-76.9052, 37.1470, -76.3784, 37.5903)
bbox_Jam <- c(-77.5250, 36.7142, -76.2311, 37.7369)
bbox_MD <- NA
bbox_VA <- NA
bbox_points <- NA
pick_zoomregion_bbox <- c("NA", "bbox_points", "bbox_Cho", "bbox_Jam", "bbox_Pat", "bbox_Pot"
                          , "bbox_Rap", "bbox_Sus", "bbox_Yor")

# Map, base ####
map_coord_ratio <- 1.3
map_base <- ggplot() + geom_polygon(data = fort_shp
                                            , aes(long, lat, group=group, fill=hole), colour = "grey59"
                                    , fill = "lightskyblue") +
  #scale_fill_manual(values = c("lightskyblue", "grey92"), guide=FALSE) +
  theme_void() + # no grid or box for lat-long
  #theme(legend.position = "none") + # remove legend
  coord_fixed(map_coord_ratio) +
  scalebar(fort_shp, dist=25, dist_unit = "km", transform=TRUE, model = "WGS84") + 
  north(fort_shp, symbol = 3) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing (Comment out in final version)
boo_test <- FALSE
if(boo_test==TRUE){
  # data
  df_test <- read.csv(file.path(system.file(package="baytrendsmap"), "extdata", "MD17v104chnggam_DOselect.csv"))
  # 
  # Test, Range ####
  tst_rand_15 <- floor(runif(1, min=1, max=5))
  tst_rand_14 <- floor(runif(1, min=1, max=4))
  tst_gamDiff <- pick_gamDiff[1] #[tst_rand_15]
  tst_gamDiff_Desc <- pick_gamDiff_Desc[1] #[tst_rand_15]
  tst_cI_type <- pick_classInt[tst_rand_14]
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
  tst_cI_type
  tst_pal
  tst_ext
  tst_numclasses
  summary(tst_data)
  
  tst_cI_val <- classInt::classIntervals(df_test[, tst_gamDiff], tst_numclasses, tst_cI_type)
  tst_cI_val
  str(tst_cI_val)
  
  
  #col_test <- display.brewer.pal(n=tst_numclasses, name=tst_pal)
  
  # # Add coordinates so df becomes a spatial points data frame
  # sp::coordinates(df_test) <- ~longitude+latitude
  # # add project for WGS84
  # sp::proj4string(df_test) <- sp::CRS("+proj=longlat +datum=WGS84")
  # # transform to same as base layer
  # df_test <- sp::spTransform(df_test, sp::CRS(sp::proj4string(ogr_shp)))
  # fortify
  fort_df_test <- ggplot2::fortify(df_test)
  
  
  fort_df_test$map_brk_col <- cut(fort_df_test[, tst_gamDiff]
                               , breaks = tst_cI_val$brks
                               , labels = brewer.pal(tst_numclasses, tst_pal))
  
  fort_df_test$map_brk_num <- cut(fort_df_test[, tst_gamDiff]
                               , breaks = tst_cI_val$brks
                               )
  
  # Add points
  map_tst <- map_tst + geom_point(data = fort_df_test
                                  , aes_string(x = "longitude"
                                               , y = "latitude"
                                               , fill = "map_brk_col")
                                  , size = 4
                                  , pch = 21
                                  , color = "black"
                                  , na.rm=TRUE)
  # Modify Legend parts
  map_tst <- map_tst + scale_color_discrete(name=tst_gamDiff_Desc
                                 , labels = levels(fort_df_test$map_brk_num))
                                 #, labels = tst_CI)
  
  
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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test, Trend
  
  # data
  df_test <- read.csv(file.path(system.file(package="baytrendsmap"), "extdata", "MD17v104chnggam_DOselect.csv"))
  #
  df_trend <- df_test %>% filter(parmName == "Dissolved Oxygen [mg/L]") %>% 
    filter(gamName == "2-Non-linear trend with Seas+Int") %>%
    filter(layer == "S") %>%
    filter(periodName == "2008/09-Present") %>%
    filter(seasonName == "All")
  
  
  df_trend <- df_test[df_test$parmName=="Dissolved Oxygen [mg/L]", ]
  df_trend <- df_trend[df_trend$gamName == "2-Non-linear trend with Seas+Int", ]
  df_trend <- df_trend[df_trend$layer == "S", ]
  df_trend <- df_trend[df_trend$periodName == "2008/09-Present", ]
  df_trend <- df_trend[df_trend$seasonName == "All", ]
  
  
  
  #
  # Fortify
  fort_df_test <- ggplot2::fortify(df_trend)
  
  #"gamDiff.pct.chg" 
  #"gamDiff.chg.pval"
  myParam <- "gamDiff.pct.chg" 
  trend_parmName <- "DO"
  if(trend_parmName=="DO" | trend_parmName=="Sechi"){
    boo_upisgood <- TRUE
  } else {
    boo_upisgood <- FALSE
  }
  
  chg_pval_sig_2 <- 0.25 #map_trend_val_poss
  chg_pval_sig_1 <- 0.05 #map_trend_val_sig
  #
  if (boo_upisgood == TRUE){
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] > chg_pval_sig_2, "ChangeClass"] <- "NS"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_2 & fort_df_test[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posIncr"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_2 & fort_df_test[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posDecr"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_1 & fort_df_test[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigIncr"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_1 & fort_df_test[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigDecr"
  } else {
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] > chg_pval_sig_2, "ChangeClass"] <- "NS"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_2 & fort_df_test[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "posIncr"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_2 & fort_df_test[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "posDecr"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_1 & fort_df_test[, "gamDiff.pct.chg"] < 0, "ChangeClass"] <- "sigIncr"
    fort_df_test[fort_df_test[, "gamDiff.chg.pval"] < chg_pval_sig_1 & fort_df_test[, "gamDiff.pct.chg"] > 0, "ChangeClass"] <- "sigDecr"
  }##boo_upisgood~END
  
  trend_ChangeClass <- c("sigDecr", "sigIncr", "posDecr", "posIncr", "NS")
  trend_leg_label   <- c("Significant decrease", "Significant increase", "Possible decrease", "Possible increase", "Unlikely")
  trend_leg_color   <- c("orange", "dark green", "orange", "dark green", "dark gray")
  trend_leg_shape   <- c("25", "24", "21", "21", "23")
  trend_leg_size    <- c("4", "4", "3", "3", "2")
  
  manval_color <- c("sigDecr" = "orange", "sigIncr" = "dark green", "posDecr" = "orange", "posIncr" = "dark green", "NS" = "dark gray")
  manval_shape <- c("sigDecr" = 25, "sigIncr" = 24, "posDecr" = 21, "posIncr" = 21, "NS" = 23)
  manval_size  <- c("sigDecr" = 4, "sigIncr" = 4, "posDecr" = 3, "posIncr" = 3, "NS" = 2)
  
  # add to data frame
  fort_df_test[, "ChangeClass"] <- factor(fort_df_test[, "ChangeClass"], trend_ChangeClass)
  fort_df_test$ChangeClass_color <- trend_leg_color[match(fort_df_test$ChangeClass, trend_ChangeClass)]
  fort_df_test$ChangeClass_shape <- trend_leg_shape[match(fort_df_test$ChangeClass, trend_ChangeClass)]
  fort_df_test$ChangeClass_size  <- trend_leg_size[match(fort_df_test$ChangeClass, trend_ChangeClass)]
  
  # Add points
  map_t <- map_base + geom_point(data=fort_df_test
                                  , aes_string(x="longitude", y="latitude"#, group ="ChangeClass"
                                               , color="ChangeClass"
                                               , shape="ChangeClass"
                                               , size="ChangeClass"
                                               , fill="ChangeClass"
                                               )
                                  # , color = fort_df_test$ChangeClass_color 
                                  # , shape = as.numeric(fort_df_test$ChangeClass_shape)
                                  # , size = as.numeric(fort_df_test$ChangeClass_size)
                                  # , fill = fort_df_test$ChangeClass_color
                                  #, na.rm=TRUE
                                 ) +
    scale_color_manual(name = "Type of trend", labels = trend_leg_label, values = manval_color, drop = FALSE ) + 
    scale_shape_manual(name = "Type of trend", labels = trend_leg_label, values = manval_shape, drop = FALSE ) + 
    scale_fill_manual( name = "Type of trend", labels = trend_leg_label, values = manval_color, drop = FALSE ) + 
    scale_size_manual( name = "Type of trend", labels = trend_leg_label, values = manval_size,  drop = FALSE ) +
    theme(legend.position = c(1, 0.12), legend.justification = c(1, 0), legend.title = element_text(face = "bold"))
  
  # Zoom ####
  ## uses data points
  zoom_buffer <- 0.5
  if(!is.null(zoom_buffer)){
    x_min <- min(fort_df_test[, "longitude"]) - zoom_buffer
    x_max <- max(fort_df_test[, "longitude"]) + zoom_buffer
    y_min <- min(fort_df_test[, "latitude"]) - zoom_buffer
    y_max <- max(fort_df_test[, "latitude"]) + zoom_buffer
    #map_t + coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))
    
    map_t + ggplot2::coord_fixed(ratio = map_coord_ratio
                               , xlim = c(x_min, x_max)
                               , ylim = c(y_min, y_max))
    
    
  }
  # Need to solve legend, best if at bottom.
  # really only works if zooming in on a river or watershed.
  # can distort if too big an area
  # Need way to revert back - 
  
  # Zoom base
  zoom_buffer <- 0.05
  zoom_region <- bbox_Yor
  x_min <- zoom_region[1] - -zoom_buffer
  x_max <- zoom_region[3] + -zoom_buffer
  y_min <- zoom_region[2] - zoom_buffer / (map_coord_ratio * 5)
  y_max <- zoom_region[4] + zoom_buffer / (map_coord_ratio * 5)
  # map_base + coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))
  # coord_map needs a projection
  # Coord_fixed of 1.3 gives better aspect ratio
  map_base + coord_fixed(ratio=map_coord_ratio, xlim = c(x_min, x_max), ylim = c(y_min, y_max))
  # gives warning
  
  
  
  
  # Custom Legend
  map_t <- map_t + scale_color_manual(name = "Type of trend"
                                      , labels = trend_leg_label
                                      , values = trend_leg_color)
  
  
  
  #
  map_t + scale_color_manual(values = manval_color, breaks = trend_ChangeClass) +
    scale_color_manual(values = manval_shape, breaks = trend_ChangeClass) +
    scale_size_manual(values = manval_size, breaks = trend_ChangeClass)
  
  # Modify Legend parts
  map_t2 <- map_t + scale_shape_manual(values = trend_leg_shape) +
                    scale_size_manual(values = trend_leg_size) + 
                    scale_color_manual(values = trend_leg_color)
  
  
  
  
  
  map_t <- map_t + scale_color_manual(name="Type of trend"
                                      , labels = trend_leg_label
                                      , values = trend_leg_color)
  
 
  map_t + scale_shape_manual(breaks = trend_ChangeClass, values = trend_leg_shape, drop = FALSE)
  
  
  map_t + theme(legend.position = "bottom", legend.box = "horizontal", legend.title=element_text(face="bold"))

  # 
}##IF~boo_test~END














