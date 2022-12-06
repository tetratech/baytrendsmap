# Shiny Global File

# Packages ----
suppressMessages(library(shiny, quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library(baytrends, quietly = TRUE, warn.conflicts = FALSE))
library(shinyBS)
suppressMessages(library(DT, quietly = TRUE, warn.conflicts = FALSE))
suppressMessages(library(ggplot2, quietly = TRUE, warn.conflicts = FALSE))
#suppressMessages(library(rgdal, quietly = TRUE, warn.conflicts = FALSE))
library(sf) # replace rgdal
suppressMessages(library(ggsn, quietly = TRUE, warn.conflicts = FALSE))
library(classInt)
suppressMessages(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
library(RColorBrewer)
library(leaflet)
library(shinyjs) # turn on/off buttons
library(shinyalert) # popup modal at start up
## Packages, No longer used
# library(plotly)
# library(dataRetrieval)
# library(data.table)
# library(stringr)
library(cowplot)

# Package Version ----
#pkgver <- utils::packageVersion("baytrendsmap") # does not work on Shinyapps.io
#pkgver <- installed.packages()["baytrendsmap", "Version"]
# believe ShinyApps.io blocks some system level commands
pkgver <- "1.2.3.9001 (test 2021 data)"

# File Size ----
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 100*1024^2)

# Other
## DT Col Width
col_width_manual <- "200px"

# Data, Repository----
url_remote_base_github <- "https://raw.githubusercontent.com/tetratech/baytrends_files/main/test/"
url_remote_base_cbp <- "https://dx3ga8blp094q.cloudfront.net/"
url_remote_base <- url_remote_base_github

# Pick Lists----
pick_gamDiff <- paste0("gamDiff.", c("bl.mn.obs", "cr.mn.obs", "abs.chg.obs", "pct.chg", "chg.pval"))
pick_gamDiff_Desc <- c("Baseline mean", "Current mean", "Absolute change", "Percent change (%)", "p-value")
pick_classInt <- c("quantile", "equal", "pretty") #c("sd", "quantile", "equal", "pretty")
pick_pal <- c("PuOr", "BuPu", "OrRd", "PuBu", "RdPu") #RColorBrewer
pick_pal_names <- c("Purple_Orange", "Blue_Purple", "Orange_Red", "Purple_Blue", "Red_Purple")
pick_pal_change <- c("Red_Blue", "Purple_Green", "Orange_Green") # custom
pal_change_OrGn <- c("orange", "green")
# Color blind friendly
# https://colorbrewer2.org/#type=diverging&scheme=BrBG&n=5
pal_change_RdBu <- c("#ef8a62", "#67a9cf")  # red_blue (more orange than red)
pal_change_PuGn <- c("#af8dc3", "#7fbf7b") # purple_green
pick_ext <- c("jpg", "tiff", "png", "pdf")
pick_zoomregion <- c("none", "points", "Choptank", "James", "Patuxent", "Potomac"
                     , "Rappahannock", "Susquehanna", "York", "DC") #bbox too
# pick_files_radio <- c("Non-linear Trend (Full Period)"
#                       , "Non-linear Trend (1999-2000 to 2018-2019)"
#                       , "Non-linear Trend (2010-2011 to 2018-2019)"
#                       , "Non-linear Trend with Flow Adjustment (Full Period)"
#                       , "Non-linear Trend with Flow Adjustment (1999-2000 to 2018-2019)"
#                       , "Non-linear Trend with Flow Adjustment (2010-2011 to 2018-2019)"
#                       )
# pick_files_names <- c("NLT_FA_F_FullPeriod.csv"
#                       , "NLT_FA_F_19992000_20182019.csv"
#                       , "NLT_FA_F_20102011_20182019.csv"
#                       , "NLT_FA_T_FullPeriod.csv"
#                       , "NLT_FA_T_19992000_20182019.csv"
#                       , "NLT_FA_T_20102011_20182019.csv"
#                       )
## Pick Lists, Files (data driven)----
url_data_pick_files <- paste0(url_remote_base, "data/")
df_pick_files <- read.csv(paste0(url_data_pick_files, "pick_files.csv"))
pick_files_radio <- df_pick_files[, "radio"]
pick_files_names <- paste0(url_data_pick_files, df_pick_files[, "names"])
# Full Period should always be 1st and 4th (flow adjusted) elements
#pick_files_radio_plots <- pick_files_radio[1] #c(1,4)
#pick_files_radio_plots_dir <- df_pick_files[, "dir_plot"]

# pick_mapLayer <- c("CHLA|Surface|Jan-Dec"
#                    , "CHLA|Surface|Jul-Sep"
#                    , "CHLA|Surface|Mar-May"
#                    , "DO|Bottom|Jun-Sep"
#                    , "DO|Surface|Jun-Sep"
#                    , "SECCHI|Surface|Apr-Oct"
#                    , "SECCHI|Surface|Jan-Dec"
#                    , "SECCHI|Surface|Jul-Sep"
#                    , "SECCHI|Surface|Jun-Sep"
#                    , "SECCHI|Surface|Mar-May"
#                    , "TN|Bottom|Jan-Dec"
#                    , "TN|Surface|Jan-Dec"
#                    , "TP|Bottom|Jan-Dec"
#                    , "TP|Surface|Jan-Dec"
#                    , "WTEMP|Bottom|Apr-Oct"
#                    , "WTEMP|Bottom|Jan-Dec"
#                    , "WTEMP|Bottom|Jul-Sep"
#                    , "WTEMP|Bottom|Jun-Sep"
#                    , "WTEMP|Bottom|Mar-May"
#                    , "WTEMP|Surface|Apr-Oct"
#                    , "WTEMP|Surface|Jan-Dec"
#                    , "WTEMP|Surface|Jul-Sep"
#                    , "WTEMP|Surface|Jun-Sep"
#                    , "WTEMP|Surface|Mar-May" )

# Background ----
## Copy background file to www folder
# bg_fn <- "TidalWaterQualityChange.pdf"
# bg_url <- paste0("https://github.com/tetratech/baytrends_files/raw/main/", bg_fn)
# file.copy(bg_url, paste0("www/", bg_fn), overwrite = TRUE)
# abandoned as can read direct from remote GitHub repo

# Map ----
## Map, Shapefile----
fn_shp <- file.path("data", "cbseg")
ogr_shp <- sf::st_read(dsn=fn_shp, layer="cbseg2003Combined2-latlong"
                       , quiet = TRUE) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
fort_shp <- suppressMessages(ggplot2::fortify(ogr_shp))
#fort_shp <- load(file.path(getwd(), "data", "data_GIS_cbpseg.rda"))
#fort_shp <- load(file.path("data", "data_GIS_cbpseg.rda"))

## Map, Labels (name, x, y)----
lab_Sus <- c("Susquehanna", -76.172, 39.659)
lab_Pat <- c("Patuxent", -76.700, 38.945)
lab_Cho <- c("Choptank", -75.942, 38.750)
lab_Pot <- c("Potomac", -77.250, 38.500)
lab_Rap <- c("Rappahannock", -76.950, 37.993)
lab_Yor <- c("York", -76.731, 37.551)
lab_Jam <- c("James", -77.380, 37.500)

## Map, Bounding Box----
#           EXT_MIN_X, EXT_MIN_Y, EXT_MAX_X, EXT_MAX_Y
bbox_Sus <- c(-76.1467, 39.4016, -75.9790, 39.6090)
bbox_Pat <- c(-76.8621, 38.2459, -76.4035, 38.8751)
bbox_Cho <- c(-76.3636, 38.5184, -75.7926, 39.0376)
bbox_Pot <- c(-77.7047, 37.6438, -76.2050, 39.0159)
bbox_Rap <- c(-77.4901, 37.4703, -76.2413, 38.4143)
bbox_Yor <- c(-76.9052, 37.1470, -76.3784, 37.5903)
bbox_Jam <- c(-77.5250, 36.7142, -76.2311, 37.7369)
bbox_DC  <- c(-(77 + 8/60 + 30/3600), (38 + 46/60 + 30/3600)
              , -(76 + 54/60 + 0/3600), (38 + 56/60 + 30/3600))
# bbox_inset_DC <- c(-(76+56/60), (38+58/60+30/3600)
#                    , -(76+8/60), (39+8/60))
bbox_MD <- NA
bbox_VA <- NA
bbox_points <- NA
pick_zoomregion_bbox <- c("NA", "bbox_points", "bbox_Cho", "bbox_Jam", "bbox_Pat", "bbox_Pot"
                          , "bbox_Rap", "bbox_Sus", "bbox_Yor", "bbox_DC")
# pick_zoomregion at start

## Map, base ####
map_coord_ratio <- 1.3
# map_base <- ggplot() + geom_polygon(data = fort_shp
#                                             , aes(long
#                                                   , lat
#                                                   , group=group
#                                                   , fill=hole)
#                                     , colour = "grey59"
#                                     , fill = "lightskyblue") +
#   #scale_fill_manual(values = c("lightskyblue", "grey92"), guide=FALSE) +
#   theme_void() + # no grid or box for lat-long
#   #theme(legend.position = "none") + # remove legend
#   coord_fixed(map_coord_ratio) +
#   scalebar(fort_shp, dist=25, dist_unit = "km", transform=TRUE, model = "WGS84") + 
#   north(fort_shp, symbol = 3) 

map_base <- ggplot() +
  geom_sf(data = ogr_shp, color = "grey59", fill = "lightskyblue") + 
  theme_void() + # no grid or box for lat-long
  #coord_fixed(map_coord_ratio) +
  scalebar(fort_shp, dist=25, dist_unit = "km", transform=TRUE, model = "WGS84") + 
  north(fort_shp, symbol = 3) 

## Map, Inset (DC) ----
map_inset <- map_base + 
  ggplot2::coord_sf(expand = FALSE
                    , xlim = c(bbox_DC[1], bbox_DC[3])
                    , ylim = c(bbox_DC[2], bbox_DC[4])) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2))

## Map, Inset, Location ----
inset_draw_x <- 0.15
inset_draw_y <- 0.35 
inset_draw_width <- 0.25

inset_draw_x_adv <- inset_draw_x
inset_draw_y_adv <- inset_draw_y # 0.35
inset_draw_width_adv <- inset_draw_width

## Map, output size ----
plot_h <- 9
plot_w <- plot_h/map_coord_ratio
plot_units <- "in"
plot_scale <- 1.25

## Map, DEFAULT ----
### use default data to generate a default map

### Map, DEFAULT, Data ----
## default data import for default map
df_data_default <- read.csv(pick_files_names[1])
# filter
default_mapLayer <- sort(unique(df_data_default$mapLayer))[1]
df_mrd <- df_data_default[df_data_default$mapLayer == default_mapLayer, ]

### Map, DEFAULT, map ----
m_r_d <- map_base

mrd_cI_type  <- "pretty"
mrd_pal <- "PuOr"
mrd_var_name <- "Baseline mean"
mrd_var <- pick_gamDiff[match(mrd_var_name, pick_gamDiff_Desc)]
# mr_var <- input$SI_variable
# mr_var_name <- pick_gamDiff_Desc[match(mr_var, pick_gamDiff)]
mrd_brks_user <- NULL
#evals to NULL if left blank

# breaks vs numclasses
# no breaks, use slider for num classes
mrd_numclass <- 5 #input$numclass
# derive breaks from user n and style
mrd_cI_val <- classInt::classIntervals(df_mrd[, mrd_var]
                                      , mrd_numclass
                                      , mrd_cI_type)
# Redo num classes as "pretty" picks its own number of breaks
mrd_numclass <- ifelse(mrd_cI_type=="pretty"
                      , length(mrd_cI_val$brks) - 1
                      , mrd_numclass)
#mr_numclass <- ifelse(mr_cI_type=="pretty"
# , length(mr_cI_val$brks)
# , mr_numclass)
# breaks
mrd_brks <- mrd_cI_val$brks

mrd_pal_col <- RColorBrewer::brewer.pal(n=mrd_numclass, name=mrd_pal)

# River Names
boo_riverNames <- "Yes"

if(boo_riverNames == "Yes"){
  m_r_d <- m_r_d + 
    annotate(geom = "text"
             , x = as.numeric(lab_Sus[2])
             , y=as.numeric(lab_Sus[3])
             , label=lab_Sus[1]) +
    annotate(geom = "text", x = as.numeric(lab_Pat[2])
             , y=as.numeric(lab_Pat[3])
             , label=lab_Pat[1]) +
    annotate(geom = "text", x = as.numeric(lab_Cho[2])
             , y=as.numeric(lab_Cho[3])
             , label=lab_Cho[1], hjust=0) +
    annotate(geom = "text", x = as.numeric(lab_Pot[2])
             , y=as.numeric(lab_Pot[3])
             , label=lab_Pot[1], hjust=0) +
    annotate(geom = "text", x = as.numeric(lab_Rap[2])
             , y=as.numeric(lab_Rap[3])
             , label=lab_Rap[1], hjust=1) +
    annotate(geom = "text", x = as.numeric(lab_Yor[2])
             , y=as.numeric(lab_Yor[3])
             , label=lab_Yor[1], hjust=0) +
    annotate(geom = "text", x = as.numeric(lab_Jam[2])
             , y=as.numeric(lab_Jam[3])
             , label=lab_Jam[1], hjust=0)
}##IF~riverNames~END


# title
mrd_sep1 <- ": "
mrd_sep2 <- "\n" #"; "
mrd_title_parmName   <- sort(unique(df_mrd[, "parmName"]))
mrd_title_gamName    <- sort(unique(df_mrd[, "gamName"]))
mrd_title_periodName <- sort(unique(df_mrd[, "periodName"]))

mrd_title_layer      <- sort(unique(df_mrd[, "layer"]))
mrd_title_seasonName <- sort(unique(df_mrd[, "seasonName"]))
mrd_str_title <- paste(paste(mrd_title_parmName, collapse = ", ")
                   , paste("GAM", paste(mrd_title_gamName
                                        , collapse = ", ")
                           , sep = mrd_sep1)
                   , paste("Layer", paste(mrd_title_layer
                                          , collapse = ", ")
                           , sep = mrd_sep1)
                   , paste("Period", paste(mrd_title_periodName
                                           , collapse = ", ")
                           , sep = mrd_sep1)
                   , paste("Season", paste(mrd_title_seasonName
                                           , collapse = ", ")
                           , sep = mrd_sep1)
                   , sep = mrd_sep2)
mrd_title <- mrd_str_title

if(!is.null(mrd_title)){
  m_r_d <- m_r_d +
    labs(title=paste(mrd_title, collapse="; "))
  # labs(title=paste(mr_pal_col, collapse="; "))
}##IF~riverNames~END

# Points
# fortify
fort_df_mrd <- ggplot2::fortify(df_mrd)
# Add to data frame

## Break, Color
fort_df_mrd$map_brk_col <- cut(fort_df_mrd[, mrd_var]
                              , breaks = mrd_brks
                              #, labels = brewer.pal(max(3, mr_numclass)
                              #, mr_pal)[1:mr_numclass]
                              , labels = mrd_pal_col
                              , include.lowest = TRUE
)
# Minimum of 3 different levels or get warning
## Break, Text
fort_df_mrd$map_brk_num <- cut(fort_df_mrd[, mrd_var]
                              , breaks = mrd_brks
                              , include.lowest = TRUE
)

# Points, Add
m_r_d <- m_r_d + geom_point(data=fort_df_mrd
                        , aes_string(x =" longitude"
                                     , y = "latitude"
                                     #  , fill = "map_brk_num")
                                     , fill = "map_brk_col")
                        , size = 4
                        , pch = 21
                        , color = "black"
                        , na.rm = TRUE)



m_r_d <- m_r_d + scale_fill_brewer(palette = mrd_pal
                               , name = mrd_var_name
                               , labels = levels(fort_df_mrd$map_brk_num)
                               , drop = FALSE
)



# Legend 
m_r_d <- m_r_d + theme(legend.position = "bottom"
                   , legend.box = "horizontal"
                   , legend.title = element_text(face = "bold"))





