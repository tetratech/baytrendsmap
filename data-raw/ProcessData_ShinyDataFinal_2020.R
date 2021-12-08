# Prepare data for use in the package
#
# Erik.Leppo@tetratech.com
# 2021-12-06
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "final" data for 2020
# Received data from Rebecca via email, 2021-10-18
# different format from 2019 (all data in CSV)
# 2019 was single RDA file with 6 datasets in it.
# Mike Lane uses gamOption
#
# NLT_FA_F <- GAM 1 and 2
# NLT_FA_T <- GAM 4 and 5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep ----
#wd <- getwd() # assume is package directory
#library(usethis)

# 1. Get data and process ----
dn_data <- file.path(".", "data-raw", "data", "data_final_2020")
fn_zip <- "baytrendsmaps_2020_chngCombo.zip"
fn_data <- "baytrendsmaps_2020_chngCombo.csv"
df_data <- read.csv(unz(file.path(dn_data, fn_zip), fn_data))

# Loads 6 data sets (2019)
#ls(globalenv())
# "Non-linear_Trend_1999-2000_to_2018-2019"                     
# "Non-linear_Trend_2010-2011_to_2018-2019"                     
# "Non-linear_Trend_Full_Period"                                
# "Non-linear_Trend_with_Flow_Adjustment_1999-2000_to_2018-2019"
# "Non-linear_Trend_with_Flow_Adjustment_2010-2011_to_2018-2019"
# "Non-linear_Trend_with_Flow_Adjustment_Full_Period"

# Munge Datasets

## Add mapLayer

# df_data$mapLayer <- paste(df_data$parmName
#                           , df_data$layer
#                           , df_data$seasonName
#                           , sep = "|")
# ## Replace long names for short
# ### have to escape special characters with \\
# df_data$mapLayer <- gsub("Chlorophyll a \\(Corrected\\) \\[ug\\/L\\]", "CHLA", df_data$mapLayer)
# df_data$mapLayer <- gsub("Dissolved Inorganic Nitrogen \\[mg\\/L\\]", "DIN", df_data$mapLayer)
# df_data$mapLayer <- gsub("Dissolved Oxygen \\[mg\\/L\\]", "DO", df_data$mapLayer)
# df_data$mapLayer <- gsub("Orthophosphorus \\(Filtered\\) \\[mg\\/L\\]", "PO4_FILT", df_data$mapLayer) # not sure
# df_data$mapLayer <- gsub("Orthophosphorus \\[mg\\/L\\]", "PO4", df_data$mapLayer)
# df_data$mapLayer <- gsub("Secchi Depth \\[m\\]", "SECCHI", df_data$mapLayer)
# df_data$mapLayer <- gsub("Total Nitrogen \\[mg\\/L\\]", "TN", df_data$mapLayer)
# df_data$mapLayer <- gsub("Total Phosphorus \\[mg\\/L\\]", "TP", df_data$mapLayer)
# df_data$mapLayer <- gsub("Total Suspended Solids \\[mg\\/L\\]", "TSS", df_data$mapLayer)
# df_data$mapLayer <- gsub("Water Temperature \\[deg C\\]", "WTEMP", df_data$mapLayer)
#
# use "dep" column
df_data$mapLayer <- paste(toupper(df_data$dep)
                          , df_data$layer
                          , df_data$seasonName
                          , sep = "|")

# trim the file to required columns
col_req <- c("station"
             , "layer"
             , "latitude"
             , "longitude"
             , "cbSeg92"
             , "state"
             , "stationGrpName"
             , "parmName"
             , "gamOption"
             , "gamName"
             , "periodName"
             , "seasonName"
             , "gamDiff.bl.mn.obs"
             , "gamDiff.cr.mn.obs"
             , "gamDiff.abs.chg.obs"
             , "gamDiff.pct.chg"
             , "gamDiff.chg.pval"
             , "mapLayer")
df_data <- df_data[, col_req]


## Filter

NLT_FA_F_19992000_20192020 <- dplyr::filter(df_data
                                            , (gamOption == 2 | gamOption == 3)
                                            & periodName == "1999/00-2019/20")
NLT_FA_F_20112012_20192020 <- dplyr::filter(df_data
                                            , (gamOption == 2 | gamOption == 3)
                                            & periodName == "2011/12-2019/20")
NLT_FA_F_FullPeriod <- dplyr::filter(df_data
                                     , (gamOption == 2 | gamOption == 3)
                                     & periodName == "Full Period")
NLT_FA_T_19992000_20192020 <- dplyr::filter(df_data
                                            , (gamOption == 4 | gamOption == 5)
                                            & periodName == "1999/00-2019/20")
NLT_FA_T_20112012_20192020 <- dplyr::filter(df_data
                                            , (gamOption == 4 | gamOption == 5)
                                            & periodName == "2011/12-2019/20")
NLT_FA_T_FullPeriod <- dplyr::filter(df_data
                                     , (gamOption == 4 | gamOption == 5)
                                     & periodName == "Full Period")




# Data not documented in package

# 2. Save as RDA for use in package####
# data_GIS_cbpseg <- fort_shp
# usethis::use_data(data_GIS_cbpseg, overwrite = TRUE)
# 
# # Save as CSV to shiny app
path_out <- file.path(".", "inst", "shiny-examples", "baytrendsmap", "data")
# data_6 <- c(`Non-linear_Trend_1999-2000_to_2019-2020`
#             , `Non-linear_Trend_2011-2012_to_2019-2020`
#             , `Non-linear_Trend_Full_Period`
#             , `Non-linear_Trend_with_Flow_Adjustment_1999-2000_to_2019-2020`
#             , `Non-linear_Trend_with_Flow_Adjustment_2011-2012_to_2019-2020`
#             , `Non-linear_Trend_with_Flow_Adjustment_Full_Period`)
# fn_6  <- c("NLT_FA_F_19992000_20192020.csv"
#            , "NLT_FA_F_20112012_20192020.csv"
#            , "NLT_FA_F_FullPeriod.csv"
#            , "NLT_FA_T_19992000_20192020.csv"
#            , "NLT_FA_T_20112012_20192020.csv"
#            , "NLT_FA_T_FullPeriod.csv")

# write CSV
# for (i in 1:length(data_6)){
#   write.csv(data_6[i], file = file.path(path_out, fn_6[i]))
# }## FOR ~ i ~ END

write.csv(NLT_FA_F_19992000_20192020
          , file = file.path(path_out, "NLT_FA_F_19992000_20192020.csv"), row.names = FALSE)
write.csv(NLT_FA_F_20112012_20192020
          , file = file.path(path_out, "NLT_FA_F_20112012_20192020.csv"), row.names = FALSE)
write.csv(NLT_FA_F_FullPeriod
          , file = file.path(path_out, "NLT_FA_F_FullPeriod_20192020.csv"), row.names = FALSE)
write.csv(NLT_FA_T_19992000_20192020
          , file = file.path(path_out, "NLT_FA_T_19992000_20192020.csv"), row.names = FALSE)
write.csv(NLT_FA_T_20112012_20192020
          , file = file.path(path_out, "NLT_FA_T_20112012_20192020.csv"), row.names = FALSE)
write.csv(NLT_FA_T_FullPeriod
          , file = file.path(path_out, "NLT_FA_T_FullPeriod_20192020.csv"), row.names = FALSE)

