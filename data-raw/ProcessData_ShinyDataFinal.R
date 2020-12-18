# Prepare data for use in the package
#
# Erik.Leppo@tetratech.com
# 2020-12-18
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "final" data for 2019
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep ----
#wd <- getwd() # assume is package directory
#library(usethis)

# 1. Get data and process ----
load(file.path(".", "data-raw", "data", "data_final_2019", "baytrendsmap2019data.rda"))

# Loads 6 data sets
ls(globalenv())
# "Non-linear_Trend_1999-2000_to_2018-2019"                     
# "Non-linear_Trend_2010-2011_to_2018-2019"                     
# "Non-linear_Trend_Full_Period"                                
# "Non-linear_Trend_with_Flow_Adjustment_1999-2000_to_2018-2019"
# "Non-linear_Trend_with_Flow_Adjustment_2010-2011_to_2018-2019"
# "Non-linear_Trend_with_Flow_Adjustment_Full_Period"

# Data not documented in package

# 2. Save as RDA for use in package####
# data_GIS_cbpseg <- fort_shp
# usethis::use_data(data_GIS_cbpseg, overwrite = TRUE)
# 
# Save as CSV to shiny app
path_out <- file.path(".", "inst", "shiny-examples", "baytrendsmap", "data")
data_6 <- c(`Non-linear_Trend_1999-2000_to_2018-2019`
            , `Non-linear_Trend_2010-2011_to_2018-2019`
            , `Non-linear_Trend_Full_Period`
            , `Non-linear_Trend_with_Flow_Adjustment_1999-2000_to_2018-2019`
            , `Non-linear_Trend_with_Flow_Adjustment_2010-2011_to_2018-2019`
            , `Non-linear_Trend_with_Flow_Adjustment_Full_Period`)
fn_6  <- c("NLT_FA_F_19992000_20182019.csv"
           , "NLT_FA_F_20102011_20182019.csv"
           , "NLT_FA_F_FullPeriod.csv"
           , "NLT_FA_T_19992000_20182019.csv"
           , "NLT_FA_T_20102011_20182019.csv"
           , "NLT_FA_T_FullPeriod.csv")

# write CSV
# for (i in 1:length(data_6)){
#   write.csv(data_6[i], file = file.path(path_out, fn_6[i]))
# }## FOR ~ i ~ END

write.csv(`Non-linear_Trend_1999-2000_to_2018-2019`
          , file = file.path(path_out, fn_6[1]), row.names = FALSE)
write.csv(`Non-linear_Trend_2010-2011_to_2018-2019`
          , file = file.path(path_out, fn_6[2]), row.names = FALSE)
write.csv(`Non-linear_Trend_Full_Period`
          , file = file.path(path_out, fn_6[3]), row.names = FALSE)
write.csv(`Non-linear_Trend_with_Flow_Adjustment_1999-2000_to_2018-2019`
          , file = file.path(path_out, fn_6[4]), row.names = FALSE)
write.csv(`Non-linear_Trend_with_Flow_Adjustment_2010-2011_to_2018-2019`
          , file = file.path(path_out, fn_6[5]), row.names = FALSE)
write.csv(`Non-linear_Trend_with_Flow_Adjustment_Full_Period`
          , file = file.path(path_out, fn_6[6]), row.names = FALSE)

