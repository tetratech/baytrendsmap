#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages ####
## referenced global.R

# Source Pages ####
# Load files for individual screens
tab_Data       <- source("external/tab_Data.R", local=T)$value
tab_Filter     <- source("external/tab_Filter.R", local=TRUE)$value
tab_Plot_Range <- source("external/tab_Plot_Range.R", local=TRUE)$value
tab_Plot_Trend <- source("external/tab_Plot_Trend.R", local=TRUE)$value
tab_Help       <- source("external/tab_Help.R", local=TRUE)$value 

# UI ####
# Define UI for application that draws a histogram
shinyUI(#fluidPage(
  
  # Application title
  #titlePanel("baytrends output mapping tool")
  
  navbarPage(paste0("baytrends output mapping tool v0.0.1.9013") #, packageVersion("baytrendsmap"))
             , theme = "boostrap.css"
             , inverse= TRUE
             , tab_Data()
             , tab_Filter()
             #, tab_Plot_Range()
             #, tab_Plot_Trend()
             , tab_Help()
  )##navbarPage~END
  

)##shinyUI~END
