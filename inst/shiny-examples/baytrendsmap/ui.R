#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinyBS)
# library(leaflet)
# library(dplyr)
# library(dataRetrieval)
# library(data.table)
# library(ggplot2)
# library(stringr)

# Load files for individual screens
tab_Data <- source("external/tab_Data.R", local=T)$value
tab_Map1 <- source("external/tab_Map1.R", local=TRUE)$value
# tab_Map2 <- source("external/tab_Map2.R", local=TRUE)$value
tab_Help <- source("external/tab_Help.R", local=TRUE)$value 

# Define UI for application that draws a histogram
shinyUI(#fluidPage(
  
  # Application title
  #titlePanel("baytrends output mapping tool")
  
  navbarPage("baytrends output mapping tool"
             , theme = "boostrap.css"
             , inverse= TRUE
             , tab_Data()
              , tab_Map1()
             # , tab_Map2()
             , tab_Help()
)
  
  # Sidebar with a slider input for number of bins 
  # , sidebarLayout(
  #   sidebarPanel(
  #     # App Steps
  #     h3("App Steps")
  #     , h3("1. Load baytrends Output")
  #     , h4("Select CSV input file")
  #     , fileInput('fn_input', 'Choose file to upload',
  #                 accept = c(
  #                   'text/csv'
  #                   , 'text/comma-separated-values'
  #                   , '.csv'
  #                 )
  #     )##fileInput~END
  #     
  #   )##sidebarPanel~END
  #   #   , sliderInput("bins",
  #   #                "Number of bins:",
  #   #                min = 1,
  #   #                max = 50,
  #   #                value = 30)
  #   # ),
  #   
  #   # Show a plot of the generated distribution
  #   , mainPanel(
  #       tabsetPanel(type="tabs"
  #                   ,tabPanel("Data"
  #                             , DT::dataTableOutput('df_import_DT'))##tabPanel~Data~END
  #                   ,tabPanel("Map1")
  #                   ,tabPanel("Map2")
  #       )##tabsetPanel~END
  #      
  #   )##mainPanel~END
  # )##sidebarLayout~END
#)
)##shinyUI~END
