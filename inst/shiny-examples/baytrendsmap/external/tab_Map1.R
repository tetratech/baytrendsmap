function(){
  tabPanel("2. Filter Data"
           #, fluidPage(

             , sidebarLayout(
                 sidebarPanel(
                   # App Steps
                   #h3("App Steps")
                    h3("1. Load baytrends Output")
                   , h4("Select CSV input file")
                   , fileInput('fn_input', 'Choose file to upload',
                               accept = c(
                                 'text/csv'
                                 , 'text/comma-separated-values'
                                 , '.csv'
                               )
                   )##fileInput~END
                )##sidebarPanel~END
                , mainPanel(
                  tabsetPanel(type="tabs"
                              ,tabPanel("Data"
                                        , p("After import (~ 1 second / MB) the data with appear below.")
                                        , DT::dataTableOutput('df_import_DT')
                              )##tabPanel~Data~END
                              #,tabPanel("Map1")
                              #,tabPanel("Map2")
                  )##tabsetPanel~END
                )##mainPanel~END
                
                
             )##sidebarLayout~END
             
          # )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END