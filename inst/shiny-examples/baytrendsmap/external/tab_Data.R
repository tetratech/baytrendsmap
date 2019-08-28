# Load a baytrends package output file.
#~~~~~~~~~~~~~~~~~
#
function(){
  tabPanel("1. Import Data"
           #, fluidPage(

             , sidebarLayout(
                 sidebarPanel(width=3, 
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
                                        , p("After upload (~ 3 second / MB) the data with appear below.")
                                        , DT::dataTableOutput('df_import_DT')
                              )##tabPanel~Data~END
                  )##tabsetPanel~END
                )##mainPanel~END
                
                
             )##sidebarLayout~END
             
          # )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END