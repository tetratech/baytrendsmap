# Load a baytrends package output file.
#~~~~~~~~~~~~~~~~~
#
function(){
  tabPanel("1. Select Data"
           #, fluidPage(
              , tags$head(
                 tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}"))
               )## tags ~ END
             , sidebarLayout(
                 sidebarPanel(width=3, 
                   # App Steps
                   #h3("App Steps")
                   h2("Choose baytrends Output")
                   , h3("1a. Select official file")
                   , radioButtons("radio_input", "Choose file to load", choices = pick_files_radio)
                   , bsButton("but_radio_load", "Load 'Official' File")
                   # , bsPopover(id = "but_radio_load", title = "Load 'Official' File"
                   #           , content = "Click button to load 'official' file based on radio button selection above.")
                   #, button to load data
                   , h3("1b. Load user file")
                   , h4("Select CSV input file")
                   , fileInput('fn_input', 'Choose file to upload',
                               accept = c(
                                 'text/csv'
                                 , 'text/comma-separated-values'
                                 , '.csv'
                               )
                   )##fileInput~END
                   , hr()
                   , p("Use the button below to clear a 'user' file to select an 'official' file.")
                   , bsButton("but_ClearInput", label="Reset File Selections", style="primary")
                )##sidebarPanel~END
                , mainPanel(
                  tabsetPanel(type="tabs"
                              ,tabPanel("Data"
                                        , p("After upload (~ 3 second / MB) the data will appear below.")
                                        , DT::dataTableOutput('df_import_DT')
                              )##tabPanel~Data~END
                  )##tabsetPanel~END
                )##mainPanel~END
                
                
             )##sidebarLayout~END
             
          # )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END
