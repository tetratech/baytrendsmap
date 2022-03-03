# Load a baytrends package output file.
#~~~~~~~~~~~~~~~~~
#
function() {
  tabPanel("Create Custom Maps"
           , navbarPage("Advanced Functions"
                      , theme = "boostrap.css"
                      , inverse = FALSE
                      # , tab_Plot_Example()
                      , tab_Data()
                      , tab_Filter()

           )##navbarPage~END
          #  #, fluidPage(
          #     , tags$head(
          #        tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}"))
          #      )## tags ~ END
          #    , sidebarLayout(
          #        sidebarPanel(width = 4,
          #          # App Steps
          #          #h3("App Steps")
          #          h2("Choose baytrends Output")
          #          , h3("1a. Load final file")
          #          , radioButtons("radio_input", "Choose file to load", choices = pick_files_radio)
          #          , bsButton("but_radio_load", "Load 'Final' File")
          #          # , bsPopover(id = "but_radio_load", title = "Load 'final' File"
          #          #           , content = "Click button to load 'final' file based on radio button selection above.")
          #          #, button to load data
          #          , h3("1b. Load user file")
          #          , h4("Select CSV input file")
          #          , fileInput('fn_input', 'Choose file to upload \n(maximum file size 100 MB)',
          #                      accept = c(
          #                        'text/csv'
          #                        , 'text/comma-separated-values'
          #                        , '.csv'
          #                      )
          #          )##fileInput~END
          #          #, p("Maximum file size is 100 MB.")
          #          , hr()
          #          , textOutput("txt_click_filetype")
          #       )##sidebarPanel~END
          #       , mainPanel(
          #         tabsetPanel(type="tabs"
          #                     ,tabPanel("Data"
          #                               , p("After upload (~ 3 second / MB) the data will appear below.")
          #                               , DT::dataTableOutput('df_import_DT')
          #                     )##tabPanel~Data~END
          #         )##tabsetPanel~END
          #       )##mainPanel~END
          #    )##sidebarLayout~END
          # # )##fluidPage~END
  )##tabPanel~END
}##FUNCTION~END