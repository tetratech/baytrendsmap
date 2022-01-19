# Load a baytrends package output file.
#~~~~~~~~~~~~~~~~~
#
function() {
  tabPanel("Select data and map options"
           #, fluidPage(
              , tags$head(
                 tags$style(HTML("hr {border-top: 1px solid #A9A9A9;}"))
               )## tags ~ END
             , sidebarLayout(
                 sidebarPanel(width = 4
                   # App Steps
                   #h3("App Steps")
                   , h3("1. Choose Data")
                   , radioButtons("radio_input_basic", "Choose file to load", choices = pick_files_radio)
                   , bsButton("but_radio_load_basic", "Load Data")
                   # , bsPopover(id = "but_radio_load", title = "Load 'final' File"
                   #           , content = "Click button to load 'final' file based on radio button selection above.")
                   #, button to load data
                   , h3("2. Choose Map Layer (parameter|layer|season)")
                   , fluidRow(h4("Filters", uiOutput("filt_collapse_basic")))
                   , h3("3.A. Map Options, Range")
                   , bsCollapse(multiple = TRUE
                          , bsCollapsePanel("Color Brewer Palette"
                                            , style='info'
                                            , uiOutput('opt_pal_basic')
                                )##bsCollapsePanel~Color
                   )##bsCollapse 1
                   , h3("3.B. Map Options, Trends")
                   , bsCollapse(multiple = TRUE
                                , bsCollapsePanel("Direction of 'good' change (Trends Map Only)", style='info',
                                                  uiOutput('opt_upisgood_basic')
                                )##bsCollapsePanel~Direction
                   )##bsCollapse 2
                   , h3("4. Update and Save Maps")
                   , fluidRow(column(1),
                              column(1, bsButton("but_map_basic", "Update Basic Maps")),
                              bsPopover("but_map_basic", "Click 'apply' after modifying filters", "A 'data change' map will be created with the selected options.",
                                        "top", trigger = "hover", options = list(container = "body"))
                   )##fluidRow ~ update button
                  , p("Must 'update' and view map prior to save.")
                  , fluidRow(column(1),
                             column(1, downloadButton("but_map_basic_save", "Save Basic Maps (Static Range and Trends)")),
                             bsPopover("but_map_trend_save", "Click 'save' after updating map", "A 'data change' map will be saved in the selected format.",
                                       "top", trigger = "hover", options = list(container = "body"))
                  )##fluidRow ~ save button
                   
                )##sidebarPanel~END
                , mainPanel(
                    tabsetPanel(type = "tabs"
                                , tabPanel("Range Map, Interactive"
                                 , leafletOutput("map_r_leaflet_basic"
                                                 , height = "85vh")
                                )
                      , tabPanel("Range Map, Static"
                                 , plotOutput("map_r_render_basic"
                                              , height = 800
                                              ,  width=800/1.5)
                                 )
                      , tabPanel("Trends Map"
                                 , plotOutput("map_t_render_basic"
                                              , height = 800
                                              ,  width=800/1.5)
                                 )
                                
                                
                           )##tabsetPanel ~ END
                  

                )##mainPanel~END
             )##sidebarLayout~END
          # )##fluidPage~END
  )##tabPanel~END
}##FUNCTION~END