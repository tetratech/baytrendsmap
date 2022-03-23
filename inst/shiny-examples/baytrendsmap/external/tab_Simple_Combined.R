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
                   , radioButtons("radio_input_basic"
                                  , "Choose file to load"
                                  , choices = pick_files_radio)
                   #, bsButton("but_radio_load_basic", "Load Data")
                   # , bsPopover(id = "but_radio_load"
                   #           , title = "Load 'final' File"
                   #           , content = "Click button to load 'final' file based on radio button selection above.")
                   #, button to load data
                   , h3("2. Choose Map Layer (parameter|layer|season)")
                   , fluidRow(h4("Filters", uiOutput("filt_collapse_mapLayer_basic")))
                   , h3("3. Map Options")
                   , h4("3.a. Range Map Options")
                   , bsCollapse(multiple = TRUE
                          , bsCollapsePanel("Color Palette (Range Map Only)"
                                            , style='info'
                                            , uiOutput('opt_pal_range_basic')
                                )##bsCollapsePanel~Color
                          , open = "Color Palette (Range Map Only)"
                   )##bsCollapse 1
        #           , fluidRow(h4("range2", uiOutput("filt_collapse_pal_range_basic")))
                   , h4("3.b. Change Map Options")
                   , bsCollapse(multiple = TRUE
                                , bsCollapsePanel("Color Palette (Change Map Only)"
                                                  , style = 'info'
                                                  , uiOutput('opt_pal_trend_basic')
                                )##bsCollapsePanel
                                , open = "Color Palette (Change Map Only)" # to auto open panels
                    )##bsCollapse ~ Pal Trend
                   
                   , bsCollapse(multiple = TRUE
                                , bsCollapsePanel("Direction of 'Good' Change (Change Map Only)"
                                                  , style='info'
                                                  , uiOutput('opt_upisgood_basic')
                                )##bsCollapsePanel~Direction
                                , open = "Direction of 'Good' Change (Change Map Only)" # to auto open panels
                   )##bsCollapse 2
                   , h3("4. Save Maps")
                   # , fluidRow(column(1)
                   #            , column(1, bsButton("but_map_basic", "Update Basic Maps"))
                   #            , bsPopover("but_map_basic"
                   #                      , "Click 'apply' after modifying filters"
                   #                      , "A 'data change' map will be created with the selected options."
                   #                      , "top"
                   #                      , trigger = "hover"
                   #                      , options = list(container = "body"))
                   # )##fluidRow ~ update button
                  # , p("Must 'update' and view map prior to save.")
                  # , p("Save will return a zip file with the Range and Change maps but not the interactive map.")
                  #, useShinyjs()
                  # , fluidRow(column(1)
                  #            , column(1
                  #                   # , shinyjs::disabled(downloadButton("but_map_basic_save"
                  #                   #                                    , "Save Basic Maps"))
                  #                   , downloadButton("but_map_basic_save"
                  #                                                      , "Save Basic Maps")
                  #            )
                  #            , bsPopover("but_map_trend_save"
                  #                      , "Click 'save' after updating map"
                  #                      , "A 'data change' map will be saved in the selected format."
                  #                      , "top"
                  #                      , trigger = "hover"
                  #                      , options = list(container = "body"))
                  # )##fluidRow ~ save button
                  , p("To save the maps use the button on the screen above each map.")
                  , p("The interactive map does not have a save option.")
                   
                )##sidebarPanel~END
                , mainPanel(
                    tabsetPanel(type = "tabs"
                                , tabPanel("Range Map, Interactive"
                                 , leafletOutput("map_r_leaflet_basic"
                                                 , height = "85vh")
                                )
                      , tabPanel("Range Map, Static"
                                 , br()
                                 , downloadButton("but_map_range_basic_save"
                                                  , "Save Range Basic Map")
                                 , hr()
                                 , plotOutput("map_r_render_basic"
                                              , height = 800
                                              ,  width=800/1.5)
                                            
                                 )##tabPanel ~ Range Map Static
                      , tabPanel("Change Map"
                                 , br()
                                 , downloadButton("but_map_trend_basic_save"
                                                  , "Save Change Basic Map")
                                 , hr()
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