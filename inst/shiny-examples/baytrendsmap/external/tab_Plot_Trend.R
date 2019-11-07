# Create a map based on user selections
# uiOutput coded in server.R
#~~~~~~~~~~~~~~~~~
#
function(){
  tabPanel("3.b. Plot Trend"
           , fluidPage(h2("Plot Data Trend")
                       , br()
                       #, p(paste0("abc", nrow_df_import(), "xyz"))
                       
                       , sidebarLayout(
                         sidebarPanel(width=3, 
                                      # App Steps
                                      #h3("App Steps")
                                      h3("3.b. Trend Map Options")
                                      , fluidRow(column(1),
                                                 column(1, bsButton("but_map_trend", "Update Trend Map")),
                                                 bsPopover("but_map_trend", "Click 'apply' after modifying filters", "A 'data trend' map will be created with the selected options.",
                                                           "top", trigger = "hover", options = list(container = "body"))
                                      )##fluidRow~filter button~END
                                      
                                      
                                      
                                     , br()
                                     , bsCollapse(multiple = TRUE
                                                  , bsCollapsePanel("Direction of 'good' trend", style='info',
                                                                    uiOutput('opt_upisgood')
                                                  )##bsCollapsePanel~upisgood~END
                                                  # , bsCollapsePanel("Class Interval", style='info',
                                                  #                   uiOutput('opt_classInt')
                                                  # )##bsCollapsePanel~classInt~END
                                                  # , bsCollapsePanel("Color Brewer Palatte", style='info',
                                                  #                   uiOutput('opt_pal')
                                                  # )##bsCollapsePanel~pal~END
                                                  , bsCollapsePanel("File Save Extension", style='info',
                                                                    uiOutput('opt_ext_t')
                                                  )##bsCollapsePanel~ext~END
                                                  , bsCollapsePanel("River Names (Y/N)", style='info',
                                                                    uiOutput('opt_riverNames_t')
                                                  )##bsCollapsePanel~riverNames~END

                                      )##bsCollapse~END
                                     , numericInput("map_trend_val_poss", h4(paste0("  Threshold for 'possible' trend:"))
                                                    , value = 0.25
                                                    , min = 0.01
                                                    , max = 0.99
                                                    , step = 0.01)
                                     , numericInput("map_trend_val_sig", h4(paste0("  Threshold for 'significant' trend:"))
                                                    , value = 0.05
                                                    , min = 0.01
                                                    , max = 0.99
                                                    , step = 0.01)
                                      , fluidRow(column(1),
                                                 column(1, tipify(bsButton("but_mt_title", "Auto-generate Title")
                                                                  , "Click to auto-generate title from 'filters'.")))
                                      , textAreaInput("map_trend_title", "Title: "
                                                      , value = NULL
                                                      , rows = 4
                                      )##textInput~END
                                      # , sliderInput("numclass", "Number of classes"
                                      #               , min = 3, max = 8, value = 5)
                                     , br()
                                     , bsCollapse(multiple = TRUE
                                                  , bsCollapsePanel("Zoom Region", style='info',
                                                                    uiOutput('opt_zoomregion_t')
                                                  )##bsCollapsePanel~zoomregion~END
                                     )##bsCollapse~END
                                     , numericInput("map_trend_val_zoom", h4(paste0("  Zoom buffer (decimal degrees):"))
                                                    , value = 0.05
                                                    , min = 0.01
                                                    , max = 2.5
                                                    , step = 0.01)
                                       
                                      , hr()
                                      , p("Must 'update' map prior to save.")
                                      , fluidRow(column(1),
                                                 column(1, downloadButton("but_map_trend_save", "Save Trend Map")),
                                                 bsPopover("but_map_trend_save", "Click 'save' after updating map", "A 'data trend' map will be saved in the selected format.",
                                                           "top", trigger = "hover", options = list(container = "body"))
                                      )##fluidRow~save button~END
                                      , br()
                                      
                                      
                         )##sidebarPanel~END
                         , mainPanel(plotOutput("map_t_render", height = 800,  width=800/1.5)
                         #, mainPanel(plotlyOutput("map_t_render", height = 800,  width=800/1.5)
                                     
                         )##mainPanel~END
                         
                         
                       )##sidebarLayout~END
                       
                       
           )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END