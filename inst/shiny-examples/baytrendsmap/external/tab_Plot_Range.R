# Create a map based on user selections
# uiOutput coded in server.R
#~~~~~~~~~~~~~~~~~
#
function(){
  tabPanel("3.a. Plot Range"
           , fluidPage(h2("Plot Data Range")
          , br()
          #, p(paste0("abc", nrow_df_import(), "xyz"))
          
          , sidebarLayout(
            sidebarPanel(width=3, 
                         # App Steps
                         #h3("App Steps")
                         h3("3. Range Map Options")
                         , fluidRow(column(1),
                                    column(1, bsButton("but_map_range", "Update Range Map")),
                                    bsPopover("but_map_range", "Click 'apply' after modifying filters", "A 'data range' map will be created with the selected options.",
                                              "top", trigger = "hover", options = list(container = "body"))
                         )##fluidRow~filter button~END
                         
                         
                         
                         , br()
                         , bsCollapse(multiple = TRUE
                                      , bsCollapsePanel("Variable to Plot", style='info',
                                                      uiOutput('opt_var')
                                                      )##bsCollapsePanel~variable~END
                                      , bsCollapsePanel("Class Interval", style='info',
                                                        uiOutput('opt_classInt')
                                                        )##bsCollapsePanel~classInt~END
                                      , bsCollapsePanel("Color Brewer Palatte", style='info',
                                                        uiOutput('opt_pal')
                                                        )##bsCollapsePanel~pal~END
                                      , bsCollapsePanel("File Save Extension", style='info',
                                                        uiOutput('opt_ext')
                                                        )##bsCollapsePanel~ext~END
                                      , bsCollapsePanel("River Names (Y/N)", style='info',
                                                        uiOutput('opt_riverNames')
                                      )##bsCollapsePanel~riverNames~END
                                      
                         )##bsCollapse~END
                         , textAreaInput("map_range_title", "Title: "
                                     , value = paste0("Layer: "
                                                      , "; Parameter: "
                                                      , "; Season: "
                                                      , "; Period: ")
                                     , rows = 4
                                     )##textInput~END
                         , sliderInput("numclass", "Number of classes"
                                       , min = 3, max = 8, value = 5)
                         
                         , br()
                         , p("Must 'update' map prior to save.")
                         , fluidRow(column(1),
                                    column(1, downloadButton("but_map_range_save", "Save Range Map")),
                                    bsPopover("but_map_range_save", "Click 'save' after updating map", "A 'data range' map will be saved in the selected format.",
                                              "top", trigger = "hover", options = list(container = "body"))
                         )##fluidRow~save button~END
                         , br() 
                         
                         
            )##sidebarPanel~END
            , mainPanel(plotOutput("map_r_render", height = 800,  width=800/1.5)
              
            )##mainPanel~END
            
            
          )##sidebarLayout~END
          
             
           )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END