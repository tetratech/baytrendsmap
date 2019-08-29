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
                         , fluidRow(column(3),
                                    column(1, bsButton("but_map_range", "Apply Range Map Options")),
                                    bsPopover("but_map_range", "Click 'apply' after modifying filters", "A 'data range' map will be created with the selected options.",
                                              "top", trigger = "hover", options = list(container = "body"))
                         )##fluidRow~filter button~END
                         
                         
                         
                         , br()
                         , bsCollapse(multiple = TRUE
                                      , bsCollapsePanel("var", style='info',
                                                      uiOutput('opt_var')
                                                      )##bsCollapsePanel~variable~END
                                      , bsCollapsePanel("classInt", style='info',
                                                        uiOutput('opt_classInt')
                                                        )##bsCollapsePanel~classInt~END
                                      , bsCollapsePanel("col", style='info',
                                                        uiOutput('opt_col')
                                                        )##bsCollapsePanel~col~END
                                      , bsCollapsePanel("ext", style='info',
                                                        uiOutput('opt_ext')
                                                        )##bsCollapsePanel~ext~END
                                      , bsCollapsePanel("riverNames", style='info',
                                                        uiOutput('opt_riverNames')
                                      )##bsCollapsePanel~riverNames~END
                                      
                         )##bsCollapse~END
                         , textAreaInput("title", "Title: "
                                     , value = paste0("Layer: "
                                                      , "; Parameter: "
                                                      , "; Season: "
                                                      , "; Period: ")
                                     , rows = 4
                                     )##textInput~END
                         , sliderInput("numclass", "Number of classes"
                                       , min = 3, max = 8, value = 5)
                         
                         
                         
                         
                         
            )##sidebarPanel~END
            , mainPanel(plotOutput("map_r", height = "600")
              
            )##mainPanel~END
            
            
          )##sidebarLayout~END
          
             
           )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END