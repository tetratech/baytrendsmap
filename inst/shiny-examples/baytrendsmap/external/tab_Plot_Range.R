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
                                    column(1, bsButton("but_filt_apply", "Apply Trend Map Options")),
                                    bsPopover("but_filt_apply", "Click 'apply' after modifying filters", "Only filters with items selected will be applied. Note: At least one station must be selected.",
                                              "top", trigger = "hover", options = list(container = "body"))
                         )##fluidRow~filter button~END
                         
                         
                         
                         , br()
                         , bsCollapse(multiple = TRUE,
                                      bsCollapsePanel("Variable", style='info',
                                                      fluidRow(column(1), column(10, radioButtons('sel_state', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                      uiOutput('opt_var')
                                      )##bsCollapsePanel~state~END
                                      , bsCollapsePanel("Number of Classes", style='info',
                                                        fluidRow(column(1), column(10, radioButtons('sel_cbSeg92', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                        uiOutput('opt_numclass')
                                      )##bsCollapsePanel~cbSeg92~END
                                      , bsCollapsePanel("Class Interval", style='info',
                                                        fluidRow(column(1), column(10, radioButtons('sel_stationGrpName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                        uiOutput('opt_classint')
                                      )##bsCollapsePanel~stationGrpName~END
                                      , bsCollapsePanel("Color Style", style='info',
                                                        fluidRow(column(1), column(10, radioButtons('sel_station', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                        uiOutput('opt_color')
                                      )##bsCollapsePanel~station~END
                                      , bsCollapsePanel("Title", style='info',
                                                        fluidRow(column(1), column(10, radioButtons('sel_layer', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                        uiOutput('opt_title')
                                      )##bsCollapsePanel~layer~END
                                      , bsCollapsePanel("Legend Location", style='info',
                                                        fluidRow(column(1), column(10, radioButtons('sel_parmName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                        uiOutput('lopt_legloc')
                                      )##bsCollapsePanel~parmName~END
                                      , bsCollapsePanel("Output File Type", style='info',
                                                        fluidRow(column(1), column(10, radioButtons('sel_gamOption', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                        uiOutput('opt_outputfiletype')
                                      )##bsCollapsePanel~gamOption~END
                                      
                         )##bsCollapse~END
                         
                         
                         
                         
                         
                         
            )##sidebarPanel~END
            , mainPanel(
              tabsetPanel(type="tabs"
                          ,tabPanel("Data"
                                    , p(paste0("Number of records imported (n=", -999 ,") and after filters applied (n=", -999, ")."))
                                    # , renderText("txt_nrow_df_import")
                                    # , p("Once have target turn green or something.")
                                    # , p("At this point no 'save' or 'load' feature for the filters".)
                                    # , p("need to modify for 'user-supplied' variables.")
                                    # , DT::dataTableOutput('df_import_DT')
                                    , DT::dataTableOutput('df_filt_DT')
                          )##tabPanel~Data~END
                          ,tabPanel("Map")
              )##tabsetPanel~END
            )##mainPanel~END
            
            
          )##sidebarLayout~END
          
             
           )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END