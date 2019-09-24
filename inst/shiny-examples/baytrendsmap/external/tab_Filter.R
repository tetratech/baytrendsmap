# Filter data
# uiOutput coded in server.R
#~~~~~~~~~~~~~~~~~
#
function(){
  tabPanel("2. Filter Data"
           #, fluidPage(

             , sidebarLayout(
                 sidebarPanel(width=3, 
                   # App Steps
                   #h3("App Steps")
                    h3("2. Filter Data")
                    , fluidRow(column(1),
                               column(1, bsButton("but_filt_apply", "Apply Filters")),
                               # bsPopover("but_filt_apply", "Click 'apply' after modifying filters", "Only filters with items selected will be applied. Note: At least one station must be selected.",
                               #           placement = "top", trigger = "hover", options = list(container = "body"))
                               bsPopover(id = "but_filt_apply", title = "Apply Filters"
                                         , content = "Click 'apply' after modifying filters", "Only filters with items selected will be applied. Note: At least one station must be selected.")
                               )##fluidRow~filter button~END
                    
                    
                    
                    , br()
                    , bsCollapse(multiple = TRUE,
                                 bsCollapsePanel("Filter by 'State'", style='info',
                                                 fluidRow(column(1), column(10, radioButtons('sel_state', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                 uiOutput('filt_state')
                                                 )##bsCollapsePanel~state~END
                                 , bsCollapsePanel("Filter by 'CB Segment'", style='info',
                                                  fluidRow(column(1), column(10, radioButtons('sel_cbSeg92', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                  uiOutput('filt_cbSeg92')
                                                  )##bsCollapsePanel~cbSeg92~END
                                 , bsCollapsePanel("Filter by 'Station Group'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_stationGrpName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_stationGrpName')
                                                  )##bsCollapsePanel~stationGrpName~END
                                 , bsCollapsePanel("Filter by 'Station Identifier'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_station', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_station')
                                                  )##bsCollapsePanel~station~END
                                 , bsCollapsePanel("Filter by 'Full Parameter Name'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_parmName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_parmName')
                                                    )##bsCollapsePanel~parmName~END
                                 , bsCollapsePanel("Filter by 'GAM Formula Name'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_gamName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_gamName')
                                                   )##bsCollapsePanel~gamName~END
                                 , bsCollapsePanel("Filter by 'Sample Layer'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_layer', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_layer')
                                                  )##bsCollapsePanel~layer~END
                                 , bsCollapsePanel("Filter by 'Period Name'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_periodName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_periodName')
                                                   )##bsCollapsePanel~periodName~END
                                 , bsCollapsePanel("Filter by 'Season Name'", style='info',
                                                   fluidRow(column(1), column(10, radioButtons('sel_seasonName', "", c("Select All"=1, "Deselect All" = 2), selected = 1))),
                                                   uiOutput('filt_seasonName')
                                                  )##bsCollapsePanel~seasonName~END
                 )##bsCollapse~END
                 , br()
                 , fluidRow(column(1), 
                          column(1,bsButton("but_ClearFilters", label="Reset Filterer Selections", style="primary"))
                          )##fluidRow~END
                 
                    
                    
                    
                    
                    
                   
                )##sidebarPanel~END
                , mainPanel(
                  tabsetPanel(type="tabs"
                              , tabPanel("Filter Data Summary"
                                         , br()
                                         , p("The most common fields that require filtering are 'Parameter', 'GAM', 'Layer', 'Period', and 'Season'.")
                                         , p("Number of unique entries by station are in the table below. If greater than 1 need to modify filters and re-'apply'.")
                                         , br()
                                         , textOutput("filt_dups_num")
                                         , br()
                                         , DT::dataTableOutput("df_filt_dups_DT")
                                         )##tabPanel~Data Filter Summary ~ END
                              ,tabPanel("Filter Data"
                                        #, p(paste0("Number of records imported (n=", -999 ,") and after filters applied (n=", -999, ")."))
                                        # , renderText("txt_nrow_df_import")
                                        # , p("Once have target turn green or something.")
                                        # , p("At this point no 'save' or 'load' feature for the filters".)
                                        # , p("need to modify for 'user-supplied' variables.")
                                        # , DT::dataTableOutput('df_import_DT')
                                        , DT::dataTableOutput('df_filt_DT')
                                        #, DT::dataTableOutput('df_import_DT')
                                        )##tabPanel~Data~END
                              ,tabPanel("Range Map"
                                        , tab_Plot_Range()
                                        )##tabPanel~Range~END
                              ,tabPanel("Trend Map"
                                        , tab_Plot_Trend()
                                        )##tabPanel~Trends~END
                  )##tabsetPanel~END
                )##mainPanel~END
                
                
             )##sidebarLayout~END
             
          # )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END