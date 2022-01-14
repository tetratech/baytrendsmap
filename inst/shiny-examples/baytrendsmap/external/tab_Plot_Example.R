# Create a map based on user selections
# uiOutput coded in server.R
#~~~~~~~~~~~~~~~~~
#
function(){
  tabPanel("0. Plot Example"
           , fluidPage(h2("Example, Plot Data Range")
          , br()
          #, p(paste0("abc", nrow_df_import(), "xyz"))
          
          , sidebarLayout(
            sidebarPanel(width=3 
                         # App Steps
                         #h3("App Steps")
                         , p("Example maps to the right.")
                         , p("Two types of maps ('Static' and 'Interactive') are generated with the same data and legend.")
                         , p("The 'Static' map is intended for inclusion in reports and presentations.")
                         , p("The 'Interactive' allows the user to manipulate it similar to Google Maps and other online mapping tools.")
                         , p("To generate custom maps proceed through selecting data, filter data.")
                         , p("Custom maps can be modified with onscreen tools.")
            )##sidebarPanel~END
            #, mainPanel(plotlyOutput("map_r_render", height = 800,  width=800/1.5)
            , mainPanel(
              tabsetPanel(type = "tabs"
                          , tabPanel("Static"
                                     , plotOutput("map_r_render_example"
                                                  , height = 800
                                                  ,  width=800/1.5)
                                     )##tabPanel, Static
                           , tabPanel("Interactive"
                                      # , leafletOutput("map_r_leaflet_example"
                                      #                 , height = "85vh")
                                      )## tabPanel, Dynamic
                          )##tabsetPanel ~ END
                        )##mainPanel~END
            
          )##sidebarLayout~END
          
             
           )##fluidPage~END
  )##tabPanel~END
  
}##FUNCTION~END