# Load a baytrends package output file.
#~~~~~~~~~~~~~~~~~
#
function() {
  tabPanel("Background"
    , p("Maryland Department of Natural Resources (MDDNR), Virginia Department of Environmental Quality (VADEQ), the District of Columbia, and others have coordinated to sample water quality on a bi-monthly or monthly basis at more than 130 stations located throughout the mainstem of the Chesapeake Bay and the tidal portions of numerous tributaries on the western and eastern shores since the mid-1980s. Scientists evaluate long- and short--term changes, or trends, in nutrients, dissolved oxygen (DO), Secchi depth (a measure of clarity), chlorophyll-a, and water temperature using a Generalized Additive Modeling (GAM) approach.")
    , p("The GAM approach enables us to describe nonlinear seasonally varying changes over time, to incorporate hydrologic variability via either river flow or salinity, to accommodate method or laboratory changes suspected to impact data values, and to represent left- and interval-censored data (Murphy et al, 2019, 2021). The baytrends R package was designed to fit GAMs for the tidal Chesapeake Bay water quality data. The package has already been used by MDNNR and VADEQ (via analysts at Old Dominion University, ODU) and can be downloaded through CRAN for anyone interested in the analyses.")
    , p("Long- and short-term changes")
    , p("Long-term changes are based on data from 1985 to present with the exception of dissolved of dissolved inorganic nitrogen (DIN), orthophosphorus (PO4), and total suspended solids (TSS) which are based on data from 1999 to present. Short-term trends are based on data from the last 10 years.")
    , p("Flow/salinity-adjustment procedure")
    , p("Water quality in the tidal waters is frequently correlated to the amount of freshwater flowing in the bay. There are very high fluctuations in freshwater river flow measured at the fall-line river input monitoring stations, which can influence many factors that impact water quality including the amount of nutrients flowing into the bay, the degree of vertical density stratification, and the overall circulation. Therefore, a flow-adjustment procedure for the GAM implementation in Chesapeake Bay tidal waters is an option when producing trend results. The nine USGS river gages at the fall-lines of the major tributaries provide daily flow values to use in the GAM approach. Another option is to use salinity measured at the same place and time as the water quality observations. This approach has precedent (Beck and Hagy 2015; Beck and Murphy 2017) and may be advantageous to use in locations where it is unclear which USGS gage to match to a monitoring station.")
    , p("Laboratory and method changes")
    , p("In addition to changes in detection limits for water quality observations over time, there have also been changes in analytical laboratories and methods for some of the parameters. In many cases, when it was suspected that a change impacted the values of observations, a paired sampling study was conducted to compare the two methods side-by-side (CBP 2010) and often an adjustment factor was computed that could be used to account for the change.")
    , p("Selected Tidal Water Quality Change results can be found on CAST " 
      , a("here"
        , href = "https://cast.chesapeakebay.net/TrendsOverTime"))
        
    , p("Beck, M.W. and J.D. Hagy. 2015. Adaptation of a Weighted Regression Approach to Evaluate Water Quality Trends in an Estuary. Environ Model Assess 20:637. "
      , a("doi: 10.1007/s10666-015-9452-8."
        , href = "https://doi.org/10.1007/s10666-015-9452-8"
        , target = "_blank"))
    
    , p("Beck, M.W., Murphy, R.R., 2017. Numerical and Qualitative Contrasts of Two Statistical Models for Water Quality Change in Tidal Waters. JAWRA J. Am. Water Resour. Assoc. 53, 197–219. "
      , a("doi: 10.1111/1752-1688.12489."
        , href = "https://doi.org/10.1111/1752-1688.12489"
        , target = "_blank"))
    
    , p("Murphy, R.R., E. Perry, J. Harcum, and J. Keisman. 2019. A generalized additive model approach to evaluating water quality: Chesapeake Bay case study. Environmental Modeling and Software 118(August 2019):1-13. "
      , a("doi: 10.1016/j.envsoft.2019.03.027."
        , href = "https://doi.org/10.1016/j.envsoft.2019.03.027"
        , target = "_blank")
            )
    
    , p("Murphy, R, E. Perry, J. Keisman, J. Harcum, and E. Leppo. 2021. baytrends: Long Term Water Quality Trend Analysis. R package version 2.0.5."
      , a("https://CRAN.R-project.org/package=baytrends"
        , href = "https://CRAN.R-project.org/package=baytrends"
        , target = "_blank"))
          
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
