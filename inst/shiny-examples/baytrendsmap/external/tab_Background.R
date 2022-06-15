# Load a baytrends package output file.
#~~~~~~~~~~~~~~~~~
#
function() {
  tabPanel("Background"
           , p("Maryland Department of Natural Resources (MDDNR), Virginia Department of Environmental Quality (VADEQ), the District of Columbia, and others have coordinated to sample water quality on a bi-monthly or monthly basis at more than 130 stations located throughout the mainstem of the Chesapeake Bay and the tidal portions of numerous tributaries on the western and eastern shores since the mid-1980s. Scientists evaluate short- and long-term changes/trends in nutrients, dissolved oxygen (DO), Secchi depth (a measure of clarity), and chlorophyll-a using a Generalized Additive Modeling (GAM) approach.")
           , p("The approach includes selecting a GAM structure to describe nonlinear seasonally-varying changes over time, incorporation of hydrologic variability via either river flow or salinity, the use of an intervention to accommodate method or laboratory changes suspected to impact data values, and representation of left- and interval-censored data (Murphy et al, 2019, 2021).")
           , p("Selected results can be found here:"
               , a("Tidal Water Quality Change"
                   , href = "https://github.com/tetratech/baytrends_files/raw/main/TidalWaterQualityChange.pdf"))
          , p("Murphy, R.R., E. Perry, J. Harcum, and J. Keisman. 2019. A generalized additive model approach to evaluating water quality: Chesapeake Bay case study. Environmental Modeling and Software 118(August 2019):1-13.")
           
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