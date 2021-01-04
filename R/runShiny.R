#' @title run baytrends Shiny Example
#'
#' @description Launches Shiny app for baytrends
#'
#' @details The Shiny app based on the R package baytrends is included in the R 
#' package. This function launches that app.
#'
#' The Shiny app is online at:
#' https://leppott.shinyapps.io/baytrendsmap
#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShiny()
#' }
#
#' @export
runShiny <- function(){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples", "baytrendsmap"
                        , package = "baytrendsmap")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `baytrendsmap`."
         , call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END