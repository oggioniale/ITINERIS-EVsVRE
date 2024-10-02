#' Run the main app
#' @import shiny
#' @export
runShinyApp <- function() {
  appDir <- system.file("app", package = "ITINERIS.EVsVRE")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `ItinerisEVsVRE`.", call. = FALSE)
  }
  #source(appDir)

  shiny::runApp(appDir, display.mode = "normal")
}
