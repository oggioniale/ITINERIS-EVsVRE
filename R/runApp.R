#' Run the main app
#' @import shiny
#' @param ... other params passed to `shiny::runApp`
#' @export
#' @examples 
#' ITINERIS.EVsVRE::runShinyApp(launch.browser=rstudioapi::viewer)
runShinyApp <- function(...) {
  appDir <- system.file("app", package = "ITINERIS.EVsVRE")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `ItinerisEVsVRE`.", call. = FALSE)
  }
  #source(appDir)

  shiny::runApp(appDir, display.mode = "normal", ...)
}
