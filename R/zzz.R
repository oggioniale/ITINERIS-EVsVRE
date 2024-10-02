.onLoad <- function(libname, pkgname) {
  resources <- system.file("app/www", package = "ITINERIS.EVsVRE")
  shiny::addResourcePath("www", resources)
}
