#-------------------------------------------------------------------------------
# RMV2.0 (version 1.1.0)
# LBNL MV 2.0 Toolbox
# Samir Touzani, PhD
#-------------------------------------------------------------------------------

#' @import dplyr
#' @export
runRMV_UI <- function() {
  message("Starting the RMV2.0 App")
  appDir <- system.file("shiny", "guiApp", package = "RMV2.0")
  if (appDir == "") {
    stop("RMV2.0 is not installed", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
