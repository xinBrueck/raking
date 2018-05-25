#' Run shiny example
#'
#' This function will run the shiny app to interactively reprocess the survey input, 
#' get the census constrain data and get the weights
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "myApp", package = "raking2018")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `raking2018`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}