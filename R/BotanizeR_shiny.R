
BotanizeR_shiny <- function() {
  require(shiny)
  
  appDir <- system.file(package = "BotanizeR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `BotanizeR`.",
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}