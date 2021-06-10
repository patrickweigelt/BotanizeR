
BotanizeR_shiny <- function(run = FALSE) {
  require(shiny)
  
  appDir <- system.file(package = "BotanizeR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `BotanizeR`.",
         call. = FALSE)
  }
  
  if(run){
  shiny::runApp(appDir, display.mode = "normal")
  } else{
    message("If you want to run the application, set 'run = TRUE'")
  }
}