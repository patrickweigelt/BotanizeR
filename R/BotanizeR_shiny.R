#' Shiny application
#'
#' Running the Shiny application of BotanizeR.
#'
#' @param run a logical, when TRUE, the Shiny application is lauched
#' 
#' @return
#' Shiny application.
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R-package with Shiny-App for practicing plant identification in times of
#'     online teaching and beyond. submitted.
#'
#' @examples
#' # Example
#' BotanizeR_shiny(run = FALSE)
#' 
#' @export

BotanizeR_shiny <- function(run = FALSE) {
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