#' BotanizeR Shiny application
#'
#' Running the Shiny application of BotanizeR locally from installed BotanizeR 
#' R package.
#'
#' @param run a logical, when TRUE, the Shiny application is launched. Can be 
#' set to FALSE to avoid launching the shiny app in a non-interactive 
#' environment.
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
#' \dontrun{
#' BotanizeR_shiny()
#' }
#' 
#' @export

BotanizeR_shiny <- function(run = TRUE) {
  
  if(!is.logical(run)){
    stop("'run' must be a logical indicating if BotanizeR Shiny app shall be 
         launched")
  }
  
  appDir <- system.file(package = "BotanizeR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `BotanizeR`.",
         call. = FALSE)
  }
  
  if(run){
  shiny::runApp(appDir, display.mode = "normal")
  } else {
    message("If you want to run the application, set 'run = TRUE'")
  }
}