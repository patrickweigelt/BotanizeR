#' Resizing images
#'
#' Batch resize images for plotting in BotanizeR quiz.
#'
#' @param image_folders character vector that defines a specific folder from
#' which the user wants to retrieve images
#' 
#' @param image_width number to define the width of the images.
#' 
#' @param max_height number to define the height of the images.
#' 
#' @param int_type number to define the type of the images.
#' 
#' @param quality number to define the quality of the images.
#' 
#' @return
#' Resized images.
#'
#' @details Resizing images so they fit in the Shiny application.
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R-package with Shiny-App for practicing plant identification in times of
#'     online teaching and beyond. Plants, People, Planet.
#'
#' @seealso [imager::resize()]
#'
#' @examples
#' # Example
#' 
#' @export

BotanizeR_imageresize <- function(image_folders = NULL, image_width = NA,
                                  max_height = NA, int_type = 1, quality = 1){
  
  # 1. Controls ----
  # Arguments
  
  
  # 2. Prep ----
  
  # 3. Scan image folders and resize images ----
  if(!is.null(image_folders)){
    for(k in 1:length(image_folders)){
      image_files <- list.files(
        image_folders[k],
        pattern = "\\.jpg|\\.jpeg|\\.png|\\.JPG|\\.JPEG|\\.PNG",
        recursive = TRUE, full.names = FALSE)
      
      if(!dir.exists(file.path(paste(image_folders[k],
                                     image_width, sep = "_")))){
        dir.create(file.path(paste(image_folders[k], image_width, sep = "_")))
      } else {
        stop("Error: Folder ", file.path(paste(image_folders[k], image_width,
                                               sep = "_")), " already exists!")
      }
      
      # load, resize, save
      if(length(image_files) > 0){
        for(i in 1:length(image_files)){
          image_i <- load.image(file.path(image_folders[k], image_files[i]))
          
          if(!is.na(max_height) &
             (image_width/nrow(image_i)*ncol(image_i)) > max_height){
            image_i <- imager::resize(
              image_i,
              size_x = max_height/ncol(image_i)*nrow(image_i),
              size_y = max_height, interpolation_type = int_type)
          } else {
            image_i <- imager::resize(
              image_i, size_x = image_width,
              size_y = image_width/nrow(image_i)*ncol(image_i),
              interpolation_type = int_type)
          }
          
          # save image
          if(grepl("/",image_files[i]) &
             !dir.exists(file.path(paste(
               image_folders[k], image_width, sep="_"),
               gsub("(^.*/)([^/]*)$", "\\1", image_files[i])))){
            dir.create(file.path(paste(
              image_folders[k], image_width, sep="_"),
              gsub("(^.*/)([^/]*)$", "\\1", image_files[i])))
          }
          imager::save.image(
            image_i,
            file.path(
              paste(image_folders[k], image_width, sep="_"),
              gsub("\\.JPG$|\\.JPEG$|\\.PNG$|\\.png$", "\\.jpg",
                   image_files[i])), quality = quality)
        }
      }
    }
  }
  message("completed")
}
