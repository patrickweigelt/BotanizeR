#' Batch resize images
#'
#' Batch resize images for plotting in [BotanizeR::BotanizeR_quiz()] or 
#' BotanizeR Shiny app maintaining aspect ratio.
#'
#' @param image_folders character vector defining folders where images
#' that shall be resized are stored.
#' 
#' @param image_width number defining the width of the resized images as number 
#' of pixels.
#' 
#' @param max_height number defining the maximum height of the resized images. 
#' If `NA`, `max_height` is ignored and all images are resized to a width of 
#' `image_width`. If `max_height` is defined images are resized to a width of 
#' `image_width` only if a height of `max_height` is not exceeded. Otherwise 
#' they will be resized to a height of `max_height`.
#' 
#' @param int_type number indicating the method of interpolation used in 
#' [imager::resize()]: -1 = no interpolation: raw memory resizing. 
#' 0 = no interpolation: additional space is filled according to 
#' boundary_conditions. 1 = nearest-neighbor interpolation. 
#' 2 = moving average interpolation. 3 = linear interpolation. 
#' 4 = grid interpolation. 5 = cubic interpolation. 6 = lanczos interpolation.
#' 
#' @param quality numeric > 0 and <= 1 defining the quality of the images when
#' saved as *.jpg by [imager::save.image()]. Higher quality means less 
#' compression.
#' 
#' @return
#' Resized images will be saved at the same location where the input folders 
#' are located but folder names will be expanded about 
#' `paste("_",image_width,sep="")`.
#'
#' @details Resizing images to make them fit and load faster in the 
#' BotanizeR Shiny application and [BotanizeR::BotanizeR_quiz()].
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R package with Shiny app to practice plant identification for 
#'     online teaching and beyond. *PLANTS, PEOPLE, PLANET*, 
#'     [https://doi.org/10.1002/ppp3.10226](https://doi.org/10.1002/ppp3.10226).
#'
#' @seealso [imager::resize()]
#'
#' @examples
#' # Example
#' 
#' \dontrun{
#' BotanizeR_imageresize(image_folders = "inst/WWW/images_to_resize", 
#'                       image_width = 560, max_height = 700, quality = 1)
#'                       
#' BotanizeR_imageresize(image_folders = c("inst/WWW/images_to_resize_1", 
#'                                         "inst/WWW/images_to_resize_2"), 
#'                       image_width = 560, max_height = 700, 
#'                       quality = 1, int_type = 5)
#' }
#' 
#' @export

BotanizeR_imageresize <- function(image_folders = NULL, image_width = NA,
                                  max_height = NA, int_type = 1, quality = 1){
  
  # 1. Controls ----
  # Arguments

  if(is.null(image_folders) | !is.character(image_folders)){
      stop("'image_folders' must be a character vector defining folders where 
           images that shall be resized are stored")
  }

  
  if(!is.numeric(image_width) | is.na(image_width)){
    stop("'image_width' must be numeric defining the width of
           the resized images in number of pixels.")
  }
  
  if(!is.na(max_height)){
    if(!is.numeric(max_height)){
      stop("'max_height' must be either NA or a number defining the maximum 
      height of the resized images in pixels.")
    }
  }
  
  if(!is.numeric(int_type)){
    stop("'int_type' must be a numeric defining the type of the indicating the 
         method of interpolation.")
  }
  
  if(!is.numeric(quality)){
    stop("'quality' must be a numeric > 0 and <= 1 defining the quality of the 
         saved *.jpg images.")
  } else {
    if(quality <= 0 | quality > 1) {
      stop("'quality' must be a numeric > 0 and <= 1 defining the quality of 
           the saved *.jpg images.")
    }
  }
  
  # 2. Prep ----
  
  # 3. Scan image folders, resize and save images ----
  for(k in seq_along(image_folders)){
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
      for(i in seq_along(image_files)){
        
        # load
        image_i <- imager::load.image(file.path(image_folders[k],
                                                image_files[i]))
        
        # rotate according to exif
        try(exif <- exifr::read_exif(file.path(image_folders[k],
                                           image_files[i]), tags="orientation"))
        
        if(!is.null(exif) & "Orientation" %in% names(exif)){
          if (exif$Orientation == 3){
            image_i <- imager::imrotate(image_i, 180)
          } else if (exif$Orientation == 6){
            image_i <- imager::imrotate(image_i, 90)
          } else if (exif$Orientation == 8){
            image_i <- imager::imrotate(image_i, 270)
          }   
        }
        
        # resize
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
        
        # save
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
    } else {
      message(paste0("No images found in ",image_folders[k],"!"))
    }
  }
  message("completed")
}
