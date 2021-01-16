### BotanizeR_collect
BotanizeR_imageresize <- function(image_folders = NULL, image_width = NA, quality = 1){
  
  # 1. Controls ----
  # Package dependencies
  require(imager)

  # Arguments

  
  # 2. Prep ----

  # 3. Scan image folders and resize images ----
  if(!is.null(image_folders)){
    for(k in 1:length(image_folders)){
      image_files <- list.files(image_folders[k], pattern = "\\.jpg|\\.jpeg",
                                recursive = TRUE, full.names = FALSE)
      
      dir.create(file.path(image_folders[k],paste0("images_",image_width)))
      
      # load, resize, save
      if(length(image_files)>0){
        for(i in 1:length(image_files)){
          image_i <- load.image(file.path(image_folders[k],image_files[i]))
          # resize
          image_i <- resize(image_i, size_x = image_width, size_y = image_width/nrow(image_i)*ncol(image_i))
          # save image
          if(!dir.exists(file.path(image_folders[k],paste0("images_",image_width), gsub("(^.*/)([^/]*)$","\\1",image_files[i])))){
            dir.create(file.path(image_folders[k],paste0("images_",image_width), gsub("(^.*/)([^/]*)$","\\1",image_files[i])))
          }
          imager::save.image(image_i, file.path(image_folders[k],paste0("images_",image_width),image_files[i]), quality = quality)
        }
      }
    }
  }
  message("completed")
}

