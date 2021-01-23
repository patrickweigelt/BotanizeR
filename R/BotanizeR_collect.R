### BotanizeR_collect
BotanizeR_collect <- function(species_row, image_floraweb=TRUE, hints_floraweb = NULL, 
                           hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                           file_location="temporary", only_links = FALSE, image_required = FALSE, 
                           image_width = NA){
  
  # Information can come from floraweb and/or from own resources
  
  # 1. Controls ----
  # Package dependencies
  require(imager)
  require(XML)
  if("map" %in% hints_floraweb){
    require(sf)
  }
  
  # Arguments
  if(!all(hints_floraweb %in% c("map","description","status","habitat","family","German name"))){
    stop('"hints_floraweb" must be a subset of c("map","description","status","habitat","family","German name")')
  }
  
  # nrow(species_row) == 1
  
  # all(hints_custom %in% colnames(species_row))
  
  # all(imagelink_custom %in% colnames(species_row))

  
  # 2. Prep ----
  if(file_location=="temporary"){
    # Create folder for temp files
    dir <- tempfile()
    dir.create(dir)
  } else {
    dir <- file_location
  }
  species <- species_row$SPECIES
  
  hints <- list()
  hints[[1]] <- list()
  names(hints)[1] <- "images"
  floraweb_image <- FALSE
  
  # 3. Images ----
  # 3.1 Floraweb ----
  
  if(!is.na(species_row$NAMNR) & species_row$NAMNR != "" & (!is.null(hints_floraweb) | image_floraweb)){
    
    # Main infos
    # I need this step because of an error in RCURL when getting the url
    download.file(paste("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",species_row$NAMNR,"&", sep=""), 
                  destfile = file.path(dir,"main.txt"), quiet = T)
    html_main <- htmlTreeParse(file = file.path(dir,"main.txt"), isURL = F, isHTML=T, useInternalNodes = T)
    infos_main <- xpathApply(html_main, "//div[@id='content']//p",xmlValue)
    
    if(image_floraweb){
      
      #download.file(paste("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",species_row$NAMNR[i], sep=""),
      #                    destfile = file.path(dir,"photo.txt"), quiet = T)
      
      # Photo
      if(length(xpathApply(html_main, "//a[@class='imglink']",xmlAttrs))>0 & 
         grepl("foto\\.xsql",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs))[1]){
        
        download.file(paste("https://www.floraweb.de/pflanzenarten/",grep("foto\\.xsql",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)[[1]], value = T), sep=""), 
                      destfile = file.path(dir,"photo.txt"), quiet = T)
        html_photo <- htmlTreeParse(file = file.path(dir,"photo.txt"), isURL = F, isHTML=T, useInternalNodes = T)
        infos_photo <- xpathApply(html_photo, "//div[@id='content']//p",xmlValue)
        # photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
        photolinks <- sapply(xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs), function(x) grep("bilder", x, value=T))
        
        if(photolinks[1]!="../bilder/arten/"){
          if(only_links){
            hints[[1]][[1]] <- paste("https://www.floraweb.de", gsub("\\.\\.","",photolinks[1]), sep="")
            floraweb_image <- TRUE  
          } else {
            try({hints[[1]][[1]] <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",photolinks[1]), sep=""))
                floraweb_image <- TRUE       
            },silent = T)
          }
          if (length(photolinks)>1){ # check for cases with more then two images and put loop here
            if(only_links){
              hints[[1]][[2]] <- paste("https://www.floraweb.de", gsub("\\.\\.","",gsub("\\.tmb","",photolinks[2])), sep="")
            } else {
              try(hints[[1]][[2]] <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",gsub("\\.tmb","",photolinks[2])), sep="")),silent = T)
            }
          }
        }
      }
    }
  }
  
  # 3.2 Images from own image link ----
  if(!is.null(imagelink_custom)){
    for(i in 1:length(imagelink_custom)){
      if(!is.na(species_row[,imagelink_custom[i]]) & species_row[,imagelink_custom[i]] != ""){
        if(only_links){
          hints[[1]][[length(hints[[1]])+1]] <- species_row[,imagelink_custom[i]]
        } else {
          try({hints[[1]][[length(hints[[1]])+1]] <- load.image(species_row[,imagelink_custom[i]])})
        }
      }
    }
  }

  # 3.3 Images from image folder ----
  if(!is.null(image_folders)){
    for(k in 1:length(image_folders)){
      image_files <- list.files(image_folders[k], pattern = "\\.jpg|\\.jpeg",
                                recursive = TRUE, full.names = FALSE)
      image_files <- image_files[which(grepl(species, image_files) | grepl(gsub(" ","_",species), image_files))]
      if(length(image_files)>0){
        if(only_links){
          # WWW/image_folder in local shiny needs to be image_folder
          # ~/ShinyApps/BotanizeR/WWW/image_folder in server shiny needs to be image_folder
          for(i in 1:length(image_files)){
            hints[[1]][[length(hints[[1]])+1]] <- file.path(gsub(".*[wwwWWW]/(.+)$",("\\1"),image_folders[k]),image_files[i])
          }
        } else {
          for(i in 1:length(image_files)){
            try(hints[[1]][[length(hints[[1]])+1]] <- load.image(file.path(image_folders[k],image_files[i])))
          }
        }
      }
    }
  }

  # 3.4 Image resize ----
  
  if(!is.na(image_width) & !is.null(image_width) & length(hints$images) > 0 & only_links == FALSE){
    hints$images <- lapply(hints$images, function(x) {
      if(nrow(x) > image_width){
        resize(x, size_x = image_width, size_y = image_width/nrow(x)*ncol(x))
      } else {
        x
      }
    })
  }
  
  # 4. Other information ----
  if(image_required & length(hints$images) == 0){
    hints_floraweb <- NULL
    hints_custom <- NULL
  }
  
  # if(!is.null(hints_floraweb)){
  # if(length(hints$images) > 0){
    
    # 4.1 Floraweb ----
    if(!is.na(species_row$NAMNR) & species_row$NAMNR != "" & !is.null(hints_floraweb)) { 
      
      # ecology
      if("habitat" %in% hints_floraweb){
        download.file(paste0("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",species_row$NAMNR,"&"), 
                      destfile = file.path(dir,"ecology.txt"), quiet = T)
        html_ecology <- htmlTreeParse(file = file.path(dir,"ecology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
        infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",xmlValue)
      }
      
      # biology
      if("description" %in% hints_floraweb & floraweb_image == FALSE){
        download.file(paste0("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=", species_row$NAMNR, "&"),
                      destfile = file.path(dir, "biology.txt"), quiet = TRUE)
        html_biology <- htmlTreeParse(file = file.path(dir, "biology.txt"),
                                      isURL = FALSE, isHTML = TRUE,
                                      useInternalNodes = TRUE)
        infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",
                                    xmlValue)
      }
      
      # map
      map <- NA
      if("map" %in% hints_floraweb & length(xpathApply(html_main, "//a[@class='imglink']",xmlAttrs))>0 & 
         any(grepl("webkarten",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)))){
        
        if(!exists("CGRS_Germany")){
          data(CGRS_Germany)
        }
        
        try({
          taxon_ID_map <- gsub("/webkarten/karte.html\\?taxnr=", "",
                               grep("webkarten",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)[[which(grepl("webkarten",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)))]], value = T))
          
          download.file(paste0("https://www.floraweb.de/pflanzenarten/download_afe.xsql?suchnr=", taxon_ID_map),
                        destfile = file.path(dir, "map.csv"), quiet = TRUE)
          map <- read.csv(file.path(dir, "map.csv"), skip=41)[-1, c("CGRSNAME","AFE_SYMBOLCODE","AFESYMBOL_TEXT")]
          
          map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE==4)] <- 2
          map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE==5)] <- 3
          map$AFE_SYMBOLCODE[which(map$AFESYMBOL_TEXT=="cultivated, synanthrope, not established aliens)")] <- 4
          map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE==6)] <- 5
          map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE==8)] <- 6
          map$AFE_SYMBOLCODE <- map$AFE_SYMBOLCODE + 2
          
          map <- map[order(map$AFE_SYMBOLCODE, decreasing = TRUE),]
          map <- map[!duplicated(map$CGRSNAME),]
          
          map <- merge(CGRS_Germany, map, all.x = TRUE, sort=FALSE)
          map$AFE_SYMBOLCODE[is.na(map$AFE_SYMBOLCODE)] <- 1
          map$AFE_SYMBOLCODE <- as.factor(map$AFE_SYMBOLCODE)
          
          legend_info <- data.frame(AFE_SYMBOLCODE=c(1:8), 
                                    SYMBOL_TEXT=c("absent","not assigned","records uncertain","extinct","probably extinct",
                                                  "cultivated, not established alien","established alien","native, incl. archaeophytes"),
                                    colour=c("white","grey90","grey80","grey70","grey60","#fdb462","#fb8072","#b3de69")) 
          
          legend_info <- legend_info[which(legend_info$AFE_SYMBOLCODE %in% map$AFE_SYMBOLCODE),]
          
          levels(map$AFE_SYMBOLCODE) <- legend_info$SYMBOL_TEXT
        }, silent = TRUE)
      }

      for (i in 1:length(hints_floraweb)){
        
        if(hints_floraweb[i] == "description"){
          if(floraweb_image){
            description <- paste0("Bestimmungshilfe/Morphologie:\n",
                                  infos_photo[[which(infos_photo == "Bestimmungshilfe:")+1]])
          } else {
            description <- gsub("Morphologie:","Morphologie:\n",infos_biology[[2]])
          }
          if(!grepl("keine Angaben", description)){
            hints[[i+1]] <- description
          }
        }
        
        if(hints_floraweb[i]=="status"){
          hints[[i+1]] <- paste(infos_main[[7]],"\n",infos_main[[8]], sep="")
        }
        
        if(hints_floraweb[i]=="family"){
          hints[[i+1]] <- paste(infos_main[[6]])
        }
        
        if(hints_floraweb[i]=="German name"){
          hints[[i+1]] <- paste(infos_main[[5]])
        }
        
        if(hints_floraweb[i]=="habitat"){
          if(infos_ecology[[3]] != "Formation: \r\nTaxon keiner Formation zugeordnet "){
            hints[[i+1]] <- paste(infos_ecology[[3]])
          }
        }
        
        if(hints_floraweb[i]=="map"){
          if(!is.na(map)[1]){
            hints[[i+1]] <- list(map["AFE_SYMBOLCODE"], legend_info$colour)
          }
        }
        
        if(length(hints)==i+1){
          names(hints)[i+1] <- hints_floraweb[i]
        }
      }
    }
    
    
    # 4.2 Hints from own entries ----
    if(!is.null(hints_custom)){
      for(i in 1:length(hints_custom)){
        if(!is.na(species_row[,hints_custom[i]]) & species_row[,hints_custom[i]] != ""){
          hints[[length(hints)+1]] <- species_row[,hints_custom[i]]
          names(hints)[length(hints)] <- hints_custom[i]
        }
      }
    }
  # }
  
  hints <- hints[which(!sapply(hints, is.null))]
  
  return(hints)
} 




