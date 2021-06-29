
#' Collect information for quiz
#'
#' Collects information from [FloraWeb](https://www.floraweb.de) 
#' (images, map and species descriptions) and from user defined image folders
#' and columns in the species_list data.frame to show them as hints in
#' [BotanizeR::BotanizeR_quiz()]
#'
#' @param species_row a data.frame with the species for which we want to
#' retrieve information. **It should contain the following columns**: 
#' *NAMNR*, *TAXONNAME*, *SPECIES*, *GENUS*, *EPITHET*, *AUTHOR*, *COUNT* and
#' *SCORE*
#'
#' @param image_floraweb logical that defines if images from
#' [FloraWeb](https://www.floraweb.de) should be retrieved
#'
#' @param hints_floraweb character vector that defines what hints the user
#' wants to retrieve from [FloraWeb](https://www.floraweb.de)
#'
#' @param image_ukplantatlas logical that defines if images from the
#' [Online Atlas of the British and Irish flora ](https://www.brc.ac.uk/plantatlas/)
#' should be retrieved
#'
#' @param hints_ukplantatlas character vector that defines what hints the user
#' wants to retrieve from the
#' [Online Atlas of the British and Irish flora ](https://www.brc.ac.uk/plantatlas/)
#'
#' @param imagelinks_custom character vector that defines a custom link to
#' retrieve images
#'
#' @param image_folders character vector that defines a specific folder from
#' which the user wants to retrieve images
#' 
#' @param hints_custom character vector that defines personal hints the user
#' wants to use. **Note:** in that case, these hints should be present as
#' columns in the `species_row` table.
#' 
#' @param file_location character vector that defines 
#' 
#' @param only_links logical, if `TRUE`, then the images behind the links are
#' not loaded.
#' 
#' @param image_required logical.
#' 
#' @param image_width number to define the width of the images.
#' 
#' @return
#' A functional distance matrix, **column** and **row** names follow
#' **species name** from `traits_table` row names.
#'
#'
#' @details This function combines with [BotanizeR::BotanizeR_quiz()]
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R-package with Shiny-App for practicing plant identification in times of
#'     online teaching and beyond. Plants, People, Planet.
#'
#' @seealso [BotanizeR::BotanizeR_quiz()]
#'
#' @examples
#' # Species list for Germany with IDs from floraweb.de
#' data(floraweb_species)
#'
#' # Select Acer campestre
#' species_row = floraweb_species[which(floraweb_species$SPECIES ==
#' "Acer campestre"),]
#'
#' # Some self-made hints
#' species_row$ownhint_1 <- "very nice species"
#' species_row$ownhint_2 <- "tree"
#' species_row$ownhint_3 <- NA
#' 
#' # Image links from Wikipedia
#' species_row$imagelink_1 <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/66/Acer_campestre_005.jpg/1280px-Acer_campestre_005.jpg"
#' species_row$imagelink_2 <- "https://upload.wikimedia.org/wikipedia/commons/f/f5/237_Acer_campestre.jpg"
#' species_row$imagelink_3 <- ""
#' 
#' # only floraweb image + description + map
#' hints <- BotanizeR_collect(
#' species_row, image_floraweb = TRUE, hints_floraweb = c("map",
#' "description", "status", "habitat", "family", "German name"),
#' hints_custom = NULL, imagelinks_custom = NULL, image_folders = NULL,
#' file_location="temporary")
#' 
#' par(oma = c(0, 0, 0, 10.5))
#' plot(hints$map[[1]], pal = hints$map[[2]], key.pos = 4, main = "")
#' 
#' par(mar = rep(0.5, 4), oma = rep(0, 4))
#' plot(hints$image[[1]], axes = FALSE)
#' 
#' message(hints$habitat)
#' 
#' # only custom image links + floraweb descriptions
#' hints <- BotanizeR_collect(species_row, image_floraweb = FALSE,
#' hints_floraweb = c("description", "status", "habitat", "family",
#' "German name"), hints_custom = NULL, imagelinks_custom = c("imagelink_1",
#' "imagelink_2", "imagelink_3"), image_folders = NULL,
#' file_location = "temporary")
#' 
#' # floraweb + custom images + floraweb descriptions
#' hints <- BotanizeR_collect(species_row, image_floraweb = TRUE,
#' hints_floraweb = c("description", "status", "habitat", "family",
#' "German name"), hints_custom = NULL, imagelinks_custom = c("imagelink_1",
#' "imagelink_2", "imagelink_3"), image_folders = NULL,
#' file_location = "temporary")
#' 
#' # floraweb + custom images + floraweb descriptions + custom description
#' hints <- BotanizeR_collect(species_row, image_floraweb = TRUE,
#' hints_floraweb = c("description", "status", "habitat", "family",
#' "German name"), hints_custom = c("ownhint_1", "ownhint_2", "ownhint_3"),
#' imagelinks_custom = c("imagelink_1", "imagelink_2", "imagelink_3"), 
#' image_folders = NULL, file_location="temporary")
#' 
#' # floraweb + custom images + only custom description
#' hints <- BotanizeR_collect(species_row, image_floraweb = TRUE,
#' hints_floraweb = NULL, hints_custom = c("ownhint_1", "ownhint_2",
#' "ownhint_3"), imagelinks_custom = c("imagelink_1", "imagelink_2",
#' "imagelink_3"), image_folders = NULL, file_location = "temporary")
#' 
#' # To load images from your local computer, specify an image folder with
#' pictures included. File names need to include the species names.
#' 
#' @export

BotanizeR_collect <-
  function(species_row, image_floraweb = TRUE, hints_floraweb = NULL,
           image_ukplantatlas = FALSE, hints_ukplantatlas = NULL,
           imagelinks_custom = NULL, image_folders = NULL, hints_custom = NULL,
           file_location = "temporary", only_links = FALSE,
           image_required = FALSE, image_width = NA){
    
    # Information can come from floraweb and/or from own resources
    
    # 1. Controls ----
    # Arguments
    if(!is.data.frame(species_row)){
      stop(".")
    }
    
    if(!all(hints_floraweb %in% c("map", "description", "status", "habitat",
                                  "family", "German name"))){
      stop('"hints_floraweb" must be a subset of c("map", "description",
           "status", "habitat", "family", "German name")')
    }
    
    if(!all(hints_ukplantatlas %in% c("mapuk", "familyuk", "ecology",
                                      "statusuk", "trends", "perennation",
                                      "lifeform", "woodiness", "clonality"))){
      stop('"hints_ukplantatlas" must be a subset of c("mapuk", "familyuk",
           "ecology", "statusuk", "trends", "perennation", "lifeform",
           "woodiness", "clonality")')
    }
    
    # nrow(species_row) == 1
    
    # all(hints_custom %in% colnames(species_row))
    
    # all(imagelinks_custom %in% colnames(species_row))

    # 2. Prep ----
    if(file_location == "temporary"){
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
    
    if(!is.na(species_row$NAMNR) & species_row$NAMNR != "" &
       (length(hints_floraweb)>0 | image_floraweb)){
      
      # Main infos
      # I need this step because of an error in RCURL when getting the url
      try({
        download.file(paste0(
          "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
          species_row$NAMNR, "&"), 
          destfile = file.path(dir,"main.txt"), quiet = TRUE)
        html_main <- htmlTreeParse(file = file.path(dir, "main.txt"),
                                   isURL = FALSE, isHTML = TRUE,
                                   useInternalNodes = TRUE)
        infos_main <- xpathApply(html_main, "//div[@id='content']//p",
                                 xmlValue)})
      
      if(image_floraweb & exists("html_main")){
        
        # download.file(
        #   paste0("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",
        #          species_row$NAMNR[i]),
        #   destfile = file.path(dir,"photo.txt"), quiet = TRUE)
        
        # Photo
        if(length(xpathApply(html_main, "//a[@class='imglink']", xmlAttrs)) > 0 & 
           grepl("foto\\.xsql",
                 xpathApply(html_main, "//a[@class='imglink']", xmlAttrs))[1]){
          
          download.file(
            paste0("https://www.floraweb.de/pflanzenarten/",
                   grep("foto\\.xsql",
                        xpathApply(html_main, "//a[@class='imglink']",
                                   xmlAttrs)[[1]], value = TRUE)), 
            destfile = file.path(dir, "photo.txt"), quiet = TRUE)
          html_photo <- htmlTreeParse(file = file.path(dir, "photo.txt"),
                                      isURL = FALSE, isHTML = TRUE,
                                      useInternalNodes = TRUE)
          infos_photo <- xpathApply(html_photo, "//div[@id='content']//p",
                                    xmlValue)
          # photolink <- xpathApply(html_photo, "//div[@id='content']//img",
          # xmlAttrs)[[1]][3]
          photolinks <- sapply(xpathApply(html_photo,
                                          "//div[@id='content']//img",
                                          xmlAttrs),
                               function(x) grep("bilder", x, value = TRUE))
          
          if(photolinks[1] != "../bilder/arten/"){
            if(only_links){
              hints[[1]][[1]] <- paste0("https://www.floraweb.de",
                                        gsub("\\.\\.", "", photolinks[1]))
              floraweb_image <- TRUE  
            } else {
              try({hints[[1]][[1]] <- load.image(
                paste0("https://www.floraweb.de",
                       gsub("\\.\\.", "", photolinks[1])))
              floraweb_image <- TRUE       
              }, silent = TRUE)
            }
            if (length(photolinks) > 1){ # check for cases with more than two images and put loop here
              if(only_links){
                hints[[1]][[2]] <- paste0("https://www.floraweb.de",
                                          gsub("\\.\\.", "",
                                               gsub("\\.tmb", "",
                                                    photolinks[2])))
              } else {
                try(hints[[1]][[2]] <- load.image(
                  paste0("https://www.floraweb.de",
                         gsub("\\.\\.", "", gsub("\\.tmb", "", photolinks[2])))),
                  silent = TRUE)
              }
            }
          }
        }
      }
    }
    
    # 3.2 Images from ukplantatlas ----
    
    if((length(hints_ukplantatlas) > 0) | image_ukplantatlas){
      try({
        species_main <- GET(paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                   gsub("[\\.\\(\\)]", "",
                                        gsub(" ", "-",
                                             tolower(species_row$SPECIES)))))
        species_main <- htmlTreeParse(file = species_main, isURL = FALSE,
                                      isHTML = TRUE, useInternalNodes = TRUE)
      })
      
      if(image_ukplantatlas & exists("species_main")){
        imagelinks <- xpathApply(species_main, "//img[@class='img-responsive']",
                                 xmlAttrs)
        
        if(length(imagelinks) > 0){
          imagelinks <- imagelinks[sapply(imagelinks,
                                          function(x) any(grepl("images", x)))]
          imagelinks <- sapply(imagelinks, function(x) x[[2]])
          imagelinks <- imagelinks[grepl("/medium/", imagelinks)]
          imagelinks <- gsub("(.*)(\\?itok.*)", "\\1", imagelinks)
          imagelinks <- gsub("/medium/", "/large/", imagelinks)
          
          if(length(imagelinks) > 0){
            # Check if medium sized image is available
            imagelinks_error <- sapply(imagelinks, http_error)
            imagelinks[imagelinks_error] <- gsub("/large/","/largest_1152_870/",
                                                 imagelinks[imagelinks_error])
            imagelinks_error <- sapply(imagelinks, http_error)
            imagelinks <- imagelinks[!imagelinks_error]
          }
        }
        
        if(length(imagelinks)>0){
          # image credits
          imagecredits <- unlist(
            xpathApply(species_main,
                       "//div[@class='views-field views-field-field-acknowledgement']",
                       xmlValue))
          imagecredits <- imagecredits[2:length(imagecredits)]
          imagecredits <- gsub(" +", " ", imagecredits)
          
          for(i in 1:length(imagelinks)){
            if(only_links){
              hints[[1]][[length(hints[[1]])+1]] <- imagelinks[i]
            } else {
              try({hints[[1]][[length(hints[[1]])+1]] <- load.image(imagelinks[i])
              }, silent = TRUE)
            }
          }
        }
      }
    }
    
    
    # 3.3 Images from own image link ----
    if(length(imagelinks_custom) > 0){
      for(i in 1:length(imagelinks_custom)){
        if(!is.na(species_row[, imagelinks_custom[i]]) &
           species_row[, imagelinks_custom[i]] != ""){
          if(only_links){
            hints[[1]][[length(hints[[1]])+1]] <-
              species_row[, imagelinks_custom[i]]
          } else {
            try({hints[[1]][[length(hints[[1]])+1]] <-
              load.image(species_row[, imagelinks_custom[i]])})
          }
        }
      }
    }
    
    # 3.4 Images from image folder ----
    if(length(image_folders) > 0){
      for(k in 1:length(image_folders)){
        image_files <- list.files(image_folders[k], pattern = "\\.jpg|\\.jpeg",
                                  recursive = TRUE, full.names = FALSE)
        image_files <- image_files[
          which(grepl(species, image_files) |
                  grepl(gsub("\\.", "", species), image_files) |
                  grepl(gsub(" ", "_", gsub("\\.", "",species)), image_files) |
                  grepl(gsub(" ", "_",species), image_files))]
        if(length(image_files) > 0){
          if(only_links){
            # WWW/image_folder in local shiny needs to be image_folder
            # ~/ShinyApps/BotanizeR/WWW/image_folder in server shiny needs to be image_folder
            for(i in 1:length(image_files)){
              hints[[1]][[length(hints[[1]])+1]] <-
                file.path(gsub(".*[wwwWWW]/(.+)$", ("\\1"), image_folders[k]),
                          image_files[i])
            }
          } else {
            for(i in 1:length(image_files)){
              try(hints[[1]][[length(hints[[1]])+1]] <-
                    load.image(file.path(image_folders[k], image_files[i])))
            }
          }
        }
      }
    }
    
    # 3.5 Image resize ----
    if(!is.na(image_width) & !is.null(image_width) & length(hints$images) > 0 &
       only_links == FALSE){
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
      hints_ukplantatlas <- NULL
      hints_custom <- NULL
    }
    
    # 4.1 Floraweb ----
    if(!is.na(species_row$NAMNR) & species_row$NAMNR != "" &
       length(hints_floraweb)>0) { 
      
      # ecology
      if("habitat" %in% hints_floraweb){
        try({download.file(
          paste0("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",
                 species_row$NAMNR, "&"), 
          destfile = file.path(dir,"ecology.txt"), quiet = TRUE)
          html_ecology <- htmlTreeParse(file = file.path(dir,"ecology.txt"),
                                        isURL = FALSE, isHTML = TRUE,
                                        useInternalNodes = TRUE)
          infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",
                                      xmlValue)
        })
      }
      
      # biology
      if("description" %in% hints_floraweb & floraweb_image == FALSE){
        try({download.file(
          paste0("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=",
                 species_row$NAMNR, "&"),
          destfile = file.path(dir, "biology.txt"), quiet = TRUE)
          html_biology <- htmlTreeParse(file = file.path(dir, "biology.txt"),
                                        isURL = FALSE, isHTML = TRUE,
                                        useInternalNodes = TRUE)
          infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",
                                      xmlValue)
        })
      }
      
      # Map
      map <- NA
      if("map" %in% hints_floraweb & exists("html_main")){
        if(length(xpathApply(html_main, "//a[@class='imglink']", xmlAttrs)) > 0 & 
           any(grepl("webkarten", xpathApply(html_main, "//a[@class='imglink']",
                                             xmlAttrs)))){
          
          if(!exists("CGRS_Germany")){
            data(CGRS_Germany)
          }
          
          try({
            taxon_ID_map <- gsub(
              "/webkarten/karte.html\\?taxnr=", "",
              grep(
                "webkarten",
                xpathApply(html_main, "//a[@class='imglink']",
                           xmlAttrs)[[which(grepl("webkarten",
                                                  xpathApply(
                                                    html_main,
                                                    "//a[@class='imglink']",
                                                    xmlAttrs)))]],
                value = TRUE))
            
            download.file(
              paste0("https://www.floraweb.de/pflanzenarten/download_afe.xsql?suchnr=",
                     taxon_ID_map),
              destfile = file.path(dir, "map.csv"), quiet = TRUE)
            map <- read.csv(
              file.path(dir, "map.csv"),
              skip = 41)[-1, c("CGRSNAME", "AFE_SYMBOLCODE", "AFESYMBOL_TEXT")]
            
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 4)] <- 2
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 5)] <- 3
            map$AFE_SYMBOLCODE[which(map$AFESYMBOL_TEXT == "cultivated, synanthrope, not established aliens)")] <- 4
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 6)] <- 5
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 8)] <- 6
            map$AFE_SYMBOLCODE <- map$AFE_SYMBOLCODE + 2
            
            map <- map[order(map$AFE_SYMBOLCODE, decreasing = TRUE),]
            map <- map[!duplicated(map$CGRSNAME), ]
            
            map <- merge(CGRS_Germany, map, all.x = TRUE, sort = FALSE)
            map$AFE_SYMBOLCODE[is.na(map$AFE_SYMBOLCODE)] <- 1
            map$AFE_SYMBOLCODE <- as.factor(map$AFE_SYMBOLCODE)
            
            legend_info <- data.frame(
              AFE_SYMBOLCODE = c(1:8), 
              SYMBOL_TEXT = c("absent", "not assigned", "records uncertain",
                              "extinct", "probably extinct",
                              "cultivated, not established alien",
                              "established alien",
                              "native, incl. archaeophytes"),
              colour = c("white", paste0("grey", c(90, 80, 70, 60)), "#fdb462",
                         "#fb8072", "#b3de69"),
              stringsAsFactors = FALSE) 
            
            legend_info <- legend_info[which(legend_info$AFE_SYMBOLCODE %in%
                                               map$AFE_SYMBOLCODE), ]
            
            levels(map$AFE_SYMBOLCODE) <- legend_info$SYMBOL_TEXT
          }, silent = TRUE)
        }
      }
      
      for (i in 1:length(hints_floraweb)){
        
        if(hints_floraweb[i] == "description" & (exists("infos_photo") |
                                                 exists("infos_biology"))){
          if(floraweb_image){
            description <- paste0(
              "Bestimmungshilfe/Morphologie:\n",
              infos_photo[[which(infos_photo == "Bestimmungshilfe:") + 1]])
          } else {
            description <- gsub("Morphologie:", "Morphologie:\n",
                                infos_biology[[2]])
          }
          if(!grepl("keine Angaben", description)){
            hints[[i+1]] <- description
          }
        }
        
        if(hints_floraweb[i] == "status" & exists("infos_main")){
          hints[[i+1]] <- paste0(infos_main[[7]],"; \n",infos_main[[8]])
        }
        
        if(hints_floraweb[i] == "family" & exists("infos_main")){
          hints[[i+1]] <- paste(infos_main[[6]])
        }
        
        if(hints_floraweb[i] == "German name" & exists("infos_main")){
          hints[[i+1]] <- paste(infos_main[[5]])
        }
        
        if(hints_floraweb[i] == "habitat" & exists("infos_ecology")){
          if(infos_ecology[[3]] !=
             "Formation: \r\nTaxon keiner Formation zugeordnet "){
            hints[[i+1]] <- paste(infos_ecology[[3]])
          }
        }
        
        if(hints_floraweb[i] == "map"){
          if(!is.na(map)[1]){
            hints[[i+1]] <- list(map["AFE_SYMBOLCODE"], legend_info$colour)
          }
        }
        
        if(length(hints)==i+1){
          names(hints)[i+1] <- hints_floraweb[i]
        }
      }
    }
    
    # 4.2 UK Plant Atlas ----
    if(length(hints_ukplantatlas) > 0 & exists("species_main")) { 
      
      infos <- xpathApply(species_main, "//div[@class='field-items']", xmlValue)
      
      # Map
      mapuk <- NA
      if("mapuk" %in% hints_ukplantatlas){
        try({mapuk_temp <- xpathApply(species_main,
                                      "//img[@class='img-responsive']",
                                      xmlAttrs)
        # mapuk <- unlist(mapuk[sapply(mapuk, function(x) any(grepl("\\.png", x)))])[1]
        mapuk_temp <- unlist(mapuk_temp[sapply(mapuk_temp,
                                               function(x) any(grepl("atlas_maps", x)))])[1]
        mapuk <- paste0("https://www.brc.ac.uk/plantatlas",
                        gsub("\\.\\.", "", mapuk_temp))
        if (!only_links){
          mapuk <- image_read(mapuk)
        }
        })
      }
      
      for (i in 1:length(hints_ukplantatlas)){
        
        if(hints_ukplantatlas[i] == "statusuk" & length(infos) >= 3){
          hints[[length(hints)+1]] <- paste("Status:", infos[[3]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "familyuk" & length(infos) >= 1){
          hints[[length(hints)+1]] <- paste("Family:",
                                            gsub("(.*›)(.*ceae|.*ferae)(›.*)",
                                                 "\\2", infos[[1]]))
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i]=="ecology" & length(infos) >= 2){
          hints[[length(hints)+1]] <- paste("Ecology:", infos[[2]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "trends" & length(infos) >= 4){
          hints[[length(hints)+1]] <- paste("Trends:", infos[[4]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "perennation" & length(infos) >= 21){
          hints[[length(hints)+1]] <- paste("Perennation:", infos[[21]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "lifeform" & length(infos) >= 22){
          hints[[length(hints)+1]] <- paste("Life form:", infos[[22]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "woodiness" & length(infos) >= 23){
          hints[[length(hints)+1]] <- paste("Woodiness:", infos[[23]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "clonality" & length(infos) >= 24){
          hints[[length(hints)+1]] <- paste("Clonality:", infos[[24]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "mapuk" & !is.na(mapuk)){
          hints[[length(hints)+1]] <- mapuk
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
      }
    }
    
    # 4.3 Hints from own entries ----
    if(length(hints_custom) > 0){
      for(i in 1:length(hints_custom)){
        if(!is.na(species_row[,hints_custom[i]]) &
           species_row[,hints_custom[i]] != ""){
          hints[[length(hints)+1]] <- paste0(
            gsub("_", " ", gsub("ownhint_", "", hints_custom[i])), ": ",
            species_row[,hints_custom[i]])
          names(hints)[length(hints)] <- hints_custom[i]
        }
      }
    }
    
    hints <- hints[which(!sapply(hints, is.null))]
    return(hints)
  } 
