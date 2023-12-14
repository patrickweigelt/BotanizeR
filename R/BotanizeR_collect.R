#' Collect images and information for BotanizeR quiz
#'
#' Collects information from [FloraWeb](https://www.floraweb.de) 
#' (images, map and species descriptions) and/or the [Online Atlas of the 
#' British Irish flora](https://www.brc.ac.uk/plantatlas/) as well as from user 
#' defined image folders and columns in the `species_list` data.frame to show 
#' them as hints in [BotanizeR::BotanizeR_quiz()] or the BotanizeR Shiny app.
#'
#' @param species_row a data.frame of one row including the species for which 
#' information shall be retrieved (Usually an entry of the `species_list` 
#' data.frame used in [BotanizeR::BotanizeR_quiz()] and the BotanizeR Shiny 
#' app. **It needs to contain at least the following columns**: 
#' *NAMNR* and *SPECIES*. The *SPECIES* column includes the species name 
#' (character or factor) to be looked up in the online resources or image 
#' folders. *NAMNR* contains the ID (numeric) of the species used by 
#' [FloraWeb](https://www.floraweb.de). In case FloraWeb content is not 
#' retrieved or not available for a given species, this may be NA.
#'
#' @param image_floraweb logical that defines if images from
#' [FloraWeb](https://www.floraweb.de) shall be retrieved.
#'
#' @param hints_floraweb character vector defining the hints to 
#' retrieve from [FloraWeb](https://www.floraweb.de). 'hints_floraweb' 
#' must be either NULL or a character string with the wanted hints from 
#' c('map', 'description', 'status', 'habitat', 'family', 'German name').
#'
#' @param image_ukplantatlas logical that defines if images from the
#' [Online Atlas of the British 
#' and Irish flora](https://www.brc.ac.uk/plantatlas/) shall be retrieved.
#'
#' @param hints_ukplantatlas character vector defining the hints to 
#' retrieve from the [Online Atlas of the British and Irish 
#' flora](https://www.brc.ac.uk/plantatlas/). 'hints_ukplantatlas' 
#' must be either NULL or a character string with the wanted hints from 
#' c('mapuk', 'familyuk', 'ecology', 'statusuk', 'trends', 'perennation', 
#' 'lifeform', 'woodiness', 'clonality').
#'
#' @param imagelinks_custom character vector defining columns of `species_row` 
#' containing links (URLs) to retrieve images from the internet. For using it
#' inside the BotanizeR Shiny App these columns need to be named like
#' c("imagelink_1", "imagelink_2") etc.
#'
#' @param image_folders character vector defining folders from
#' which to retrieve images. Image file names need to contain the species names 
#' to be found.
#' 
#' @param hints_custom character vector defining custom hints to use. 
#' **Note:** In that case, these hints should be stored in `species_row` in 
#' additional columns named like ownhint_*HintName* where *HintName* should be 
#' different than the hints allowed for `hints_ukplantatlas` and 
#' `hints_floraweb`.
#' 
#' @param file_location character vector defining a location to temporarily 
#' store the images retrieved from online resources. If put to "temporary", R 
#' will create a temporary folder automatically.
#' 
#' @param only_links logical, if `TRUE`, then all images found will not be 
#' loaded but links will be returned. Set to `TRUE` for shiny app.
#' 
#' @param image_required logical indicating whether additional hints shall only 
#' be retrieved if at least one image for the given species is available. Set 
#' to `TRUE` for [BotanizeR::BotanizeR_quiz()].
#' 
#' @param image_width numeric defining to what width of the images shall be 
#' rescaled in case `only_links` is `FALSE`and `image_width` is not `NA`.
#' 
#' @return
#' list of named elements including (for the selected species) the hints as 
#' characters and images and maps either as link or image or spatial object.
#'
#'
#' @details This function provides the information shown in 
#' [BotanizeR::BotanizeR_quiz()] and the BotanizeR Shiny app. Have a look into 
#' the [BotanizeR 
#' tutorials](https://patrickweigelt.github.io/BotanizeR/index.html) 
#' on how to use the quiz and the shiny app to learn more about its usage.
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R package with Shiny app to practice plant identification for 
#'     online teaching and beyond. PLANTS, PEOPLE, PLANET, 
#'     https://doi.org/10.1002/ppp3.10226.
#'
#' @seealso [BotanizeR::BotanizeR_quiz()] and [BotanizeR::BotanizeR_shiny()]
#'
#' @examples
#' # Load species list for Britain and Germany with species and IDs from 
#' # https://www.floraweb.de and https://www.brc.ac.uk/plantatlas/
#' data(BotanizeR_species)
#' 
#' # Select Acer campestre
#' species_row = BotanizeR_species[which(BotanizeR_species$SPECIES ==
#'                                         "Acer campestre"),]
#' 
#' 
#' # only ukplantatlas image(s) + hints + map
#' hints <- BotanizeR_collect(species_row, image_ukplantatlas = TRUE, 
#'                            hints_ukplantatlas = c("mapuk", "familyuk", 
#'                                                  "ecology", "statusuk", 
#'                                                  "trends", "perennation",
#'                                                  "lifeform", "woodiness", 
#'                                                  "clonality"))
#' 
#' par(mar = rep(0.5, 4), oma = rep(0, 4))
#' plot(hints$image[[1]], axes = FALSE)
#' plot(hints$image[[2]], axes = FALSE)
#' 
#' plot(hints$mapuk)
#' 
#' hints$statusuk
#' hints$clonality
#' 
#' 
#' # only floraweb image(s) + hints + map
#' hints <- BotanizeR_collect(species_row, image_floraweb = TRUE, 
#'                            hints_floraweb = c("map", "description", 
#'                                               "status", "habitat", 
#'                                               "family", "German name"))
#' 
#' \dontrun{
#' par(oma = c(0, 0, 0, 10.5))
#' plot(hints$map[[1]], pal = hints$map[[2]], key.pos = 4, main = "")
#' }
#' 
#' plot(hints$image[[1]], axes = FALSE)
#' 
#' hints$family
#' 
#' 
#' # only images from custom image links + custom hints
#' hints <- BotanizeR_collect(species_row, image_floraweb = FALSE,
#'                            hints_custom = c("ownhint_English_name", 
#'                                             "ownhint_Description", 
#'                                             "ownhint_Distribution"),
#'                            imagelinks_custom = c("imagelink_1", 
#'                                                  "imagelink_2"))
#' 
#' plot(hints$image[[1]], axes = FALSE)
#' plot(hints$image[[2]], axes = FALSE)
#' 
#' hints$ownhint_English_name
#' 
#' 
#' # only returning image links instead of actual images
#' hints <- BotanizeR_collect(species_row, image_floraweb = TRUE,
#'                            image_ukplantatlas = TRUE, 
#'                            imagelinks_custom = c("imagelink_1", 
#'                                                  "imagelink_2"),
#'                            hints_ukplantatlas = c("mapuk", "familyuk"),
#'                            only_links = TRUE)
#' hints
#' 
#' # retrieving nothing
#' BotanizeR_collect(species_row)
#' 
#' # To load images from your local computer, specify an image folder with
#' # pictures included. File names need to include the species names.
#' 
#' \dontrun{
#' hints <- BotanizeR_collect(species_row, 
#'                            image_folders = c("images/Asteraceae_Britain",
#'                                              "images/Trees"))
#' }
#' 
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' 
#' @export

BotanizeR_collect <-
  function(species_row, image_floraweb = FALSE, hints_floraweb = NULL,
           image_ukplantatlas = FALSE, hints_ukplantatlas = NULL,
           imagelinks_custom = NULL, image_folders = NULL, hints_custom = NULL,
           file_location = "temporary", only_links = FALSE,
           image_required = FALSE, image_width = NA){
    
    # Information can come from floraweb and/or from own resources
    
    # 1. Controls ----
    # Arguments
    if(!is.data.frame(species_row)){
      stop("species_row must be a data.frame of one row including the species 
      for which information shall be retrieved. It should contain at least 
      the following columns: 'NAMNR' and 'SPECIES'.")
    }
    
    if(nrow(species_row) !=1) {
      stop("species_row must be a data.frame of one row including the species 
        for which information shall be retrieved. It should contain at least 
        the following columns: 'NAMNR' and 'SPECIES'.")
    } 
    
    if(!all(c('NAMNR','SPECIES') %in% colnames(species_row))) {
      stop("species_row must be a data.frame of one row including the species 
        for which information shall be retrieved. It should contain at least 
        the following columns: 'NAMNR' and 'SPECIES'.")
    } 
    
    if(!is.character(species_row$SPECIES) & !is.factor(species_row$SPECIES)) {
      stop("species_row must be a data.frame of one row including the species 
        for which information shall be retrieved. It should contain at least 
        the following columns: 'NAMNR' and 'SPECIES'. The *SPECIES* column has 
        to include the species name (character or factor) to be looked up in 
        the online resources or image folders.")
    } 

    if(!is.logical(image_floraweb)){
      stop("'image_floraweb' must be a logical defining if images from
      https://www.floraweb.de shall be retrieved")
    }
    
    if(!is.null(hints_floraweb)){
      if(!is.character(hints_floraweb) | 
         !all(hints_floraweb %in% c("map", "description", "status", "habitat",
                                    "family", "German name"))){
        stop("'hints_floraweb' must be either NULL or a character string 
        defining the hints to retrieve from https://www.floraweb.de: 
        c('map', 'description', 'status', 'habitat', 'family', 'German name').")
      }
    }

    if(!is.logical(image_ukplantatlas)){
      stop("'image_ukplantatlas' must be a logical defining if images from
      https://www.brc.ac.uk/plantatlas/ shall be retrieved.")
    }
    
    if(!is.null(hints_ukplantatlas)){
      if(!is.character(hints_ukplantatlas) |
         !all(hints_ukplantatlas %in% c("mapuk", "familyuk", "ecology",
                                        "statusuk", "trends", "perennation",
                                        "lifeform", "woodiness", "clonality"))){
        stop("'hints_ukplantatlas' must be either NULL or a character string 
             defining the hints to retrieve from 
             https://www.brc.ac.uk/plantatlas/: c('mapuk', 'familyuk', 
             'ecology', 'statusuk', 'trends', 'perennation', 'lifeform', 
             'woodiness', 'clonality').")
      }
    }
    
    if(!is.null(imagelinks_custom)){
      if(!is.character(imagelinks_custom)){
        stop("'imagelinks_custom' must be either NULL or a character string
        defining columns of 'species_row' containing URLs to retrieve images 
        from the internet.")
      }
    }
    
    if(!all(imagelinks_custom %in% colnames(species_row))){
      stop('"imagelinks_custom" must all be present in the column names of
           "species_row".')
    }
    
    if(!is.null(image_folders)){
      if(!is.character(image_folders)){
        stop("'image_folders' must be a character vector defining a specific
        folder from which to retrieve images.")
      }
    }
    
    if(!is.null(hints_custom)){
      if(!is.character(hints_custom)){
        stop("'hints_custom' must be either NULL or a character string
        defining columns of 'species_row' containing custom hints.")
      }
    }
    
    if(!all(hints_custom %in% colnames(species_row))){
      stop('"hints_custom" must all be present in the column names of
           "species_row".')
    }
    
    if(!is.character(file_location)){
      stop("'file_location' must be 'temporary' or a character vector that 
      defines a temporary folder location where to store images.")
    }
    
    if(!is.logical(only_links)){
      stop("'only_links' must be a logical defining whether images should
           be loaded into the R session or links to the images should be 
           returned.")
    }
    
    if(!is.logical(image_required)){
      stop("'image_required' must be a logical.")
    }
    
    if(!is.na(image_width)){
      if(!is.numeric(image_width)){
        stop("'image_width' must be either NA or a numeric defining to what 
        width of the images shall be rescaled if larger.")
      }
    }
    
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
          # "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
          "https://www.floraweb.de/xsql/artenhome.xsql?suchnr=",
          species_row$NAMNR, "&"), 
          destfile = file.path(dir,"main.txt"), quiet = TRUE)
        html_main <- XML::htmlTreeParse(file = file.path(dir, "main.txt"),
                                        isURL = FALSE, isHTML = TRUE,
                                        useInternalNodes = TRUE)
        # infos_main <- XML::xpathApply(html_main, "//div[@id='content']//p",
        #                               XML::xmlValue)})
        # infos_main <- XML::xpathApply(html_main, "//section//p",
        #                               XML::xmlValue)
        infos_main <- XML::xpathApply(html_main, "//li[@class='artenliste-portrait']",
                                    XML::xmlValue)
      })

      if(image_floraweb & exists("html_main")){
        
        # download.file(
        #   paste0("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",
        #          species_row$NAMNR[i]),
        #   destfile = file.path(dir,"photo.txt"), quiet = TRUE)
        
        # Photo
        if(length(XML::xpathApply(html_main, "//a[@class='imglink']",
                                  XML::xmlAttrs)) > 0 & 
           grepl("foto\\.xsql",
                 XML::xpathApply(html_main, "//a[@class='imglink']",
                                 XML::xmlAttrs))[1]){
          
          download.file(
            # paste0("https://www.floraweb.de/pflanzenarten/",
            paste0("https://www.floraweb.de",
                   grep("foto\\.xsql",
                        XML::xpathApply(html_main, "//a[@class='imglink']",
                                        XML::xmlAttrs)[[1]], value = TRUE)), 
            destfile = file.path(dir, "photo.txt"), quiet = TRUE)
          html_photo <- XML::htmlTreeParse(file = file.path(dir, "photo.txt"),
                                           isURL = FALSE, isHTML = TRUE,
                                           useInternalNodes = TRUE)
          # infos_photo <- XML::xpathApply(html_photo,
          #                                "//div[@id='content']//p",
          #                                XML::xmlValue)
          infos_photo <- XML::xpathApply(html_photo, "//section//p",
                                          XML::xmlValue)
          # infos_photo <- XML::xpathApply(html_photo, "//a[@class='glossar']",
          #                                XML::xmlValue)
          # photolink <- XML::xpathApply(html_photo,
          #                              "//div[@id='content']//img",
          #                              XML::xmlAttrs)[[1]][3]
          photolinks <- grep("bilder",
                             unlist(
                               XML::xpathApply(html_photo, "//section//img",
                                               XML::xmlAttrs)),
                             value = TRUE)
          
          if(length(photolinks) > 0 & photolinks[1] != "/bilder/arten/"){
            if(only_links){
              hints[[1]][[1]] <- paste0("https://www.floraweb.de",
                                        gsub("\\.\\.", "", photolinks[1]))
              floraweb_image <- TRUE  
            } else {
              try({hints[[1]][[1]] <- imager::load.image(
                paste0("https://www.floraweb.de",
                       gsub("\\.\\.", "", photolinks[1])))
              floraweb_image <- TRUE       
              }, silent = TRUE)
            }
            if (length(photolinks) > 1){ # check for cases with more than two
              # images and put loop here
              if(only_links){
                hints[[1]][[2]] <- paste0("https://www.floraweb.de",
                                          gsub("\\.\\.", "",
                                               gsub("\\.tmb", "",
                                                    photolinks[2])))
              } else {
                try(hints[[1]][[2]] <- imager::load.image(
                  paste0("https://www.floraweb.de",
                         gsub("\\.\\.", "", gsub("\\.tmb", "",
                                                 photolinks[2])))),
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
        species_main <- httr::GET(paste0(
          "https://www.brc.ac.uk/plantatlas/plant/",
          gsub("[\\.\\(\\)]", "",
               gsub(" ", "-",
                    tolower(species_row$SPECIES)))))
        species_main <- XML::htmlTreeParse(file = species_main, isURL = FALSE,
                                           isHTML = TRUE,
                                           useInternalNodes = TRUE)
      })
      
      if(image_ukplantatlas & exists("species_main")){
        imagelinks <- XML::xpathApply(species_main,
                                      "//img[@class='img-responsive']",
                                      XML::xmlAttrs)
        
        if(length(imagelinks) > 0){
          imagelinks <- grep("images", unlist(imagelinks), value = TRUE)
          imagelinks <- imagelinks[grepl("/medium/", imagelinks)]
          imagelinks <- gsub("(.*)(\\?itok.*)", "\\1", imagelinks)
          imagelinks <- gsub("/medium/", "/large/", imagelinks)
          
          if(length(imagelinks) > 0){
            # Check if medium sized image is available
            imagelinks_error <- vapply(imagelinks, httr::http_error, 
                                       FUN.VALUE = TRUE)
            imagelinks[imagelinks_error] <- gsub("/large/",
                                                 "/largest_1152_870/",
                                                 imagelinks[imagelinks_error])
            imagelinks_error <- vapply(imagelinks, httr::http_error, 
                                       FUN.VALUE = TRUE)
            imagelinks <- imagelinks[!imagelinks_error]
          }
        }
        
        if(length(imagelinks)>0){
          # image credits
          imagecredits <- unlist(
            XML::xpathApply(
              species_main,
              "//div[@class='views-field views-field-field-acknowledgement']",
              XML::xmlValue))
          imagecredits <- imagecredits[2:length(imagecredits)]
          imagecredits <- gsub(" +", " ", imagecredits)
          
          for(i in seq_along(imagelinks)){
            if(only_links){
              hints[[1]][[length(hints[[1]])+1]] <- imagelinks[i]
            } else {
              try({hints[[1]][[length(hints[[1]])+1]] <-
                imager::load.image(imagelinks[i])
              }, silent = TRUE)
            }
          }
        }
      }
    }
    
    # 3.3 Images from own image link ----
    if(length(imagelinks_custom) > 0){
      for(i in seq_along(imagelinks_custom)){
        if(!is.na(species_row[, imagelinks_custom[i]]) &
           species_row[, imagelinks_custom[i]] != ""){
          if(only_links){
            hints[[1]][[length(hints[[1]])+1]] <-
              species_row[, imagelinks_custom[i]]
          } else {
            try({hints[[1]][[length(hints[[1]])+1]] <-
              imager::load.image(species_row[, imagelinks_custom[i]])})
          }
        }
      }
    }
    
    # 3.4 Images from image folder ----
    if(length(image_folders) > 0){
      for(k in seq_along(image_folders)){
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
            # ~/ShinyApps/BotanizeR/WWW/image_folder in server shiny needs to
            # be image_folder
            for(i in seq_along(image_files)){
              hints[[1]][[length(hints[[1]])+1]] <-
                file.path(gsub(".*[wwwWWW]/(.+)$", ("\\1"), image_folders[k]),
                          image_files[i])
            }
          } else {
            for(i in seq_along(image_files)){
              try(hints[[1]][[length(hints[[1]])+1]] <-
                    imager::load.image(file.path(image_folders[k],
                                                 image_files[i])))
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
          imager::resize(x, size_x = image_width,
                         size_y = image_width/nrow(x)*ncol(x))
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
          paste0("https://www.floraweb.de/xsql/oekologie.xsql?suchnr=",
                 species_row$NAMNR, "&"), 
          destfile = file.path(dir,"ecology.txt"), quiet = TRUE)
          html_ecology <- XML::htmlTreeParse(file = file.path(dir,
                                                              "ecology.txt"),
                                             isURL = FALSE, isHTML = TRUE,
                                             useInternalNodes = TRUE)
          infos_ecology <- XML::xpathApply(html_ecology,
                                           "//section//p",
                                           XML::xmlValue)
        })
      }
      
      # biology
      if("description" %in% hints_floraweb & floraweb_image == FALSE){
        try({download.file(
          paste0("https://www.floraweb.de/xsql/biologie.xsql?suchnr=",
                 species_row$NAMNR, "&"),
          destfile = file.path(dir, "biology.txt"), quiet = TRUE)
          html_biology <- XML::htmlTreeParse(file = file.path(dir,
                                                              "biology.txt"),
                                             isURL = FALSE, isHTML = TRUE,
                                             useInternalNodes = TRUE)
          infos_biology <- XML::xpathApply(html_biology,
                                           "//section//p",
                                           XML::xmlValue)
        })
      }
      
      # Map
      map <- NA
      if("map" %in% hints_floraweb & exists("html_main")){
        if(length(XML::xpathApply(html_main,
                                  "//a[@class='imglink']",
                                  XML::xmlAttrs)) > 0 & 
           any(grepl("webkarten", XML::xpathApply(html_main,
                                                  "//a[@class='imglink']",
                                                  XML::xmlAttrs)))){
          
          if(!exists("CGRS_Germany")){
            data(CGRS_Germany)
          }
          
          try({
            taxon_ID_map <- gsub(
              "/webkarten/karte.html\\?taxnr=", "",
              grep(
                "webkarten",
                XML::xpathApply(
                  html_main, "//a[@class='imglink']",
                  XML::xmlAttrs)[[which(grepl("webkarten",
                                              XML::xpathApply(
                                                html_main,
                                                "//a[@class='imglink']",
                                                XML::xmlAttrs)))]],
                value = TRUE))
            
            download.file(
              paste0(
            "https://www.floraweb.de/pflanzenarten/download_afe.xsql?suchnr=",
                taxon_ID_map),
              destfile = file.path(dir, "map.csv"), quiet = TRUE)
            map <- read.csv(
              file.path(dir, "map.csv"),
              skip = 41)[-1, c("CGRSNAME", "AFE_SYMBOLCODE", "AFESYMBOL_TEXT")]
            
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 4)] <- 2
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 5)] <- 3
            map$AFE_SYMBOLCODE[
              which(map$AFESYMBOL_TEXT ==
                      "cultivated, synanthrope, not established aliens)")] <- 4
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 6)] <- 5
            map$AFE_SYMBOLCODE[which(map$AFE_SYMBOLCODE == 8)] <- 6
            map$AFE_SYMBOLCODE <- map$AFE_SYMBOLCODE + 2
            
            map <- map[order(map$AFE_SYMBOLCODE, decreasing = TRUE), ]
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
      
      for (i in seq_along(hints_floraweb)){
        
        if(hints_floraweb[i] == "description" & (exists("infos_photo") |
                                                 exists("infos_biology"))){
          if(floraweb_image){
            description <- paste0(
              "Bestimmungshilfe/Morphologie:\n",
              infos_photo[[1]])
          } else {
            description <- gsub("Morphologie:", "Morphologie:\n",
                                infos_biology[[2]])
          }
          if(!grepl("keine Angaben", description)){
            hints[[i+1]] <- description
          }
        }
        
        if(hints_floraweb[i] == "status" & exists("infos_main")){
          hints[[i+1]] <- gsub(" +"," ",
                               gsub("; \\n","",
                                    paste0(infos_main[[4]], 
                                           "; \n",infos_main[[5]])))
        }
        
        if(hints_floraweb[i] == "family" & exists("infos_main")){
          hints[[i+1]] <- paste(infos_main[[3]])
        }
        
        if(hints_floraweb[i] == "German name" & exists("infos_main")){
          hints[[i+1]] <- paste(infos_main[[2]])
        }
        
        if(hints_floraweb[i] == "habitat" & exists("infos_ecology")){
          if(infos_ecology[[2]] !=
             "Formation: \r\nTaxon keiner Formation zugeordnet "){
            hints[[i+1]] <- paste(infos_ecology[[2]])
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
      
      infos <- XML::xpathApply(species_main, "//div[@class='field-items']",
                               XML::xmlValue)
      
      # Map
      mapuk <- NULL
      if("mapuk" %in% hints_ukplantatlas){
        try({mapuk_temp <- XML::xpathApply(species_main,
                                           "//img[@class='img-responsive']",
                                           XML::xmlAttrs)
        mapuk_temp <- grep("atlas_maps", unlist(mapuk_temp), value = TRUE)[1]
        mapuk <- paste0("https://www.brc.ac.uk/plantatlas",
                        gsub("\\.\\.", "", mapuk_temp))
        if (!only_links){
          mapuk <- magick::image_read(mapuk)
        }
        })
      }
      
      for (i in seq_along(hints_ukplantatlas)){
        
        if(hints_ukplantatlas[i] == "statusuk" & length(infos) >= 3){
          hints[[length(hints)+1]] <- paste("Status:", infos[[3]])
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
        
        if(hints_ukplantatlas[i] == "familyuk" & length(infos) >= 1){
          hints[[length(hints)+1]] <- 
            paste("Family:",
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
        
        if(hints_ukplantatlas[i] == "mapuk" & !is.null(mapuk)){
          hints[[length(hints)+1]] <- mapuk
          names(hints)[length(hints)] <- hints_ukplantatlas[i]
        }
      }
    }
    
    # 4.3 Hints from own entries ----
    if(length(hints_custom) > 0){
      for(i in seq_along(hints_custom)){
        if(!is.na(species_row[,hints_custom[i]]) &
           species_row[,hints_custom[i]] != ""){
          hints[[length(hints)+1]] <- paste0(
            gsub("_", " ", gsub("ownhint_", "", hints_custom[i])), ": ",
            species_row[,hints_custom[i]])
          names(hints)[length(hints)] <- hints_custom[i]
        }
      }
    }
    
    hints <- hints[which(!vapply(hints, is.null, FUN.VALUE = TRUE))]
    return(hints) # end
  } 
