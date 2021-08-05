#' BotanizeR quiz
#'
#' Navigates randomly through a species list, shows defined 
#' pictures and descriptions and other hints and let's the user guess the
#' species name. Number of tries and attempts are used to calculate scores that
#' will be used to update probabilities for the random sampling of subsequent
#' species.
#'
#'
#' @param species_list a data.frame including the species that shall be 
#' practiced and which will be retrieved information for using 
#' [BotanizeR::BotanizeR_collect()] 
#' **It needs to contain at least the following columns**: 
#' *NAMNR*, *TAXONNAME*, *SPECIES* and *GENUS*. The *SPECIES* column includes 
#' the species name (genus and epithet; character or factor) to be guessed and 
#' looked up in the online resources or image folders. The *TAXONNAME* column 
#' includes the full species name including additional information like, for 
#' example the authority (character or factor). The *GENUS* column includes the 
#' corresponding genus name (character or factor). *NAMNR* contains the ID 
#' (numeric) of the species used by [FloraWeb](https://www.floraweb.de). In 
#' case FloraWeb content is not retrieved, this may be NA.
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
#' @param imagelinks_custom character vector defining columns of `species_list` 
#' containing links (URLs) to retrieve images from the internet. These columns 
#' need to be available in `species_list`.
#'
#' @param image_folders character vector defining folders from
#' which to retrieve images. Image file names need to contain the species names 
#' to be found.
#' 
#' @param hints_custom character vector defining custom hints to use. 
#' **Note:** In that case, these hints should be stored in `species_list` in 
#' additional columns named like ownhint_*HintName* where *HintName* should be 
#' different than the hints allowed for `hints_ukplantatlas` and 
#' `hints_floraweb`.
#' 
#' @param case_sensitive logical indicating whether cases need to match when 
#' guessing a species name.
#' 
#' @param file_location character vector defining a location to temporarily 
#' store the images retrieved from online resources. If put to "temporary", R 
#' will create a temporary folder automatically.
#' 
#' @param startat numeric indicating how many species have already been 
#' practiced in this session. Should be 0 when running the function and will be 
#' consecutively increased when the function calls itself.
#' 
#' @param init_count numeric indicating how many times overall species have 
#' already been practiced. If set to `NA`, the function takes the sum of the 
#' column `$COUNT` in `species_list`.
#' 
#' @param init_score numeric indicating how many times overall species have 
#' been named correctly. If set to `NA`, the function takes the sum of the
#'  column `$SCORE` in `species_list`.
#' 
#' @param init_attempts numeric indicating how many attempts have been used 
#' overall across all species so far. If set to `NA`, the function takes the 
#' sum of the column `$ATTEMPTS` in `species_list`.
#' 
#' @param max_attempts numeric defining the number of attempts allowed per 
#' species before moving on to next species
#' 
#' @param image_width numeric defining to what width of the images shall be 
#' rescaled before plotting in case `image_width` is not `NA`.
#' 
#' @return
#' A data.frame like `species_list` with updated scores and counts per species.
#'
#' @details After running the main function BotanizeR_quiz() you need to click
#' into the console to enter the species name in there. If you have no clue,
#' press enter and the next hint will appear (you have ten tries). If you type
#' the name slightly wrong the function will tell you. If the genus is correct
#' it will also tell you. It shows several photos if available. If you want to
#' skip a species write "skip". If you want to cancel the quiz write "exit".
#' Don't hit Esc if you want to save your progress. The function counts your
#' attempts and successes and uses this as sampling probabilities for choosing
#' the species randomly. The better you know a species the less likely it will
#' be shown again.
#'
#' @references
#'     Weigelt, P., Denelle, P., Brambach, F. & Kreft, H. (2021) A flexible
#'     R-package with Shiny-App for practicing plant identification in times of
#'     online teaching and beyond. submitted.
#'
#' @seealso [BotanizeR::BotanizeR_collect()]
#'
#' @examples
#' # Load species list for UK, Ireland and Germany based on floraweb.de and the 
#' # Online Atlas of the British and Irish flora 
#' # (https://www.brc.ac.uk/plantatlas/)
#' data(BotanizeR_species)
#' 
#' 
#' # Subset for about 700 species known from Sussex, UK
#' BotanizeR_Sussex <- 
#'   BotanizeR_species[which(BotanizeR_species$UK_Ireland_Sussex==1), ]
#' 
#' 
#' \dontrun{
#' # Type in species name, or press enter for next hint or type "skip" and
#' # press enter for next species or type "exit" to end quiz and save results
#' BotanizeR_Sussex_practiced <- 
#'   BotanizeR_quiz(species_list = BotanizeR_Sussex,
#'                  image_floraweb = FALSE, image_ukplantatlas = TRUE, 
#'                  hints_ukplantatlas =  c('mapuk', 'familyuk', 'ecology', 
#'                                          'statusuk', 'trends', 'perennation',
#'                                          'lifeform', 'woodiness', 
#'                                          'clonality'), 
#'                  hints_floraweb = NULL, 
#'                  case_sensitive = FALSE)
#' 
#' 
#' # Subset for about 300 species students in Goettingen, Germany, learn
#' BotanizeR_Germany <- 
#'   BotanizeR_species[which(BotanizeR_species$Germany_summer==1 | 
#'                             BotanizeR_species$Germany_BioDiv==1), ]
#' 
#' 
#' # Type in species name, or press enter for next hint or type "skip" and
#' # press enter for next species or type "exit" to end quiz and save results
#' BotanizeR_Germany_practiced <- 
#'   BotanizeR_quiz(species_list = BotanizeR_Germany, 
#'                  hints_floraweb = c("description", "status",
#'                                     "habitat","family","German name"), 
#'                  case_sensitive = FALSE)
#' 
#' 
#' # If you want to include distribution maps as hints add "map" to hints;
#' # This increases the download times a bit
#' BotanizeR_Germany_practiced <- BotanizeR_quiz(
#'   species_list = BotanizeR_Germany,
#'   hints_floraweb = c("map","description","status", "habitat", "family",
#'                      "German name"), case_sensitive = FALSE)
#' 
#' 
#' # If you want to keep track of your progress, you can save the species list
#' # with updated scores locally and load it in the next session
#' 
#' 
#' ### example for three species with custom hints and images
#' BotanizeR_Germany_custom <-  BotanizeR_Germany_practiced[
#'   which(BotanizeR_Germany_practiced$SPECIES %in% c("Acer campestre", 
#'                                                    "Erica carnea",
#'                                                    "Melampyrum nemorosum")),]
#' 
#' custom_species_trained <- BotanizeR_quiz(
#'   species_list = BotanizeR_Germany_custom, image_floraweb = TRUE,
#'   hints_floraweb = NULL, hints_custom = c("ownhint_Description", 
#'                                           "ownhint_Distribution"), 
#'   imagelinks_custom = c("imagelink_1", "imagelink_2"), 
#'   image_folders = NULL, case_sensitive = FALSE)
#' }
#' 
#' @export

BotanizeR_quiz <- function(
  species_list, image_floraweb = TRUE, 
  hints_floraweb = c("description", "status", "habitat", "family",
                     "German name"),
  image_ukplantatlas = FALSE, hints_ukplantatlas = NULL,
  imagelinks_custom = NULL, image_folders = NULL, hints_custom = NULL,
  case_sensitive = TRUE, file_location = "temporary", startat = 0,
  init_count = NA, init_score = NA, init_attempts = NA, 
  max_attempts = 10, image_width = 500){
  
  # 1. Controls ----
  # Arguments
  if(!is.data.frame(species_list)){
    stop("species_list must be a data.frame including the species that shall be 
        practiced and which will be retrieved information for. It should 
        contain at least the following columns: 'NAMNR', 'TAXONNAME', 'SPECIES' 
        and 'GENUS'.")
  }
  
  if(!all(c('TAXONNAME', 'SPECIES', 'GENUS') %in% colnames(species_list))) {
    stop("species_list must be a data.frame including the species that shall be 
        practiced and which will be retrieved information for. It should 
        contain at least the following columns: 'NAMNR', 'TAXONNAME', 'SPECIES' 
        and 'GENUS'.")
  } 
  
  if((!is.character(species_list$TAXONNAME) & 
      !is.factor(species_list$TAXONNAME))
     | (!is.character(species_list$SPECIES) & 
        !is.factor(species_list$SPECIES))
     | (!is.character(species_list$GENUS) & 
        !is.factor(species_list$GENUS))) {
    stop("One of the columns 'TAXONNAME', 'SPECIES' and 'GENUS' is not 
         a character of factor vector.")
  } 
  
  if(nrow(species_list)==0){
    stop("No entries in species_list found!")
  }
  
  if(!all(apply(species_list[,c('TAXONNAME','SPECIES','GENUS')], 2,
               function(x) all(!is.na(x) & x != "")))){
    stop("Missing entries in at least one of the columns
                                 'TAXONNAME', 'SPECIES' and 'GENUS'.")
  }
  
  if(length(which(duplicated(species_list$SPECIES))) > 0){
    stop("Duplicates in 'SPECIES' column found.")
    
  }

  if(!"NAMNR" %in% names(species_list))
    species_list$NAMNR <- NA
  if(!"COUNT" %in% names(species_list))
    species_list$COUNT <- 0
  if(!"SCORE" %in% names(species_list))
    species_list$SCORE <- 0
  if(!"ATTEMPTS" %in% names(species_list))
    species_list$ATTEMPTS <- 0
  if(!"INCLUDE" %in% names(species_list))
    species_list$INCLUDE <- 1
  
  if(!all(apply(species_list[,c('COUNT','SCORE','ATTEMPTS','INCLUDE')], 2, 
               function(x) is.numeric(x) & all(!is.na(x))))){
    stop("Not all entries of the columns 'COUNT', 'SCORE', 'ATTEMPTS' and 
    'INCLUDE' are numeric.")
  }
  
  if(sum(species_list$INCLUDE) == 0){
    stop("No species indicated as included (species_list$INCLUDE).")
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
  
  if(image_floraweb | !is.null(hints_floraweb)){
    if(all(is.na(species_list$NAMNR) | species_list$NAMNR == "")){
      stop("No FloraWeb IDs in species_list$NAMNR given.")
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
        defining columns of 'species_list' containing URLs to retrieve images 
        from the internet.")
    }
  }
  
  if(!all(imagelinks_custom %in% colnames(species_list))){
    stop('"imagelinks_custom" must all be present in the column names of
           "species_list".')
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
        defining columns of 'species_list' containing custom hints.")
    }
  }
  
  if(!all(hints_custom %in% colnames(species_list))){
    stop('"hints_custom" must all be present in the column names of
           "species_list".')
  }
  
  if(!is.logical(case_sensitive)){
    stop("'case_sensitive' must be logical.")
  }
  
  if(!is.character(file_location)){
    stop("'file_location' must be 'temporary' or a character vector that 
      defines a temporary folder location where to store images.")
  }
  
  if(!is.numeric(startat)){
    stop("'startat' must numeric")
  }
  
  if(!is.na(init_count)){
    if(!is.numeric(init_count)){
      stop("'init_count' must be numeric")
    }
  } else {
    init_count <- sum(species_list$COUNT)
  }

  if(!is.na(init_score)){  
    if(!is.numeric(init_score)){
      stop("'init_score' must be numeric")
    }
  } else {
    init_score <- sum(species_list$SCORE)
  }
  
  if(!is.na(init_attempts)){
    if(!is.numeric(init_attempts)){
      stop("'init_attempts' must be numeric")
    }
  } else {
    init_attempts <- sum(species_list$ATTEMPTS)
  }
  
  if(!is.numeric(max_attempts)){
    stop("'max_attempts' must be a numeric")
  }
  
  if(!is.na(image_width)){
    if(!is.numeric(image_width)){
      stop("'image_width' must be either NA or a numeric defining to what 
        width of the images shall be rescaled if larger.")
    }
  }
  
  

  # 2. Prep ----

  hints <- 0
  attempts <- 0
  attempt <- "start"
  startat <- startat + 1
  
  if(startat == 1){
    message("Welcome to BotanizeR quiz!\n\n
    Please click into the console to enter the species name.\n
    If you have no clou, press enter and the next image or hint will appear. 
    If you want to skip a species enter 'skip'. 
    If you want to cancel the quiz write 'exit'.\n
    Don't hit Esc if you want to save your progress.\n\n")
  }
  
  message("Species ", startat, ". ---------------------------------\n")
  
  # 3. Quiz ----
  # Species i
  i <- sample(seq_len(nrow(species_list)), 1,
              prob = ((species_list$COUNT - species_list$SCORE + 1)/
                        (species_list$SCORE+1))*species_list$INCLUDE)
  species <- species_list$SPECIES[i]
  
  # Collect information for species i
  infos <- BotanizeR_collect(
    species_list[i,], image_floraweb, hints_floraweb, image_ukplantatlas,
    hints_ukplantatlas, imagelinks_custom, image_folders, hints_custom, 
    file_location, only_links = FALSE, image_required = TRUE, 
    image_width = image_width)
  
  
  if(length(infos$images) == 0) { 
    message("No image for ", species,
            ".\nConsider adding your own image or image link\n\n")
    # Species will have zero probability to appear again
    species_list$INCLUDE[i] <- 0
    startat <- startat - 1
  } else {  
    # random image order
    infos$images <- sample(infos$images)
    
    if(length(infos)>1){
      hints_i <- c(paste("image", seq_along(infos$images)),
                   names(infos)[2:length(infos)])
    } else {
      hints_i <- paste("image", seq_along(infos$images))
    }
    
    if(!case_sensitive){
      species <- tolower(species)
    }
    
    while(attempt != species & attempt != "skip" & attempt != "exit" &
          attempts < max_attempts){
      
      genus <- FALSE
      
      for(k in seq_along(hints_i)){
        
        attempt <- "start"
        
        message("\nhint ", k, " of ", length(hints_i), ": ", hints_i[k], "\n")
        
        if(hints < length(hints_i)){
          hints <- hints + 1
        }   
        
        if(grepl("image", hints_i[k])){
          
          par(mar = rep(0.5, 4), oma = rep(0, 4))
          plot(infos$images[[as.numeric(strsplit(hints_i[k], " ")[[1]][2])]],
               axes = FALSE)
          
        } else {
          
          if(hints_i[k] == "map"){
            
            par(oma = c(0, 0, 0, 10.5))
            plot(infos$map[[1]], pal = infos$map[[2]], key.pos = 4, main = "")
            
          } else {
            
            if(hints_i[k] == "mapuk"){
              
              par(mar = rep(0.5, 4), oma = rep(0, 4))
              plot(infos$mapuk)
              
            } else {
              
              message(infos[[hints_i[k]]])
              
            }
          }
        }
        
        while(attempt != species & attempt != "" & attempt != "skip" &
              attempt != "exit" & attempts < max_attempts){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(!case_sensitive){
            attempt <- tolower(attempt)
          } 
          
          if(attempt == ""){
            attempts <- attempts - 1
            next()
          } else {
            if(strsplit(attempt, " ")[[1]][1] ==
               ifelse(case_sensitive, species_list$GENUS[i],
                      tolower(species_list$GENUS[i]))){
              genus <- TRUE
            }
          }
          
          if(species != attempt & attempt != "skip" & attempt != "exit"){
            if(case_sensitive){
              message(adist(attempt, species), " ",
                      ifelse(adist(attempt, species) > 1,
                             "characters", "character"), " different\n",
                      ifelse(strsplit(attempt, " ")[[1]][1] ==
                               species_list$GENUS[i], "Genus correct\n", "")) 
            } else {
              message(adist(attempt, species), " ",
                      ifelse(adist(attempt, species) > 1, "characters",
                             "character"), " different\n",
                      ifelse(strsplit(attempt, " ")[[1]][1] ==
                               tolower(species_list$GENUS[i]),
                             "Genus correct\n", "")) 
            }
          }
        }
        if(species == attempt | attempt == "skip" | attempt == "exit" |
           attempts >= max_attempts){
          if(attempt == "skip" | attempt == "exit"){
            attempts <- attempts - 1
          }
          break()
        }
      }
    }
    
    if(species == attempt){
      species_list$SCORE[i] <- species_list$SCORE[i] + 1
      # attempts per correct species
      species_list$ATTEMPTS[i] <- species_list$ATTEMPTS[i] + attempts + hints
      
      message("Species correct after ", hints,
              ifelse(hints == 1, " hint", " hints"), " and ", attempts,
              ifelse(attempts == 1, " attempt\n\n", " attempts\n\n"),
              species_list$TAXONNAME[i],
              ifelse("German name" %in% hints_i,
                     paste0("\n",infos[["German name"]]), ""),
              ifelse("family" %in% hints_i,
                     paste0("\n", infos[["family"]]), ""), "\n\n")
    } else {
      message("Species not ", ifelse(genus, "(but genus) ", ""),
              "correct after ", hints, ifelse(hints == 1, " hint", " hints"),
              " and ", attempts,
              ifelse(attempts == 1, " attempt\n\n", " attempts\n\n"),
              species_list$TAXONNAME[i],
              ifelse("German name" %in% hints_i,
                     paste0("\n", infos[["German name"]]), ""),
              ifelse("family" %in% hints_i,
                     paste0("\n", infos[["family"]]), ""), "\n\n")
    }
  }
  
  if(attempt == "exit"){
    message(ifelse(startat > 1, "Great! ", ""), "You practiced ", startat-1,
            " species and got ", sum(species_list$SCORE)-init_score,
            " of them right. \nOn average you used ",
            ifelse((sum(species_list$SCORE)-init_score) >= 1,
                   round((sum(species_list$ATTEMPTS)-init_attempts)/
                           (sum(species_list$SCORE)-init_score), 2), 0),
            " attempts+hints per correct species. \nGoodbye...")
    return(species_list)
  } else {
    species_list$COUNT[i] <- species_list$COUNT[i] + 1
    
    BotanizeR_quiz(species_list, image_floraweb, hints_floraweb,
                   image_ukplantatlas, hints_ukplantatlas,
                   imagelinks_custom, image_folders, hints_custom,
                   case_sensitive, file_location, startat = startat, 
                   init_count = init_count, init_score = init_score, 
                   init_attempts = init_attempts, max_attempts, image_width)
  }
}
