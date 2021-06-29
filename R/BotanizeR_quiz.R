#' BotanizeR quiz
#'
#' Navigates through species from floraweb.de (or a defined subset thereof),
#' shows pictures and descriptions and other hints and let's the user guess the
#' species name. Number of tries and attempts are used to calculate scores that
#' will be used to update probabilities for the random sampling of subsequent
#' species.
#'
#'
#' @param species_list a data.frame with the species for which we want to
#' retrieve information. **It should contain the following columns**: 
#' *NAMNR*, *TAXONNAME*, *SPECIES*, *GENUS*, *EPITHET*, *AUTHOR*, *COUNT*,
#' *SCORE* and *ATTEMPTS*
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
#' @param case_sensitive logical.
#' 
#' @param file_location character vector that defines 
#' 
#' @param startat number
#' 
#' @param init_count sum of the column `$COUNT` in `species_list`,
#' defining the total number of tries of the user 
#' 
#' @param init_score sum of the column `$SCORE` in `species_list`,
#' defining the total score of the user  
#' 
#' @param init_attempts sum of the column `$ATTEMPTS` in `species_list`,
#' defining how many attempts he user had in total 
#' 
#' @param max_attempts number defining the number of attempts per species 
#' 
#' @param image_width number to define the width of the images.
#' 
#' @return
#' A data.frame with updated scores and counts per species.
#'
#' @details After running the main function BotanizeR_quiz() you need to click
#' into the console to type the species names in there. If you have no clue,
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
#'     online teaching and beyond. Plants, People, Planet.
#'
#' @seealso [BotanizeR::BotanizeR_collect()]
#'
#' @examples
#' # Species list for Germany with IDs from floraweb.de
#' data(floraweb_species)
#'
#' # Subset for about 300 species students in Goettingen learn
#' floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1 |
#' floraweb_species$BioDiv2005==1), ]
#' 
#' # Type in species name, or press enter for next hint or type "skip" and press 
#' # enter for next species or type "exit" to end quiz and save results
#' floraweb_species_trained <- BotanizeR_quiz(
#' species_list = floraweb_species, hints_floraweb = c("description", "status",
#' "habitat","family","German name"), case_sensitive = FALSE)
#' 
#' # If you want to include distribution maps as hints add "map" to hints; This increases the download times a bit
#' floraweb_species_trained <- BotanizeR_quiz(
#' species_list = floraweb_species,
#' hints_floraweb = c("map","description","status", "habitat", "family",
#' "German name"), case_sensitive = FALSE)
#' 
#' # If you want to keep track of your progress, you can save the species list with 
#' # updated scores locally and load it in the next session
#' 
#' # Initial saving
#' write.csv(floraweb_species_trained, "floraweb_species_trained.csv",
#' row.names = FALSE)
#' 
#' # Load species list
#' floraweb_species_trained <- read.csv("floraweb_species_trained.csv")
#' 
#' # Practice
#' floraweb_species_trained <- BotanizeR_quiz(
#'   species_list = floraweb_species_trained,
#'   hints_floraweb = c("map", "description", "status", "habitat", "family",
#'                      "German name"), case_sensitive = FALSE)
#' 
#' # Save species list
#' write.csv(floraweb_species_trained, "floraweb_species_trained.csv",
#'           row.names = FALSE)
#' 
#' ### example for three species with custom hints and images
#' custom_species <- floraweb_species[
#'   which(floraweb_species$SPECIES %in% c("Acer campestre", "Erica carnea",
#'                                         "Melampyrum nemorosum")), ]
#' 
#' custom_species_trained <- BotanizeR_quiz(
#'   species_list = custom_species, image_floraweb = TRUE,
#'   hints_floraweb = NULL, hints_custom = c("ownhint_1", "ownhint_2"), 
#'   imagelinks_custom = c("imagelink_1", "imagelink_2"), 
#'   image_folders = NULL, case_sensitive = FALSE)
#' 
#' @export

BotanizeR_quiz <- function(
  species_list, image_floraweb = TRUE, 
  hints_floraweb = c("description", "status", "habitat", "family",
                     "German name"),
  image_ukplantatlas = FALSE, hints_ukplantatlas = NULL,
  imagelinks_custom = NULL, image_folders = NULL, hints_custom = NULL,
  case_sensitive = TRUE, file_location = "temporary", startat = 0,
  init_count = sum(species_list$COUNT),
  init_score = sum(species_list$SCORE),
  init_attempts = sum(species_list$ATTEMPTS), max_attempts = 10,
  image_width = 500){
  
  # 1. Controls ----
  # Arguments
  
  # 2. Prep ----
  init_count <- init_count
  init_score <- init_score
  init_attempts <- init_attempts
  
  hints <- 0
  attempts <- 0
  attempt <- "start"
  startat <- startat + 1
  
  if(startat == 1){
    message("Welcome to BotanizeR quiz!\n\nPlease click into the console to
            enter the species name.\nIf you have no clou, press enter and the
            next image or hint will appear. If you want to skip a species enter
            'skip'. If you want to cancel the quiz write 'exit'.\nDon't hit Esc
            if you want to save your progress.\n\n")
  }
  
  message("Species ", startat, ". ---------------------------------\n")
  
  # 3. Quiz ----
  # Species i
  i <- sample(1:nrow(species_list), 1,
              prob = ((species_list$COUNT - species_list$SCORE + 1)/
                        (species_list$SCORE+1))*species_list$INCLUDE)
  species <- species_list$SPECIES[i]
  
  # Collect information for species i
  infos <- BotanizeR_collect(
    species_list[i,], image_floraweb, hints_floraweb, image_ukplantatlas,
    hints_ukplantatlas, hints_custom, imagelinks_custom, image_folders,
    file_location, image_required = TRUE, image_width = image_width)
  
  
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
      hints_i <- c(paste("image", c(1:length(infos$images))),
                   names(infos)[2:length(infos)])
    } else {
      hints_i <- paste("image", c(1:length(infos$images)))
    }
    
    if(!case_sensitive){
      species <- tolower(species)
    }
    
    while(attempt != species & attempt != "skip" & attempt != "exit" &
          attempts < max_attempts){
      
      genus <- FALSE
      
      for(k in 1:length(hints_i)){
        
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
                   hints_custom, imagelinks_custom, image_folders,
                   case_sensitive, file_location, startat = startat, 
                   init_count = init_count, init_score = init_score, 
                   init_attempts = init_attempts, max_attempts, image_width)
  }
}
