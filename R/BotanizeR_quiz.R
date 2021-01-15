### BotanizeR
BotanizeR_quiz <- function(species_list, image_floraweb=TRUE, 
                           hints_floraweb = c("description","status","habitat","family","German name"), 
                           hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL, 
                           case_sensitive = TRUE, file_location="temporary", startat = 0, init_count = sum(species_list$COUNT),
                           init_score = sum(species_list$SCORE), init_attempts = sum(species_list$ATTEMPTS), max_attempts = 10){
  
  # 1. Controls ----
  # Package dependencies
  require(imager)
  require(XML)
  if("map" %in% hints_floraweb){
    require(sf)
  }
  
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
    message("Welcome to BotanizeR quiz!\n\nPlease click into the console to enter the species name.\nIf you have no clou, press enter and the next image or hint will appear. If you want to skip a species enter 'skip'. If you want to cancel the quiz write 'exit'.\nDon't hit Esc if you want to save your progress.\n\n")
  }
  
  # plot(1,1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    
  message("Species ", startat, ". ---------------------------------\n")
  
  
  # 3. Quiz ----
  
  # Species i
  i <- sample(1:nrow(species_list), 1, prob = ((species_list$COUNT - species_list$SCORE + 1)/(species_list$SCORE+1))*species_list$INCLUDE) # Account for number of attempts?
  species <- species_list$SPECIES[i]
  
  # Collect infos for species i
  infos <- BotanizeR_collect(species_list[i,], image_floraweb, hints_floraweb, 
                                         hints_custom, imagelink_custom, image_folders,
                                         file_location, image_required = TRUE)

  
  if(length(infos$images)==0) { 
    message("No image for ",species, ".\nConsider adding your own image or image link\n\n")
    species_list$INCLUDE[i] <- 0 # Species will have zero probability to appear again
    startat <- startat - 1
  } else {  

    if(length(infos)>1){
      hints_i <- c(paste("image",c(1:length(infos$images))),names(infos)[2:length(infos)])
    } else {
      hints_i <- paste("image",c(1:length(infos$images)))
    }
    
    if(!case_sensitive){
      species <- tolower(species)
    }
    
    while(attempt != species & attempt != "skip" & attempt != "exit" & attempts < max_attempts){
      
      genus <- FALSE

      for(k in 1:length(hints_i)){

        attempt <- "start"
        
        message("\nhint ", k, " of ", length(hints_i), ": ", hints_i[k],"\n")
        
        if(hints < length(hints_i)){
          hints <- hints + 1
        }   
        
        if(grepl("image",hints_i[k])){
          
          par(mar=c(0.5,0.5,0.5,0.5),oma=c(0,0,0,0))
          plot(infos$images[[as.numeric(strsplit(hints_i[k]," ")[[1]][2])]], axes=FALSE)
          
        } else {
          
          if(hints_i[k]=="map"){
            
            par(oma=c(0,0,0,10.5))
            plot(infos$map[[1]], pal = infos$map[[2]], key.pos = 4, main="")
            
          } else {
            
            message(infos[[hints_i[k]]])
            
          }
        }
        
    
        while(attempt != species & attempt != "" & attempt != "skip" & attempt != "exit" & attempts < max_attempts){
          attempts <- attempts + 1

          attempt <- readline("Species: ")

          if(!case_sensitive){
            attempt <- tolower(attempt)
          } 
          
          if(attempt==""){
            attempts <- attempts - 1
            next()
          } else {
            if(strsplit(attempt," ")[[1]][1]==ifelse(case_sensitive,species_list$GENUS[i],tolower(species_list$GENUS[i]))){
              genus <- TRUE
            }
          }
          
          if(species!=attempt & attempt != "skip" & attempt != "exit"){
            if(case_sensitive){
              message(adist(attempt, species)," ",ifelse(adist(attempt, species)>1,"characters","character")," different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
            } else {
              message(adist(attempt, species)," ",ifelse(adist(attempt, species)>1,"characters","character")," different\n",ifelse(strsplit(attempt," ")[[1]][1]==tolower(species_list$GENUS[i]),"Genus correct\n","")) 
            }
          }
        }
        if(species==attempt | attempt == "skip" | attempt == "exit" | attempts >= max_attempts){
          if(attempt == "skip" | attempt == "exit"){ #  | attempts >= max_attempts
            attempts <- attempts - 1
          }
          break()
        }
      }
    }
    
    
    if(species==attempt){
      species_list$SCORE[i] <- species_list$SCORE[i] + 1
      species_list$ATTEMPTS[i] <- species_list$ATTEMPTS[i] + attempts + hints # attempts per correct species
      
      message("Species correct after ",hints,ifelse(hints==1," hint"," hints"), " and ",attempts,ifelse(attempts==1," attempt\n\n"," attempts\n\n"),species_list$TAXONNAME[i],
              ifelse("German name" %in% hints_i,paste("\n",infos[["German name"]],sep=""),""),
              ifelse("family" %in% hints_i,paste("\n",infos[["family"]],sep=""),""),"\n\n")
    } else {
      message("Species not ",ifelse(genus,"(but genus) ",""),"correct after ",hints,ifelse(hints==1," hint"," hints"), " and ",attempts,ifelse(attempts==1," attempt\n\n"," attempts\n\n"),species_list$TAXONNAME[i],
              ifelse("German name" %in% hints_i,paste("\n",infos[["German name"]],sep=""),""),
              ifelse("family" %in% hints_i,paste("\n",infos[["family"]],sep=""),""),"\n\n")
    }
  }
  
  if(attempt=="exit"){
    message(ifelse(startat>1,"Great! ",""),"You practiced ",startat-1," species and got ",sum(species_list$SCORE)-init_score," of them right. \nOn average you used ",
            ifelse((sum(species_list$SCORE)-init_score)>=1,round((sum(species_list$ATTEMPTS)-init_attempts)/(sum(species_list$SCORE)-init_score),2),0)," attempts+hints per correct species. \nGoodbye...")
    return(species_list)
  } else {
    species_list$COUNT[i] <- species_list$COUNT[i] + 1

    BotanizeR_quiz(species_list, image_floraweb, hints_floraweb, 
                   hints_custom, imagelink_custom, image_folders,
                   case_sensitive, file_location, startat = startat, 
                   init_count = init_count, init_score = init_score, 
                   init_attempts = init_attempts)
  }
}




