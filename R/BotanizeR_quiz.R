### BotanizeR

BotanizeR_quiz <- function(species_list, hints = c("image","description","floristic status","conservation status","family","German name")){
  
  # 1. Controls ----
  # Package dependencies
  require(imager)
  require(XML)
  
  # Arguments
  if(!all(hints %in% c("image","description","floristic status","conservation status","family","German name"))){
    stop('"hints" must be a subset of c("image","description","floristic status","conservation status","family","German name")')
  }
  
  # 2. Quiz ----
  i <- sample(1:nrow(species_list), 1, prob = species_list$SCORE/species_list$COUNT)
  
  species <- species_list$SPECIES[i]
  
  # Species i
  
  # main infos
  # I need this step because of an error in RCURL when getting the url
  download.file(paste("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = "main.txt", quiet = T)
  html_main <- htmlTreeParse(file = "main.txt", isURL = F, isHTML=T, useInternalNodes = T)
  infos_main <- xpathApply(html_main, "//div[@id='content']//p",xmlValue)
  
  # ecology
  download.file(paste("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = "ecology.txt", quiet = T)
  html_ecology <- htmlTreeParse(file = "ecology.txt", isURL = F, isHTML=T, useInternalNodes = T)
  infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",xmlValue)
  
  # photo
  download.file(paste("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",species_list$NAMNR[i], sep=""), destfile = "photo.txt", quiet = T)
  html_photo <- htmlTreeParse(file = "photo.txt", isURL = F, isHTML=T, useInternalNodes = T)
  infos_photo <- xpathApply(html_photo, "//div[@id='content']//p",xmlValue)
  photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
  image=NA
  if(photolink!="../bilder/arten/"){
    try(image <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",photolink), sep="")),silent = T)
  }
  
  
  attempts <- 0
  attempt <- "start"
  while(attempt != species & attempt != "skip" & attempts <= 10){
    
    for(k in 1:length(hints)){
      
      attempt <- "start"    
      if(hints[k]=="image"){
        if(!is.na(image[1])) {
          plot(1,1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
          Sys.sleep(0.3)
          plot(image, xaxt="n", yaxt="n")
          
          while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
            attempts <- attempts + 1
            
            attempt <- readline("Species: ")
            
            if(attempt==""){
              next
            }
            if(species!=attempt & attempt != "skip") {
              message(adist(attempt, species)," characters different\n")      
            }
          }
          if(species==attempt | attempt == "skip" | attempts > 10){
            break()
          }
        }
      }
      
      if(hints[k]=="description"){
        message(infos_photo[[which(infos_photo == "Bestimmungshilfe:")+1]])
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip"){        
            message(adist(attempt, species)," characters different\n") 
          }
        }
        if(species==attempt | attempt == "skip" | attempts > 10){
          break()
        }
      }
    }
  }
  if(species==attempt){
    message("Species correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]])
  } else {
    message("Species not correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]])
  }
  BotanizeR_quiz(species_list, hints)
}




