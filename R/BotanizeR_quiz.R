### BotanizeR
BotanizeR_quiz <- function(species_list, hints = c("description","status","habitat","family","German name"), 
                           case_sensitive = TRUE, startat = 0, file_location="temporary"){
  
  # 1. Controls ----
  # Package dependencies
  require(imager)
  require(XML)
  
  # Arguments
  if(!all(hints %in% c("description","status","habitat","family","German name"))){
    stop('"hints" must be a subset of c("description","status","habitat","family","German name")')
  }
  
  # 2. Prep ----
  if(file_location=="temporary"){
    # Create folder for temp files
    dir <- tempfile()
    dir.create(dir)
  } else {
    dir <- file_location
  }

    
  # Species i
  i <- sample(1:nrow(species_list), 1, prob = species_list$SCORE/species_list$COUNT)
  species <- species_list$SPECIES[i]
  
  if(!case_sensitive){
    species <- tolower(species)
  }
  
  # 3. Main infos ----
  # I need this step because of an error in RCURL when getting the url
  download.file(paste("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"main.txt"), quiet = T)
  html_main <- htmlTreeParse(file = file.path(dir,"main.txt"), isURL = F, isHTML=T, useInternalNodes = T)
  infos_main <- xpathApply(html_main, "//div[@id='content']//p",xmlValue)

  # 4. Photo ----
  image=NA
  image2=NA
  
  #download.file(paste("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",species_list$NAMNR[i], sep=""), destfile = file.path(dir,"photo.txt"), quiet = T)
  
  if(length(xpathApply(html_main, "//a[@class='imglink']",xmlAttrs))>0 & grepl("foto\\.xsql",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs))[1]){
    download.file(paste("https://www.floraweb.de/pflanzenarten/",grep("foto\\.xsql",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)[[1]], value = T), sep=""), destfile = file.path(dir,"photo.txt"), quiet = T)
    html_photo <- htmlTreeParse(file = file.path(dir,"photo.txt"), isURL = F, isHTML=T, useInternalNodes = T)
    infos_photo <- xpathApply(html_photo, "//div[@id='content']//p",xmlValue)
    # photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
    photolinks <- sapply(xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs), function(x) grep("bilder", x, value=T))
    
    if(photolinks[1]!="../bilder/arten/"){
      try(image <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",photolinks[1]), sep="")),silent = T)
      if (length(photolinks)>1){
        try(image2 <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",gsub("\\.tmb","",photolinks[2])), sep="")),silent = T)
      }
    }
  }
  
  par(mar=c(0.5,0.5,0.5,0.5))
  plot(1,1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  
  if(is.na(image2[1])){
    hints_i <- c("image", hints)
  } else {
    hints_i <- c("image","image2", hints)
  }
  
  # 5. Quiz ----
  attempts <- 0
  attempt <- "start"
  startat <- startat + 1
  
  if(startat == 1){
    message("Welcome to BotanizeR quiz!\n\nPlease click into the console to enter the species name.\nIf you have no clou, press enter and the image or next hint will appear. If you want to skip a species enter 'skip'. If you want to cancel the quiz write 'exit'.\nDon't hit Esc if you want to save your progress.\n\n")
  }
  
  message(startat, ". ---------------------------------\n")
  
  if(is.na(image[1])) { 
    message("No image for ",species, ".\n\n")
    species_list$SCORE[i] <- 0 # Species will have zero probability to appear again
  } else {  
    
    # ecology
    download.file(paste("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"ecology.txt"), quiet = T)
    html_ecology <- htmlTreeParse(file = file.path(dir,"ecology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
    infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",xmlValue)
    
    # biology
    # download.file(paste("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"biology.txt"), quiet = T)
    # html_biology <- htmlTreeParse(file = file.path(dir,"biology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
    # infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",xmlValue)
    
    while(attempt != species & attempt != "skip" & attempt != "exit" & attempts <= 10){
      
      for(k in 1:length(hints_i)){
        
        attempt <- "start" 
        
        if(hints_i[k]=="image"){
          plot(image, axes=FALSE)
        } 
        
        if(hints_i[k]=="image2"){
          plot(1,1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
          Sys.sleep(0.3)
          plot(image2, axes=FALSE)
        }
        
        if(hints_i[k]=="description"){
          message("Bestimmungshilfe/Morphologie:\n",infos_photo[[which(infos_photo == "Bestimmungshilfe:")+1]])
          # message(infos_biology[[2]])
        }

        if(hints_i[k]=="status"){
          message(infos_main[[7]],"\n",infos_main[[8]])
        }
        
        if(hints_i[k]=="family"){
          message(infos_main[[6]])
        }
        
        if(hints_i[k]=="German name"){
          message(infos_main[[5]])
        }
        
        if(hints_i[k]=="habitat"){
          if(infos_ecology[[3]] != "Formation: \r\nTaxon keiner Formation zugeordnet "){
            message(infos_ecology[[3]])
          } else {
            next()
          }
        }
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempt != "exit" & attempts <= 10){
          attempts <- attempts + 1

          attempt <- readline("Species: ")

          if(!case_sensitive){
            attempt <- tolower(attempt)
          }

          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip" & attempt != "exit"){
            if(case_sensitive){
              message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
            } else {
              message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==tolower(species_list$GENUS[i]),"Genus correct\n","")) 
            }
          }
        }
        if(species==attempt | attempt == "skip" | attempt == "exit" | attempts > 10){
          break()
        }
      }
    }
    
    species_list$SCORE[i] <- species_list$SCORE[i] + attempts
    
    if(species==attempt){
      species_list$COUNT[i] <- species_list$COUNT[i] + 1
      message("Species correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]],"\n\n")
    } else {
      message("Species not correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]],"\n\n")
    }
  }
  
  if(attempt=="exit"){
    message("Goodbye")
    return(species_list)
  } else {
    BotanizeR_quiz(species_list, hints, case_sensitive, startat, file_location=dir)
  }
}




