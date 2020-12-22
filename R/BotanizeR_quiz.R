### BotanizeR

BotanizeR_quiz <- function(species_list, hints = c("image","image2","description","status","habitat","family","German name")){
  
  # 1. Controls ----
  # Package dependencies
  require(imager)
  require(XML)
  
  # Arguments
  if(!all(hints %in% c("image","image2","description","status","habitat","family","German name"))){
    stop('"hints" must be a subset of c("image","image2","description","status","habitat","family","German name")')
  }
  
  # Create folder for temp files
  dir <- tempfile()
  dir.create(dir)
  
  # Quiz ----
  i <- sample(1:nrow(species_list), 1, prob = species_list$SCORE/species_list$COUNT)
  
  # Species i
  species <- species_list$SPECIES[i]
  

  # main infos
  # I need this step because of an error in RCURL when getting the url
  download.file(paste("https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"main.txt"), quiet = T)
  html_main <- htmlTreeParse(file = file.path(dir,"main.txt"), isURL = F, isHTML=T, useInternalNodes = T)
  infos_main <- xpathApply(html_main, "//div[@id='content']//p",xmlValue)

  # ecology
  download.file(paste("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"ecology.txt"), quiet = T)
  html_ecology <- htmlTreeParse(file = file.path(dir,"ecology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
  infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",xmlValue)
  
  # biology
  # download.file(paste("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"biology.txt"), quiet = T)
  # html_biology <- htmlTreeParse(file = file.path(dir,"biology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
  # infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",xmlValue)
  
  # photo
  #download.file(paste("https://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=",species_list$NAMNR[i], sep=""), destfile = file.path(dir,"photo.txt"), quiet = T)
  download.file(paste("https://www.floraweb.de/pflanzenarten/",grep("foto",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)[[1]], value = T), sep=""), destfile = file.path(dir,"photo.txt"), quiet = T)
  html_photo <- htmlTreeParse(file = file.path(dir,"photo.txt"), isURL = F, isHTML=T, useInternalNodes = T)
  infos_photo <- xpathApply(html_photo, "//div[@id='content']//p",xmlValue)
  # photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
  photolinks <- sapply(xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs), function(x) grep("bilder", x, value=T))
  
  image=NA
  image2=NA
  if(photolinks[1]!="../bilder/arten/"){
    try(image <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",photolinks[1]), sep="")),silent = T)
    try(image2 <- load.image(paste("https://www.floraweb.de", gsub("\\.\\.","",gsub("\\.tmb","",photolinks[2])), sep="")),silent = T)
  }
  
  attempts <- 0
  attempt <- "start"
  while(attempt != species & attempt != "skip" & attempt != "noinfo" & attempts <= 10){
    
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
              message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n",""))     
            }
          }
          if(species==attempt | attempt == "skip" | attempts > 10){
            break()
          }
        } else {
          attempt <- "noinfo"
          break()
        }
      } 

      if(hints[k]=="image2"){
        if(!is.na(image2[1])) {
          plot(1,1, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
          Sys.sleep(0.3)
          plot(image2, xaxt="n", yaxt="n")
          
          while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
            attempts <- attempts + 1
            
            attempt <- readline("Species: ")
            
            if(attempt==""){
              next
            }
            if(species!=attempt & attempt != "skip") {
              message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n",""))     
            }
          }
          if(species==attempt | attempt == "skip" | attempts > 10){
            break()
          }
        }
      } 
      
      if(hints[k]=="description"){
        message(infos_photo[[which(infos_photo == "Bestimmungshilfe:")+1]])
        # message(infos_biology[[2]])
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip"){        
            message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
          }
        }
        if(species==attempt | attempt == "skip" | attempts > 10){
          break()
        }
      }
      if(hints[k]=="status"){
        message(infos_main[[7]],"\n",infos_main[[8]])
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip"){        
            message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
          }
        }
        if(species==attempt | attempt == "skip" | attempts > 10){
          break()
        }
      }
      if(hints[k]=="family"){
        message(infos_main[[6]])
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip"){        
            message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
          }
        }
        if(species==attempt | attempt == "skip" | attempts > 10){
          break()
        }
      }
      if(hints[k]=="German name"){
        message(infos_main[[5]])
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip"){        
            message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
          }
        }
        if(species==attempt | attempt == "skip" | attempts > 10){
          break()
        }
      }
      if(hints[k]=="habitat"){
        message(infos_ecology[[3]])
        
        while(attempt != species & attempt != "" & attempt != "skip" & attempts <= 10){
          attempts <- attempts + 1
          
          attempt <- readline("Species: ")
          
          if(attempt==""){
            next()
          }
          if(species!=attempt & attempt != "skip"){        
            message(adist(attempt, species)," characters different\n",ifelse(strsplit(attempt," ")[[1]][1]==species_list$GENUS[i],"Genus correct\n","")) 
          }
        }
        if(species==attempt | attempt == "skip" | attempts > 10){
          break()
        }
      }
    }
  }
  if(species==attempt){
    message("Species correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]],"\n\n")
  } else {
    if(attempt=="noinfo"){
      message("No image for ",species, ".\n\n")
    } else {
      message("Species not correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]],"\n\n")
    }
  }
  BotanizeR_quiz(species_list, hints)
}




