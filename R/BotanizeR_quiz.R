### BotanizeR
BotanizeR_quiz <- function(species_list, hints = c("description","status","habitat","family","German name"), 
                           case_sensitive = TRUE, file_location="temporary", startat = 0, init_count = sum(species_list$COUNT),
                           init_score = sum(species_list$SCORE)){
  
  # 1. Controls ----
  # Package dependencies
  require(imager)
  require(XML)
  if("map" %in% hints){
    require(sf)
  }
  
  # Arguments
  if(!all(hints %in% c("map","description","status","habitat","family","German name"))){
    stop('"hints" must be a subset of c("map","description","status","habitat","family","German name")')
  }
  
  # 2. Prep ----
  if(file_location=="temporary"){
    # Create folder for temp files
    dir <- tempfile()
    dir.create(dir)
  } else {
    dir <- file_location
  }

  init_count <- init_count
  init_score <- init_score
    
  # Species i
  i <- sample(1:nrow(species_list), 1, prob = ((species_list$SCORE+length(hints)+1)/(species_list$COUNT+1))*species_list$INCLUDE)
  species <- species_list$SPECIES[i]
  
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
    message("Welcome to BotanizeR quiz!\n\nPlease click into the console to enter the species name.\nIf you have no clou, press enter and the next image or hint will appear. If you want to skip a species enter 'skip'. If you want to cancel the quiz write 'exit'.\nDon't hit Esc if you want to save your progress.\n\n")
  }
  
  message(startat, ". ---------------------------------\n")
  
  if(is.na(image[1])) { 
    message("No image for ",species, ".\n\n")
    species_list$INCLUDE[i] <- 0 # Species will have zero probability to appear again
    startat <- startat - 1
  } else {  
    
    # ecology
    download.file(paste("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"ecology.txt"), quiet = T)
    html_ecology <- htmlTreeParse(file = file.path(dir,"ecology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
    infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",xmlValue)
    
    # biology
    # download.file(paste("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=",species_list$NAMNR[i],"&", sep=""), destfile = file.path(dir,"biology.txt"), quiet = T)
    # html_biology <- htmlTreeParse(file = file.path(dir,"biology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
    # infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",xmlValue)
    
    # map
    
    map <- NA
    
    if("map" %in% hints & length(xpathApply(html_main, "//a[@class='imglink']",xmlAttrs))>0 & any(grepl("webkarten",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)))){

      if(!exists("CGRS_Germany")){
        data(CGRS_Germany)
      }
      
      try({taxon_ID_map <- NA
          taxon_ID_map <- gsub("/webkarten/karte.html\\?taxnr=","",grep("webkarten",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)[[which(grepl("webkarten",xpathApply(html_main, "//a[@class='imglink']",xmlAttrs)))]], value = T))
          
          download.file(paste("https://www.floraweb.de/pflanzenarten/download_afe.xsql?suchnr=",taxon_ID_map, sep=""), destfile = file.path(dir,"map.csv"), quiet = T)
          map <- read.csv(file.path(dir,"map.csv"), skip=41)[-1,c("CGRSNAME","AFE_SYMBOLCODE","AFESYMBOL_TEXT")]
          
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
          
          legend_info <- data.frame(AFE_SYMBOLCODE=c(1:8), SYMBOL_TEXT=c("absent",
                                    "not assigned","records uncertain","extinct","probably extinct",
                                    "cultivated, not established alien","established alien","native, incl. archaeophytes"),
                                    colour=c("white","grey90","grey80","grey70","grey60","#fdb462","#fb8072","#b3de69")) 
          
          legend_info <- legend_info[which(legend_info$AFE_SYMBOLCODE %in% map$AFE_SYMBOLCODE),]
          
          levels(map$AFE_SYMBOLCODE) <- legend_info$SYMBOL_TEXT
      })#, silent = TRUE)
    }
    
    
    if(!case_sensitive){
      species <- tolower(species)
    }
    
    while(attempt != species & attempt != "skip" & attempt != "exit" & attempts <= 10){
      
      genus <- FALSE

      for(k in 1:length(hints_i)){
        
        attempt <- "start"
        
        if(hints_i[k]=="image"){
          par(mar=c(0.5,0.5,0.5,0.5),oma=c(0,0,0,0))
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
        
        if(hints_i[k]=="map"){
          if(!is.na(map)[1]){
            par(oma=c(0,0,0,10.5))
            plot(map["AFE_SYMBOLCODE"], pal = legend_info$colour, key.pos = 4, main="")
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
        if(species==attempt | attempt == "skip" | attempt == "exit" | attempts > 10){
          break()
        }
      }
    }
    
    
    if(species==attempt){
      species_list$COUNT[i] <- species_list$COUNT[i] + 1
      message("Species correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]],"\n",infos_main[[6]],"\n\n")
    } else {
      message("Species not ",ifelse(genus,"(but genus) ",""),"correct after ",attempts,ifelse(attempts==1," attempt\n"," attempts\n"),infos_main[[4]],"\n",infos_main[[5]],"\n",infos_main[[6]],"\n\n")
    }
  }
  
  if(attempt=="exit"){
    message(ifelse(startat>1,"Great! ",""),"You practiced ",startat-1," species and got ",sum(species_list$COUNT)-init_count," of them right. \nOn average you used ",ifelse(startat>1,round((sum(species_list$SCORE)-init_score)/(startat-1),2),0)," attempts/hints per species. \nGoodbye...")
    return(species_list)
  } else {
    species_list$SCORE[i] <- species_list$SCORE[i] + attempts
    BotanizeR_quiz(species_list, hints, case_sensitive, file_location = dir, startat = startat, 
                   init_count = init_count, init_score = init_score)
  }
}




