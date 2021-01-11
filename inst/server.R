
library(shiny)
library(BotanizeR)
library(imager)
library(XML)

shinyServer(function(input, output) {
    # Text for the different answers
    generateResponse <- function(response) {
        if (response == 1) {
            print(sample(list("Correct!", "Spot on!", "Got it!"), 1)[[1]])
        }
        else if (response == 2) {
            print(sample(list("Almost.", "Close.", "Just a bit off.."), 1)[[1]])
        }
        else if (response == 3) {
            print(sample(list("Warmer...", "Getting there..."), 1)[[1]])
        }
        else if (response == 4) {
            print(sample(list("Try again.", "Nope!"), 1)[[1]])
        }
    }
    
    # List of floraweb species
    # observe({
    data(floraweb_species)
    floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                                   floraweb_species$BioDiv2005==1), ]
    species_list <- floraweb_species
    plant_list <- floraweb_species$SPECIES
    
    dir <- tempfile()
    dir.create(dir)
    # })
    
    # 1. Selected species ----
    # Plant list
    output$plant_list <- renderPrint({plant_list})
    
    selected_species <- reactive({
        input$plant_list
        selected_species
    })
    
    observe({
        # Plant species chosen
        j <- which(species_list$SPECIES == input$plant_list)
        
        # Main infos
        download.file(
            paste0(
                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                species_list$NAMNR[j], "&"),
            destfile = file.path(dir, "main.txt"),
            quiet = TRUE)
        html_main <- htmlTreeParse(
            file = file.path(dir, "main.txt"),
            isURL = F, isHTML = T, useInternalNodes = T)
        infos_main <- xpathApply(html_main, "//div[@id='content']//p",
                                 xmlValue)
        
        # Photo ----
        output$selected_sp_photo <- renderPlot({
            image_select <- NA
            
            if(length(xpathApply(html_main, "//a[@class='imglink']"
                                 , xmlAttrs)) > 0 &
               grepl("foto\\.xsql", xpathApply(html_main, "//a[@class='imglink']",
                                               xmlAttrs))[1]){
                download.file(
                    paste0("https://www.floraweb.de/pflanzenarten/",
                           grep("foto\\.xsql",
                                xpathApply(html_main, "//a[@class='imglink']",
                                           xmlAttrs)[[1]], value = T)),
                    destfile = file.path(dir, "photo.txt"),
                    quiet = T)
                html_photo <- htmlTreeParse(file = file.path(dir,"photo.txt"),
                                            isURL = F, isHTML = T, useInternalNodes = T)
                infos_photo <- xpathApply(html_photo, "//div[@id='content']//p", xmlValue)
                # photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
                photolinks <- sapply(xpathApply(html_photo, "//div[@id='content']//img",
                                                xmlAttrs),
                                     function(x) grep("bilder", x, value = TRUE))
                
                if(photolinks[1] != "../bilder/arten/"){
                    try(image_select <- load.image(
                        paste0("https://www.floraweb.de",
                               gsub("\\.\\.", "", photolinks[1]))),
                        silent = TRUE)
                }
            }
            # Plot
            par(mar = rep(0.5, 4), oma = rep(0, 4))
            plot(image_select, axes = FALSE)
            #, ylim = c(height(image_sp[[2]]), 1))
        })
        
        # Description ----
        output$selected_sp_description <- renderText({
            # Download biological information from FloraWeb
            download.file(paste0("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=",
                                 species_list$NAMNR[j], "&"),
                          destfile = file.path(dir,"biology.txt"), quiet = T)
            html_biology <- htmlTreeParse(file = file.path(dir,"biology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
            infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",xmlValue)
            
            print(paste0("Description: ", infos_biology[[2]]))
        })
        
        # Habitat ----
        output$selected_sp_habitat <- renderText({
            # Download description from FloraWeb
            download.file(paste0("https://www.floraweb.de/pflanzenarten/oekologie.xsql?suchnr=",
                                 species_list$NAMNR[j], "&"),
                          destfile = file.path(dir, "ecology.txt"), quiet = T)
            html_ecology <- htmlTreeParse(file = file.path(dir, "ecology.txt"),
                                          isURL = F, isHTML=T, useInternalNodes = T)
            infos_ecology <- xpathApply(html_ecology, "//div[@id='content']//p",
                                        xmlValue)
            
            print(paste0("Habitat: ", infos_ecology[[3]]))
        })
        
        # Family ----
        output$selected_sp_family <- renderText({
            print(paste0("Family: ", infos_main[[6]]))
        })
        
        # Status ----
        output$selected_sp_status <- renderText({
            print(paste0("Status: ", infos_main[[7]], "\n", infos_main[[8]]))
        })
        
        # German name ----
        output$selected_sp_german <- renderText({
            print(paste0("German name: ", infos_main[[5]]))
        })
        
        # Map ----
        output$selected_sp_map <- renderPlot({
            map <- NA
            if(length(xpathApply(html_main, "//a[@class='imglink']",
                                 xmlAttrs))>0 &
               any(grepl("webkarten", xpathApply(html_main,
                                                 "//a[@class='imglink']",
                                                 xmlAttrs)))){
                
                if(!exists("CGRS_Germany")){
                    data(CGRS_Germany)
                }
                
                try({
                    taxon_ID_map <- gsub(
                        "/webkarten/karte.html\\?taxnr=",
                        "",
                        grep("webkarten",
                             xpathApply(html_main, "//a[@class='imglink']",
                                        xmlAttrs)
                             [[which(grepl("webkarten",
                                           xpathApply(html_main, "//a[@class='imglink']",
                                                      xmlAttrs)))]],
                             value = TRUE))
                    
                    download.file(paste0("https://www.floraweb.de/pflanzenarten/download_afe.xsql?suchnr=",
                                         taxon_ID_map),
                                  destfile = file.path(dir, "map.csv"),
                                  quiet = TRUE)
                    map <- read.csv(file.path(dir, "map.csv"), skip=41)[-1,
                                                                        c("CGRSNAME","AFE_SYMBOLCODE","AFESYMBOL_TEXT")]
                    
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
                                              SYMBOL_TEXT=c("absent",
                                                            "not assigned","records uncertain","extinct","probably extinct",
                                                            "cultivated, not established alien","established alien","native, incl. archaeophytes"),
                                              colour=c("white","grey90","grey80","grey70","grey60","#fdb462","#fb8072","#b3de69"))
                    
                    legend_info <- legend_info[which(legend_info$AFE_SYMBOLCODE %in% map$AFE_SYMBOLCODE),]
                    
                    levels(map$AFE_SYMBOLCODE) <- legend_info$SYMBOL_TEXT
                }, silent = TRUE)
            }
            # Map
            if(!is.na(map)[1]){
                # list(map["AFE_SYMBOLCODE"], legend_info$colour)
                
                par(oma = c(0, 0, 0, 10.5))
                plot(map["AFE_SYMBOLCODE"], pal = legend_info$colour,
                     key.pos = 4, main = "")
            }
        })
        
    }) # closes observe()
    
    # 2. Quizz ----
    observe({
        input$newplant # hitting the new plant button
        # random species
        species <- sample(species_list$SPECIES, 1)
        
        # species <- as.character(input$ex_sp) #species_list$SPECIES[i]
        i <- which(floraweb_species$SPECIES == species)
        
        # Main infos
        download.file(
            paste0(
                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                species_list$NAMNR[i],"&"),
            destfile = file.path(dir, "main.txt"),
            # destfile = file.path("main.txt"),
            quiet = TRUE)
        html_main <- htmlTreeParse(# file = file.path("main.txt"),
            file = file.path(dir, "main.txt"),
            isURL = F, isHTML=T, useInternalNodes = T)
        infos_main <- xpathApply(html_main, "//div[@id='content']//p",
                                 xmlValue)
        
        # Photo
        image <- NA
        # image2 <- NA
        
        if(length(xpathApply(html_main, "//a[@class='imglink']"
                             , xmlAttrs)) > 0 &
           grepl("foto\\.xsql", xpathApply(html_main, "//a[@class='imglink']",
                                           xmlAttrs))[1]){
            download.file(
                paste0("https://www.floraweb.de/pflanzenarten/",
                       grep("foto\\.xsql",
                            xpathApply(html_main, "//a[@class='imglink']",
                                       xmlAttrs)[[1]], value = T)),
                destfile = file.path(dir, "photo.txt"),
                quiet = T)
            html_photo <- htmlTreeParse(file = file.path(dir,"photo.txt"),
                                        isURL = F, isHTML = T, useInternalNodes = T)
            infos_photo <- xpathApply(html_photo, "//div[@id='content']//p", xmlValue)
            # photolink <- xpathApply(html_photo, "//div[@id='content']//img",xmlAttrs)[[1]][3]
            photolinks <- sapply(xpathApply(html_photo, "//div[@id='content']//img",
                                            xmlAttrs),
                                 function(x) grep("bilder", x, value = TRUE))
            
            if(photolinks[1] != "../bilder/arten/"){
                try(image <- load.image(
                    paste0("https://www.floraweb.de",
                           gsub("\\.\\.", "", photolinks[1]))),
                    silent = TRUE)
                # if (length(photolinks)>1){
                #     try(image2 <- load.image(paste0("https://www.floraweb.de", gsub("\\.\\.","",gsub("\\.tmb","",photolinks[2])))),silent = T)
                # }
            }
        }
        
        # Photo ----
        output$random_sp <- renderPlot({
            par(mar = rep(0.5, 4), oma = rep(0, 4))
            plot(image, axes = FALSE) #, ylim = c(height(image_sp[[2]]), 1))
        })
        
        # Answer ----
        # display text when no answer is provided
        output$status1 <- renderText({
            "Mark your answer and click 'Submit!'"
        })
        output$status2 <- renderText({
            ""
        })
        output$status3 <- renderText({
            ""
        })
        
        # Providing an answer
        observe({
            
            # Counting the number of tries
            # Defining & initializing the reactiveValues object
            counter <- reactiveValues(countervalue = 0) 
            output$nb_tries <- renderText({"Number of tries: 0"})
            # observeEvent(input$submit, {
            if(!is.null(input$lastkeypresscode)){
                if(input$lastkeypresscode == 13){ # hitting Enter
                    counter$countervalue <- counter$countervalue + 1
                    output$nb_tries <- renderText({
                        paste0("Number of tries: ", counter$countervalue)})
                    
                    # Initial score
                    counter <- reactiveValues(score = 0)
                    renderText("Score = 0")
                    # output$score <- 0
                    
                    # If provided answer is correct
                    # observeEvent(input$sp_answer == species, {
                    if (input$sp_answer == species){
                        output$status1 <- renderText({
                            ""
                        })
                        output$status2 <- renderText({
                            paste(generateResponse(1))
                        })
                        output$status3 <- renderText({
                            ""
                        })
                        
                        # Updating score
                        renderText({
                            counter$score <- counter$score + 1
                            paste0("Score: ", counter$score)
                            # output$score <- output$score + 1
                            # paste0("Score: ", output$score)
                        })
                        
                        # })
                    }
                } # closes 'Hitting enter'
            } # closes !is.null lastkeybutton
            # })
        })
        
        # Printing real answer ----
        observe({
            output$real_answer <- renderText("")
            observeEvent(input$real_answer, {
                output$real_answer <- renderText(species)
            })
        })
        
        # Number of tries ----
        # observe({
        #     # Defining & initializing the reactiveValues object
        #     counter <- reactiveValues(countervalue = 0) 
        #     output$nb_tries <- renderText({"Number of tries: 0"})
        #     observeEvent(input$submit, {
        #         counter$countervalue <- counter$countervalue + 1
        #         output$nb_tries <- renderText({
        #             paste0("Number of tries: ", counter$countervalue)})
        #     })
        # })
        
    }) # closing observe for new plant
    
})
