
library(shiny)
library(BotanizeR)
library(imager)

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
    data(floraweb_species)
    floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                                   floraweb_species$BioDiv2005==1), ]
    species_list <- floraweb_species
    
    dir <- tempfile()
    dir.create(dir)
    
    observe({
        input$newplant # hitting the new plant button
        # Sampling one species ----
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
        infos_main <- xpathApply(html_main, "//div[@id='content']//p",xmlValue)
        
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
        
        # 1. Photo of the random species ----
        output$random_sp <- renderPlot({
            par(mar = rep(0.5, 4), oma = rep(0, 4))
            plot(image, axes = FALSE, ylim = c(height(image_sp[[2]]), 1))
        })
        
        # display text
        output$status1 <- renderText({
            "Mark your answer and click 'Submit!'"
        })
        output$status2 <- renderText({
            ""
        })
        output$status3 <- renderText({
            ""
        })
        
    }) # closing observe
    
    observe({
        # Defining & initializing the reactiveValues object
        counter <- reactiveValues(countervalue = 0) 
        output$nb_tries <- renderText({"Number of tries: 0"})
        observeEvent(input$submit, {
            counter$countervalue <- counter$countervalue + 1
        output$nb_tries <- renderText({
            paste0("Number of tries: ", counter$countervalue)})
        })
    })
})
