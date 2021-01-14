
# Packages
library(shiny)
library(BotanizeR)
# library(imager)
library(XML)
library(sf)

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
    
    # dir <- tempfile()
    # dir.create(dir)
    # })
    
    # 1. Selected species ----
    # Plant list
    output$plant_list <- renderPrint({plant_list})
    
    selected_species <- reactive({
        input$plant_list
        selected_species
    })
    
    # Tying autocomplete...
    
    # selected_species <- reactive({
    #     if(input$go == 0){return()}
    #     isolate({
    #         input$go
    #         input$plant_list
    #         selected_species
    #     })
    # }) 
    
    # updateSelectizeInput("plant_list", choices = plant_list,
    #                      server = TRUE)
    
    observe({
        # Plant species chosen
        j <- which(species_list$SPECIES == input$plant_list)
        
        # Download informations with BotanizeR_collect()
        sp_infos <- BotanizeR_collect(
            species_row = floraweb_species[which(floraweb_species$SPECIES == input$plant_list), ], 
            image_floraweb = TRUE,
            hints_floraweb = c("description", "status", "habitat", "family",
                               "German name"), 
            hints_custom = NULL, imagelink_custom = NULL,
            image_folders = "www/pictures_Clemens",
            # image_folders = "~/ShinyApps/BotanizeR/WWW/pictures_Clemens", # This is needed on server; 
            file_location = "temporary", only_links = TRUE)
        
        # Photos ----
        output$selected_sp_photo <- renderUI({
            par(mar = rep(0.5, 4), oma = rep(0, 4))
            photo_list <- lapply(sp_infos$images, function(x){
                tags$div(
                    tags$img(src = x, width = "50%", height = "50%"),
                    tags$script(src = "titlescript.js")
                )
            })
            do.call(tagList, photo_list)
        })
        
        # Description ----
        # output$selected_sp_description <- renderText({
        #     # Download biological information from FloraWeb
        #     download.file(paste0("https://www.floraweb.de/pflanzenarten/biologie.xsql?suchnr=",
        #                          species_list$NAMNR[j], "&"),
        #                   destfile = file.path(dir,"biology.txt"), quiet = T)
        #     html_biology <- htmlTreeParse(file = file.path(dir,"biology.txt"), isURL = F, isHTML=T, useInternalNodes = T)
        #     infos_biology <- xpathApply(html_biology, "//div[@id='content']//p",xmlValue)
        #     
        #     print(paste0("Description: ", infos_biology[[2]]))
        # })
        
        # Habitat ----
        output$selected_sp_habitat <- renderText({
            print(sp_infos$habitat[[1]])
        })
        
        # Family ----
        output$selected_sp_family <- renderText({
            print(sp_infos$family[[1]])
        })
        
        # Status ----
        output$selected_sp_status <- renderText({
            print(sp_infos$status)
        })
        
        # German name ----
        output$selected_sp_german <- renderText({
            print(sp_infos$`German name`)
        })
        
        # Map ----
        isolate({
            observe({
                options <- pmatch("Map", input$options)
                output$selected_sp_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if(!is.na(options)){
                        # Downloading map only
                        sp_map <- BotanizeR_collect(
                            species_row = floraweb_species[which(floraweb_species$SPECIES == input$plant_list), ], 
                            image_floraweb = FALSE,
                            hints_floraweb = c("map"), 
                            hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                            file_location = "temporary", only_links = TRUE)
                        
                        par(oma = c(0, 0, 0, 10.5))
                        plot(sp_map$map[[1]], pal = sp_map$map[[2]],
                             key.pos = 4, main = "")
                    }
                })
            })
        })
        
        # Chorology ----
        output$selected_sp_chorology <- renderUI({
            par(mar = rep(0.5, 4), oma = rep(0, 4))
            tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                  species_list$NAMNR[j],
                                  ".GIF"))
        })
    }) # closes observe()
    
    # 2. Quizz ----
    observe({
        input$newplant # hitting the new plant button
        
        # random species
        species <- sample(species_list$SPECIES, 1)
        
        # species <- as.character(input$ex_sp) #species_list$SPECIES[i]
        i <- which(floraweb_species$SPECIES == species)
        
        # Download informations with BotanizeR_collect()
        sp_quizz <- BotanizeR_collect(
            species_row = floraweb_species[which(floraweb_species$SPECIES == species), ], 
            image_floraweb = TRUE,
            hints_floraweb = NULL, 
            hints_custom = NULL, imagelink_custom = NULL,
            image_folders = "www/pictures_Clemens",
            # image_folders = "~/ShinyApps/BotanizeR/WWW/pictures_Clemens", # This is needed on server; 
            file_location = "temporary", only_links = TRUE)
        
        # Photos ----
        output$random_sp <- renderUI({
            par(mar = rep(0.5, 4), oma = rep(0, 4))
            photo_random <- lapply(sp_quizz$images, function(x){
                tags$div(
                    tags$img(src = x, width = "50%", height = "50%"),
                    tags$script(src = "titlescript.js")
                )
            })
            do.call(tagList, photo_random)
        })
        
        # Map ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map"),
                                        input$quizz_options)
                output$random_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if(!is.na(quizz_options[6])){
                        # Downloading map only
                        random_map <- BotanizeR_collect(
                            species_row = floraweb_species[which(floraweb_species$SPECIES == input$plant_list), ], 
                            image_floraweb = FALSE,
                            hints_floraweb = c("map"), 
                            hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                            file_location = "temporary", only_links = TRUE)
                        
                        par(oma = c(0, 0, 0, 10.5))
                        plot(random_map$map[[1]], pal = random_map$map[[2]],
                             key.pos = 4, main = "")
                    }
                })
            })
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
