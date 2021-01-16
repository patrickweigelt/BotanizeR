
# Packages
library(shiny)
library(BotanizeR)
# library(imager)
library(XML)
library(sf)
library(slickR)

shinyServer(function(input, output) {

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
            image_folders = "www/pictures_Clemens/images_320",
            # image_folders = "~/ShinyApps/BotanizeR/WWW/pictures_Clemens/images_320", # This is needed on server; 
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
        
        # trying slickr
        output$slickr <- renderSlickR({
            photo_list <- lapply(sp_infos$images, function(x){
                tags$div(
                    tags$img(src = x, width = "20%", height = "20%"),
                    tags$script(src = "titlescript.js")
                )
            })
            
            imgs <- do.call(tagList, photo_list) # plotsAsSVG
            slickR(imgs) # slickR(imgs)
        })
        
        # Description ----
        output$selected_sp_description <- renderUI({
            HTML(paste("<b>",
                       floraweb_species[which(floraweb_species$SPECIES == input$plant_list),
                                        "TAXONNAME"], "</b>",
                       sp_infos$description, sep = '<br/>'))
        })
        
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
                options <- pmatch(c("Map", "Chorology"), input$options)
                output$selected_sp_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if(!is.na(options[1])){
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
        isolate({
            observe({
                options <- pmatch(c("Map", "Chorology"), input$options)
                output$selected_sp_chorology <- renderUI({
                    par(mar = rep(0.5, 4), oma = rep(0, 4))
                    plot.new()
                    if(!is.na(options[2])){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              species_list$NAMNR[j],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    }
                })
            })
        })
        
    }) # closes observe()
    
    # 2. Quizz ----
    
    answered <- FALSE # an indicator for whether question has been answered
    
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
            hints_floraweb = c("description", "status", "habitat", "family",
                               "German name"), 
            hints_custom = NULL, imagelink_custom = NULL,
            image_folders = "www/pictures_Clemens/images_320",
            # image_folders = "~/ShinyApps/BotanizeR/WWW/pictures_Clemens/images_320", # This is needed on server; 
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
        
        # Description ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_description <- renderUI({
                    if(!is.na(quizz_options[1])){
                        HTML(sp_quizz$description)
                    }
                })
            })
        })
        
        # Habitat ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_habitat <- renderText({
                    if(!is.na(quizz_options[4])){
                        print(sp_quizz$habitat[[1]])
                    }
                })
            })
        })
        
        # Family ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_family <- renderText({
                    if(!is.na(quizz_options[3])){
                        print(sp_quizz$family)
                    }
                })
            })
        })
        
        # Status ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_status <- renderText({
                    if(!is.na(quizz_options[2])){
                        print(sp_quizz$status)
                    }
                })
            })
        })
        
        # German name ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_german <- renderText({
                    if(!is.na(quizz_options[5])){
                        print(sp_quizz$`German name`)
                    }
                })
            })
        })
        
        # Map ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if(!is.na(quizz_options[6])){
                        # Downloading map only
                        random_map <- BotanizeR_collect(
                            species_row = floraweb_species[which(floraweb_species$SPECIES == species), ], 
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
        
        # Chorology ----
        isolate({
            observe({
                quizz_options <- pmatch(c("Description", "Status", "Family",
                                          "Habitat", "German name", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_chorology <- renderUI({
                    par(mar = rep(0.5, 4), oma = rep(0, 4))
                    plot.new()
                    if(!is.na(options[7])){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              species_list$NAMNR[j],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    }
                })
            })
        })
        
        # Answer ----
        # display text when no answer is provided
        observeEvent(input$newplant, {
            answered <- FALSE
            output$answer_status <- renderText({
                "Mark your answer and click 'Submit!'"
            })
        })
        
        # Providing an answer simple version
        observe({
            output$answer_status <- renderUI("Mark your answer and click 'Submit!'")
            observeEvent(input$submit, {
                isolate({
                    answer <- as.character(input$sp_answer)
                })
                if (answer == species){
                    output$answer_status <- renderUI(HTML(paste0(
                    "<font color=\"#00CC00\">", "Correct", "</font>")))
                } else if(answer != species){
                    output$answer_status <- renderUI(HTML(paste0(
                        "<font color=\"#FF0000\">", "Wrong", "</font>")))
                }
            })
            observeEvent(input$newplant, {
                output$answer_status <- renderUI("Mark your answer and click 'Submit!'")
            })
        })
        
        # Providing an answer
        # observe({
        #     
        #     # Counting the number of tries
        #     # Defining & initializing the reactiveValues object
        #     counter <- reactiveValues(countervalue = 0) 
        #     output$nb_tries <- renderText({"Number of tries: 0"})
        #     # observeEvent(input$submit, {
        #     if(!is.null(input$lastkeypresscode)){
        #         if(input$lastkeypresscode == 13){ # hitting Enter
        #             counter$countervalue <- counter$countervalue + 1
        #             output$nb_tries <- renderText({
        #                 paste0("Number of tries: ", counter$countervalue)})
        #             
        #             # Initial score
        #             counter <- reactiveValues(score = 0)
        #             renderText("Score = 0")
        #             # output$score <- 0
        #             
        #             # If provided answer is correct
        #             # observeEvent(input$sp_answer == species, {
        #             if (input$sp_answer == species){
        #                 output$status1 <- renderText({
        #                     ""
        #                 })
        #                 output$status2 <- renderText({
        #                     paste(generateResponse(1))
        #                 })
        #                 output$status3 <- renderText({
        #                     ""
        #                 })
        #                 
        #                 # Updating score
        #                 renderText({
        #                     counter$score <- counter$score + 1
        #                     paste0("Score: ", counter$score)
        #                     # output$score <- output$score + 1
        #                     # paste0("Score: ", output$score)
        #                 })
        #                 
        #                 # })
        #             }
        #         } # closes 'Hitting enter'
        #     } # closes !is.null lastkeybutton
        #     # })
        # })
        
        # Printing real answer ----
        observe({
            output$real_answer <- renderText("")
            observeEvent(input$real_answer, {
                output$real_answer <- renderText(species)
            })
            observeEvent(input$newplant, {
                output$real_answer <- renderText("")
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
