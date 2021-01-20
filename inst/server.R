
# Packages
library(shiny)
library(BotanizeR)
# library(imager)
library(XML)
library(sf)
library(slickR)

shinyServer(function(input, output, session) {
    
    # 0. Config ----
    # List of species
    data(floraweb_species)
    species_list <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                               floraweb_species$BioDiv2005==1), ]
    
    ## hints and images
    # floraweb:
    image_floraweb = TRUE
    hints_floraweb = c("map","description", "status", "habitat", "family",
                       "German name")
    
    ## Winter
    # species_list <- floraweb_species[which(floraweb_species$WINTER==1), ] # for winter list
    # image_floraweb = FALSE # for winter list
    # hints_floraweb = NULL # for winter list
    
    
    image_folders = c("www/pictures_Clemens_400", "www/drawings_Schulz_400")
    # image_folders = c("~/ShinyApps/BotanizeR/WWW/pictures_Clemens_400", "~/ShinyApps/BotanizeR/WWW/drawings_Schulz_400")
    # This is needed on server
    
    # List of species that have a chorology
    chorology_list <- read.table("NAMNR_chorology.txt")
    # chorology_list <- read.table("~/ShinyApps/BotanizeR/NAMNR_chorology.txt")
    # This is needed on server
    
    # Species
    species_list <- species_list[order(species_list$SPECIES),]
    plant_list <- species_list$SPECIES
    
    
    # 1. Selected species ----
    # Plant list
    output$plant_list <- renderPrint({plant_list})
    
    # output$list_or_random <- renderUI({
    #     validate(
    #         need(!is.null(input$radio), "Please select a input type")
    #     )
    #     if(input$radio == "plant_list"){
    #         selectInput("plant_list", "Plant list",
    #                     choices = plant_list,
    #                     selected = "Acer campestre")
    #         # br(),
    #         checkboxGroupInput(inputId = "options",
    #                            label = "Show:",
    #                            choices = list("Map", "Chorology"))
    #     } else if(input$radio == "random_plant"){
    #         actionButton("random_plant", "Random plant")
    #         selected_species <- sample(species_list$SPECIES, 1)
    #     }
    # }) # closes output$list_or_random
    
    observe({
        selected_species <- input$plant_list
        
        # observeEvent(input$newplant, {
        #     selected_species <- sample(species_list$SPECIES, 1)
        # })
        
        # Plant species chosen
        j <- which(species_list$SPECIES == selected_species)
        
        # Download informations with BotanizeR_collect()
        sp_infos <- BotanizeR_collect(
            species_row = species_list[which(species_list$SPECIES == selected_species), ], 
            image_floraweb,
            hints_floraweb = hints_floraweb[which(hints_floraweb!="map")], 
            hints_custom = NULL, imagelink_custom = NULL,
            image_folders,
            file_location = "temporary", only_links = TRUE)
        
        # Photos ----
        output$selected_sp_photo <- renderUI({
            if(length(sp_infos$images) > 1){
                par(mar = rep(0.5, 4), oma = rep(0, 4))
                photo_list <- lapply(sp_infos$images, function(x){
                    tags$div(
                        tags$img(src = x, width = "50%", height = "50%"),
                        tags$script(src = "titlescript.js")
                    )
                })
                do.call(tagList, photo_list)
            } else{ 
                # HTML("<strong>No picture available</strong>")
            }
        })
        
        # trying slickr
        output$slickr <- renderSlickR({
            if(length(sp_infos$images) >= 1){
                photo_list <- lapply(sp_infos$images, function(x){
                    tags$div(
                        tags$img(src = x, width = "20%", height = "20%") #,
                        # tags$script(src = "titlescript.js")
                    )
                })
            } else{
                photo_list <- list(tags$div(
                    tags$img(src = "no_picture.png",
                             width = "200px", height = "50px"),
                    tags$script(src = "titlescript.js")
                ))
            }
            imgs <- do.call(tagList, photo_list)
            slickR(imgs)# + settings(centerMode = TRUE, slidesToShow = 1, )
        })
        
        # German name ----
        output$selected_sp_german <- renderUI({
            HTML(paste("<b>",
                       species_list[which(species_list$SPECIES == selected_species),
                                    "TAXONNAME"], "</b>",
                       sp_infos$`German name`, sep = '<br/>'))
        })
        
        # Family ----
        output$selected_sp_family <- renderText({
            print(sp_infos$family[[1]])
        })
        
        # Status ----
        output$selected_sp_status <- renderText({
            print(sp_infos$status)
        })
        
        # Description ----
        output$selected_sp_description <- renderUI({
            floraweb_link <- paste0(
                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                species_list[which(species_list$SPECIES == selected_species), "NAMNR"],
                "&")
            
            HTML(paste0("<b>",
                        sp_infos$description, "</b>",
                        "Source: ",
                        "<a href='",
                        floraweb_link, # https://www.floraweb.de/,
                        "' target=_blank>FloraWeb</a>"))
        })
        
        # Habitat ----
        output$selected_sp_habitat <- renderText({
            print(sp_infos$habitat[[1]])
        })
        
        # Map ----
        isolate({
            observe({
                options <- pmatch(c("Map", "Chorology"), input$options)
                output$selected_sp_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if(!is.na(options[1]) & !is.null(hints_floraweb)){
                        # Downloading map only
                        sp_map <- BotanizeR_collect(
                            species_row = species_list[which(species_list$SPECIES == selected_species), ], 
                            image_floraweb = FALSE,
                            hints_floraweb = ifelse("map" %in% hints_floraweb, "map", NULL), 
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
                    if(!is.na(options[2]) &
                       species_list$NAMNR[j] %in% chorology_list$V1){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              species_list$NAMNR[j],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    } else if(!is.na(options[2]) &
                              !(species_list$NAMNR[j] %in% chorology_list$V1)){
                        tags$img(src = "no_chorology.png",
                                 width = "200px", height = "50px")
                    }
                })
            })
        })
        
    }) # closes observe()
    
    # 2. Quizz ----
    
    answered <- FALSE # an indicator for whether question has been answered
    
    my_progress <- data.frame(species = NULL)
    output$progress <- renderDataTable({progress()})
    
    observe({
        input$newplant # hitting the new plant button
        
        # Unchecking the checkboxes when hitting 'New plant'
        observeEvent(input$newplant, {
            updateCheckboxGroupInput(session,
                                     inputId = "quizz_options",
                                     choices = list("German name", "Family",
                                                    "Status", "Description",
                                                    "Habitat", "Map",
                                                    "Chorology"),
                                     selected = NULL)
        })
        
        sp_picture <- 0
        
        while (sp_picture == 0) { # If no picture available => new plant
            # random species
            species <- sample(species_list$SPECIES, 1)
            i <- which(species_list$SPECIES == species)
            
            # Download informations with BotanizeR_collect()
            sp_quizz <- BotanizeR_collect(
                species_row = species_list[which(species_list$SPECIES == species), ], 
                image_floraweb,
                hints_floraweb = hints_floraweb[which(hints_floraweb!="map")], 
                hints_custom = NULL, imagelink_custom = NULL,
                image_folders,
                file_location = "temporary", only_links = TRUE)
            
            if(length(sp_quizz$images) != 0){
                sp_picture <- 1
            }
        }
        
        # Photos ----
        # output$random_sp <- renderUI({
        #     par(mar = rep(0.5, 4), oma = rep(0, 4))
        #     photo_random <- lapply(sp_quizz$images, function(x){
        #         tags$div(
        #             tags$img(src = x, width = "50%", height = "50%"),
        #             tags$script(src = "titlescript.js")
        #         )
        #     })
        #     do.call(tagList, photo_random)
        # })
        
        output$random_slickr <- renderSlickR({
            photo_list <- lapply(sp_quizz$images, function(x){
                tags$div(
                    tags$img(src = x, width = "20%", height = "20%")
                )
            })
            imgs <- do.call(tagList, photo_list)
            slickR(imgs)
        })
        
        # German name ----
        isolate({
            observe({
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_german <- renderText({
                    if(!is.na(quizz_options[1])){
                        print(sp_quizz$`German name`)
                    }
                })
            })
        })
        
        # Family ----
        isolate({
            observe({
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_family <- renderText({
                    if(!is.na(quizz_options[2])){
                        print(sp_quizz$family)
                    }
                })
            })
        })
        
        # Status ----
        isolate({
            observe({
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_status <- renderText({
                    if(!is.na(quizz_options[3])){
                        print(sp_quizz$status)
                    }
                })
            })
        })
        
        # Description ----
        isolate({
            observe({
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_description <- renderUI({
                    if(!is.na(quizz_options[4])){
                        # HTML(sp_quizz$description)
                        
                        floraweb_link_quizz <- paste0(
                            "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                            species_list[which(species_list$SPECIES == species), "NAMNR"],
                            "&")
                        
                        HTML(paste0(sp_quizz$description,
                                    "</br>",
                                    "Source: ",
                                    "<a href='",
                                    floraweb_link_quizz, # https://www.floraweb.de/,
                                    "' target=_blank>FloraWeb</a>"))
                    }
                })
            })
        })
        
        # Habitat ----
        isolate({
            observe({
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_habitat <- renderText({
                    if(!is.na(quizz_options[5])){
                        print(sp_quizz$habitat[[1]])
                    }
                })
            })
        })
        
        # Map ----
        isolate({
            observe({
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if(!is.na(quizz_options[6]) & !is.null(hints_floraweb)){
                        # Downloading map only
                        random_map <- BotanizeR_collect(
                            species_row = species_list[which(species_list$SPECIES == species), ], 
                            image_floraweb = FALSE,
                            hints_floraweb = ifelse("map" %in% hints_floraweb, "map", NULL), 
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
                quizz_options <- pmatch(c("German name", "Family", "Status",
                                          "Description", "Habitat", "Map",
                                          "Chorology"),
                                        input$quizz_options)
                output$random_chorology <- renderUI({
                    if(!is.na(quizz_options[7]) &
                       species_list$NAMNR[i] %in% chorology_list$V1){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              species_list$NAMNR[i],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    } else if(!is.na(quizz_options[7]) &
                              !(species_list$NAMNR[i] %in% chorology_list$V1)){
                        tags$img(src = "no_chorology.png",
                                 width = "200px", height = "50px")
                    }
                })
            })
        })
        
        # Answer ----
        # display text when no answer is provided
        observeEvent(input$newplant, {
            answered <- FALSE
            output$answer_status <- renderUI({
                HTML(paste0("Mark your answer and click 'Submit' or hit 'Enter'!",
                            "<br>", "Click 'New plant' or hit 'Arrow up' for next species.",
                            "</br><br>",
                            "Click 'Answer' or hit 'Arrow down' to get the answer.", "</br>"))
            })
            updateTextInput(session, "sp_answer", "Species name", value = "")
        })
        
        # Providing an answer simple version
        observe({
            output$answer_status <- renderUI({
                HTML(paste0("Mark your answer and click 'Submit' or hit 'Enter'!",
                            "<br>", "Click 'New plant' or hit 'Arrow up' for next species.",
                            "</br><br>",
                            "Click 'Answer' or hit 'Arrow down' to get the answer.", "</br>"))
            })
            observeEvent(input$submit, {
                isolate({
                    answer <- as.character(input$sp_answer)
                })
                if (answer == species){
                    output$answer_status <- renderUI(HTML(paste0(
                        "<font color=\"#00CC00\">", "Correct", "</font>")))
                } else if (answer != species){
                    char_diff <-
                        paste0(adist(answer, species),
                               ifelse(adist(answer, species) > 1,
                                      "characters","character"),
                               " different")
                    
                    genus <- species_list[which(species_list$SPECIES == species), "GENUS"]
                    
                    genus_correct <- paste0(
                        ifelse(strsplit(answer, " ")[[1]][1] == genus,
                               "Genus correct", ""))
                    
                    output$answer_status <- renderUI(HTML(paste0(
                        "<font color=\"#FF0000\">", char_diff,
                        "</font><font color=\"#00CC00\"><br>",
                        genus_correct, "</font></br>")))
                }
            })
            observeEvent(input$newplant, {
                output$answer_status <- renderUI({
                    HTML(paste0("Mark your answer and click 'Submit' or hit 'Enter'!",
                                "<br>", "Click 'New plant' or hit 'Arrow up' for next species.",
                                "</br><br>",
                                "Click 'Answer' or hit 'Arrow down' to get the answer.", "</br>"))
                })
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
        
        # Real answer ----
        observeEvent(input$real_answer, {
            output$real_answer_print <- renderText(species)
        })
        observeEvent(input$newplant, {
            output$real_answer_print <- renderText("")
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
        
        # Progress ----
        progress <- eventReactive(input$newplant, {
            newrow <- data.frame(species = species)
            
            my_progress <<- rbind(my_progress, newrow)
            my_progress
        }, ignoreNULL = FALSE)
        
    }) # closing observe for new plant
    
    # Downloading progress table
    output$download <- downloadHandler(
        filename = function(){"practise.csv"}, 
        content = function(file){
            write.csv(my_progress, file, row.names = FALSE)
        }
    )
})
