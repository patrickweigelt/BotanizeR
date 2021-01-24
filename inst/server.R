
# Packages
library(shiny)
library(BotanizeR)
# library(imager)
library(XML)
library(sf)
library(slickR)
library(htmltools)

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
    hints_custom = NULL
    chorology = "Chorology"
    
    ## Winter
    # species_list <- read.csv("floraweb_species_winter.csv") # for winter list
    # image_floraweb = FALSE # for winter list
    # hints_floraweb =  c("German name", "family", "status") # for winter list
    # hints_floraweb =  NULL # for winter list
    
    
    image_folders = c("www/pictures_Clemens_400", "www/drawings_Schulz_400")
    # image_folders = c("~/ShinyApps/BotanizeR/WWW/pictures_Clemens_400", "~/ShinyApps/BotanizeR/WWW/drawings_Schulz_400")
    # This is needed on server
    
    # List of species that have a chorology map
    chorology_list <- read.table("NAMNR_chorology.txt")

    # Sort species list alphabetically
    species_list <- species_list[order(species_list$SPECIES),c(1:14)]
    
    
    # Make species list a reactive object and allow for upload
    species_list_reactive <- reactiveValues(df_data = NULL)
    species_list_reactive$df_data <- species_list
    
    observeEvent(input$file, {
        species_list_uploaded <- read.csv(input$file$datapath)
        species_list_reactive$df_data <- species_list_uploaded[order(species_list_uploaded$SPECIES),]
    })
    
    
    # Render dynamic quiz checkboxes
    firstup <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
    }
    
    hints_quiz <- sapply(c(hints_floraweb,hints_custom,chorology),firstup)
    hints_quiz_ordered <- c("German name","Family","Status","Description","Habitat","Map","Chorology", hints_custom) # Change order when hints_custom used
    
    checkboxes_quiz <- reactive({
        hints_quiz_ordered[which(hints_quiz_ordered %in% hints_quiz)]
    })
    
    output$quizz_options <- renderUI({
        checkboxGroupInput(inputId = "quizz_options", label = "Show:",
                           choices = checkboxes_quiz())
    })

    
    # Render dynamic species list checkboxes
    hints_species <- sapply(c(hints_floraweb,hints_custom,chorology),firstup)
    hints_species_ordered <- c("Map","Chorology")
    
    checkboxes_species <- reactive({
        hints_species_ordered[which(hints_species_ordered %in% hints_species)]
    })
    
    output$options <- renderUI({
        checkboxGroupInput(inputId = "options", label = "Show:",
                           choices = checkboxes_species())
    })
    
    
    
    
    # 1. Selected species ----

    # Dynamic dropdown
    choice_plants <- reactive({
        species_list_reactive$df_data$SPECIES
    })
    
    output$select_plant <- renderUI({
        selectInput("plant_list", "Plant list dyn",
                    choices = choice_plants(),
                    selected = choice_plants()[1])
    })
    
    # Plant list
    # output$plant_list <- renderPrint({plant_list})
    
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

        if(length(selected_species)==0){
            selected_species <- isolate(species_list_reactive$df_data)$SPECIES[1]
        }
        
        # observeEvent(input$newplant, {
        #     selected_species <- sample(species_list$SPECIES, 1)
        # })
        
        # Plant species chosen
        j <- which(isolate(species_list_reactive$df_data)$SPECIES == selected_species)
        
        # Download information with BotanizeR_collect()
        sp_infos <- BotanizeR_collect(
            species_row = isolate(species_list_reactive$df_data)[j, ], 
            image_floraweb,
            hints_floraweb = hints_floraweb[which(hints_floraweb!="map")], 
            hints_custom = NULL, imagelink_custom = NULL,
            image_folders,
            file_location = "temporary", only_links = TRUE)
        
        # Photos ----
        output$selected_sp_photo <- renderSlickR({
            if(length(sp_infos$images) == 0){
                sp_infos$images = "no_picture.png"
            }
            imgs <- slick_list(slick_div(sp_infos$images, css = htmltools::css(width = "100%", margin.left = "auto", margin.right = "auto"),type = "img",links = NULL))
            slickR(imgs, slideId = "slide_species")# + settings(centerMode = TRUE, slidesToShow = 1, )
        })
        
        # German name ----
        output$selected_sp_german <- renderUI({
            HTML(paste("<b>",
                       isolate(species_list_reactive$df_data)[j,
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
                isolate(species_list_reactive$df_data)[j, "NAMNR"],
                "&")
            
            HTML(paste0(sp_infos$description, "</br>",
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
                            species_row = isolate(species_list_reactive$df_data)[j, ], 
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
                       isolate(species_list_reactive$df_data)$NAMNR[j] %in% chorology_list$V1){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              isolate(species_list_reactive$df_data)$NAMNR[j],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    } else if(!is.na(options[2]) &
                              !(isolate(species_list_reactive$df_data)$NAMNR[j] %in% chorology_list$V1)){
                        tags$img(src = "no_chorology.png",
                                 width = "200px", height = "50px")
                    }
                })
            })
        })
        
    }) # closes observe()
    
    
    # 2. Quiz ----
    
    # answered <- FALSE # an indicator for whether question has been answered
    
    # my_progress <- data.frame(species = NULL)
    # output$progress <- renderDataTable({progress()})

    observe({
        input$newplant # hitting the new plant button
        
        # Unchecking the checkboxes when hitting 'New plant'
        observeEvent(input$newplant, {
            updateCheckboxGroupInput(session,
                                     inputId = "quizz_options",
                                     choices = checkboxes_quiz(),
                                     selected = NULL)
        })
        
        sp_picture <- 0
        
        while (sp_picture == 0) { # If no picture available => new plant
            # random species
            temp1 <- isolate(species_list_reactive$df_data)
            species <- sample(temp1$SPECIES, 1, 
                          prob = ((temp1$COUNT - temp1$SCORE + 1)/
                                      (temp1$SCORE+1))*temp1$INCLUDE)
            # species <- sample(species_list$SPECIES, 1, 
              #              prob = ((species_list$COUNT - species_list$SCORE + 1)/
                #                        (species_list$SCORE+1))*species_list$INCLUDE)
            i <- which(temp1$SPECIES == species)
            
            # Download information with BotanizeR_collect()
            sp_quizz <- BotanizeR_collect(
                species_row = temp1[i, ], 
                image_floraweb,
                hints_floraweb = hints_floraweb[which(hints_floraweb!="map")], 
                hints_custom = NULL, imagelink_custom = NULL,
                image_folders,
                file_location = "temporary", only_links = TRUE)
            
            if(length(sp_quizz$images) != 0){
                sp_picture <- 1
                
                # Randomly reordering pictures for the quiz
                sp_quizz$images <- sample(sp_quizz$images)
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
            imgs_quizz <- slick_list(slick_div(sp_quizz$images, css = htmltools::css(width = "100%", margin.left = "auto", margin.right = "auto"),type = "img",links = NULL))
            slickR(imgs_quizz, slideId = "slide_quiz")# + settings(centerMode = TRUE, slidesToShow = 1, )
        })
        
        observeEvent(input$newplant, once = TRUE, {
            # temp <- species_list_reactive$df_data
            # temp$COUNT[i] <- temp$COUNT[i] + 1
            # species_list_reactive$df_data <- temp
            species_list_reactive$df_data$COUNT[i] <- species_list_reactive$df_data$COUNT[i] + 1
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
                        floraweb_link_quizz <- paste0(
                            "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                            temp1[i, "NAMNR"],
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
                            species_row = temp1[i, ], 
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
                       temp1$NAMNR[i] %in% chorology_list$V1){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              temp1$NAMNR[i],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    } else if(!is.na(quizz_options[7]) &
                              !(temp1$NAMNR[i] %in% chorology_list$V1)){
                        tags$img(src = "no_chorology.png",
                                 width = "200px", height = "50px")
                    }
                })
            })
        })
        
        # Answer ----
        # display text when no answer is provided
        observeEvent(input$newplant, {
            # answered <- FALSE
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
                if (tolower(answer) == tolower(species)){
                    output$answer_status <- renderUI(HTML(paste0(
                        "<font color=\"#00CC00\">", "Correct", "</font>")))

                    #temp <- species_list_reactive$df_data
                    #temp$SCORE[i] <- temp$SCORE[i] + 1
                    #species_list_reactive$df_data <- temp
                    species_list_reactive$df_data$SCORE[i] <- species_list_reactive$df_data$SCORE[i] + 1
                    
                    
                } else { # if (answer != species){
                    char_diff <-
                        paste0(adist(tolower(answer), tolower(species)),
                               ifelse(adist(tolower(answer), tolower(species)) > 1,
                                      " characters"," character"),
                               " different")
                    
                    genus <- temp1[i, "GENUS"]
                    
                    genus_correct <- paste0(
                        ifelse(strsplit(tolower(answer), " ")[[1]][1] == tolower(genus),
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
        # progress <- eventReactive(input$newplant, {
        #     newrow <- data.frame(species = species)
        #     
        #     my_progress <<- rbind(my_progress, newrow)
        #     my_progress
        # }, ignoreNULL = FALSE)

        #progress <- eventReactive(input$newplant, {
        #    species_list$COUNT[i] <- species_list$COUNT[i] + 1
        #    species_list
        #}, ignoreNULL = FALSE)
        
        
    }) # closing observe for new plant

    output$df_data_out <- renderTable(species_list_reactive$df_data)
    # output$progress <- renderDataTable(my_progress$species_list_progress)
    
    output$download <- downloadHandler(
        filename = function(){"BotanizeR_practised.csv"}, 
        content = function(file){
            write.csv(species_list_reactive$df_data, file, row.names = FALSE)
        }
    )
    # Downloading progress table
})
