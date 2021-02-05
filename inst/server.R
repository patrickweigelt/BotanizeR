
# Packages
library(shiny)
library(BotanizeR)
# library(imager)
library(XML)
library(sf)
library(slickR)
library(htmltools)
library(shinyFiles)

shinyServer(function(input, output, session) {
    
    # 0. Preparation ----
    
    # Load starting config
    source("config.R")
    
    # List of species that have a chorology map
    chorology_list <- read.table("NAMNR_chorology.txt")
    
    # Sort species list alphabetically
    species_list <- species_list[order(species_list$SPECIES), c(1:14)]
    
    # Make species list a reactive object and allow for upload
    species_list_reactive <- reactiveValues(df_data = NULL)
    species_list_reactive$df_data <- species_list
    
    counts_reactive <- reactiveValues(init_count = 0,
                                      init_score = 0,
                                      init_count_species = 0,
                                      init_score_species = 0)

    hints_floraweb_lookup <- data.frame(variable = c("German name","family","status","description","habitat","map"),
                                        show = c("German name","Family","Status","Description","Habitat","Map"),
                                        stringsAsFactors = FALSE
    )
    
    hints_ukplantatlas_lookup <- data.frame(variable = c("familyuk","statusuk","ecology","trends","perennation","lifeform","woodiness","clonality","mapuk"),
                                        show = c("Family UK","Status UK","Ecology","Trend","Perennation","Life form","Woodiness","Clonality","Map UK"),
                                        stringsAsFactors = FALSE
    )
    
    if(!is.null(hints_floraweb)){
        hints_floraweb <- hints_floraweb_lookup$variable[which(hints_floraweb_lookup$variable %in% hints_floraweb)]
    }
    if(!is.null(hints_ukplantatlas)){
        hints_ukplantatlas <- hints_ukplantatlas_lookup$variable[which(hints_ukplantatlas_lookup$variable %in% hints_ukplantatlas)]
    }

    # Make hints a reactive object
    hints_reactive <- reactiveValues(image_floraweb = image_floraweb,
                                     hints_floraweb =  hints_floraweb,
                                     image_ukplantatlas = image_ukplantatlas,
                                     hints_ukplantatlas = hints_ukplantatlas,
                                     image_folders = image_folders,
                                     hints_custom = hints_custom,
                                     chorology = chorology)
    
    # French common names
    # fr_common <- read.table("nom_vernaculaires_cleaned.csv",
    #                        header = TRUE, sep = "\t", fill = TRUE)
    
    # 1. Setup ----
    
    # Online resources
    
    # Render checkboxes floraweb
    
    output$floraweb_images <- renderUI({
        checkboxGroupInput(inputId = "floraweb_images", label = "Germany Floraweb",
                           choices = c("Images"),
                           selected = c("Images")[image_floraweb])
    })
    output$floraweb_hints <- renderUI({
        checkboxGroupInput(inputId = "floraweb_hints", label = NULL,
                           choices = hints_floraweb_lookup$show,
                           selected = hints_floraweb_lookup$show[which(
                               hints_floraweb_lookup$variable %in% hints_floraweb)])
    })
    output$chorology_hint <- renderUI({
        checkboxGroupInput(inputId = "chorology_hint", label = NULL,
                           choices = c("Chorology"),
                           selected = c("Chorology")[(chorology == "chorology")])
    })
    
 
    # Render checkboxes UK Plant Atlas
    output$ukplantatlas_images <- renderUI({
        checkboxGroupInput(inputId = "ukplantatlas_images", label = "UK & Ireland Plant Atlas",
                           choices = c("Images"),
                           selected = c("Images")[image_ukplantatlas])
    })
    output$ukplantatlas_hints <- renderUI({
        checkboxGroupInput(inputId = "ukplantatlas_hints", label = NULL,
                           choices = hints_ukplantatlas_lookup$show,
                           selected = hints_ukplantatlas_lookup$show[which(
                               hints_ukplantatlas_lookup$variable %in% hints_ukplantatlas)])
    })

    
    
    
    # Change content of reactive hints ----
    observeEvent(input$floraweb_images, ignoreNULL = FALSE, ignoreInit = TRUE, {
        #print(paste("before:",hints_reactive$image_floraweb))
        #print(paste("input:" , input$floraweb_images))
        hints_reactive$image_floraweb <- ("Images" %in% input$floraweb_images)
        #print(paste("after:",hints_reactive$image_floraweb))
    })

    observeEvent(input$floraweb_hints, ignoreNULL = FALSE, ignoreInit = TRUE, {
        temp_variables <- hints_floraweb_lookup$variable[which(hints_floraweb_lookup$show %in% input$floraweb_hints)]
        hints_reactive$hints_floraweb <- hints_floraweb_lookup$variable[which(hints_floraweb_lookup$variable %in% temp_variables)]
    })

    observeEvent(input$chorology_hint, ignoreNULL = FALSE, ignoreInit = TRUE, {
        hints_reactive$chorology <- c("chorology")[("Chorology" %in% input$chorology_hint)]
    })
    
    observeEvent(input$ukplantatlas_images, ignoreNULL = FALSE, ignoreInit = TRUE, {
        hints_reactive$image_ukplantatlas <- ("Images" %in% input$ukplantatlas_images)
    })
    
    observeEvent(input$ukplantatlas_hints, ignoreNULL = FALSE, ignoreInit = TRUE, {
        temp_variables <- hints_ukplantatlas_lookup$variable[which(hints_ukplantatlas_lookup$show %in% input$ukplantatlas_hints)]
        hints_reactive$hints_ukplantatlas <- hints_ukplantatlas_lookup$variable[which(hints_ukplantatlas_lookup$variable %in% temp_variables)]
    })
    
    
    

    # image folder
    shinyDirChoose(input, 'image_folder', roots = c(wd = '.'),
                   filetypes = c('', 'txt'), allowDirCreate = FALSE)
    
    
    observeEvent(input$image_folder, {
        output$img_folders <-  renderText(paste(unlist(input$image_folder["path"]), collapse="/"))
        #print(str(input$image_folder))
        #print(unlist(input$image_folder["path"]))
        #print(typeof(input$image_folder["path"]))
    })
    
    
    
    
    # Choosing initial species list
    # drop down
    
    output$selectlist_note <- renderUI({
        HTML(paste0("<br>",
                    "Select a species list to start from:"))
    })
    
    
    output$select_specieslist <- renderUI({
        selectInput("select_specieslist", label = NULL,
                    choices = c("Germany_all","Germany_winter","Germany_summer","UK&Ireland_all"),
                    selected = "Germany_winter")
    })
    

    
    
    # Uploading progress
    output$upload_note <- renderUI({
        HTML(paste0("<br>",
                    "If you ran the quiz in a previous session and you saved your progress, 
                    you can upload your current scores as a .csv file here. You can also upload 
                    a modified species list with another set of species or your own hints."))
    })
    
    
    observeEvent(input$file, {
        species_list_uploaded <- read.csv(input$file$datapath)
        # write control for right columns in dataframe
        species_list_reactive$df_data <- species_list_uploaded[order(species_list_uploaded$SPECIES),]
        counts_reactive$init_count <- sum(species_list_uploaded$COUNT)
        counts_reactive$init_score <- sum(species_list_uploaded$SCORE)
        counts_reactive$init_count_species <- sum(species_list_uploaded$COUNT > 0)
        counts_reactive$init_score_species <- sum(species_list_uploaded$SCORE > 0)
    })
    
    
    # Downloading progress 
    output$download <- downloadHandler(
        filename = function(){"BotanizeR_practised.csv"}, 
        content = function(file){
            species_list_save <- species_list_reactive$df_data
            if(!answered_reactive$cheated){
                species_list_save$SCORE[i$i] <- species_list_save$SCORE[i$i] + answered_reactive$answered
            }
            write.csv(species_list_save, file, row.names = FALSE)
        }
    )
    
    
    output$download_note <- renderUI({
        HTML(paste0("<br>",
                    "Downloading the current species list allows you to save the progress 
                    you made during the quiz and load it the next time you practice to get 
                    species you are not yet familiar with shown more frequently.",
                    "<br>",
                    "You can also download the species list to modify it according to your needs."))
    })
    
    
    
    
    
    # Render dynamic quiz checkboxes
    #firstup <- function(x) { # function to turn first letter in capital letter
    #    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    #    return(x)
    #}
    
    # hints_quiz <- sapply(c(hints_floraweb, hints_custom, chorology), firstup)
    # hints_quiz_ordered <- c("German name", "Family", "Status", "Description",
    #                        "Habitat","Map", hints_custom) # Change order when hints_custom used
    
    # checkboxes_quiz <- reactive({
    #     hints_quiz_ordered[which(hints_quiz_ordered %in% hints_quiz)]
    # })
    
    output$quizz_options <- renderUI({
        checkboxGroupInput(inputId = "quizz_options", label = "Show:",
                           choices = c(hints_floraweb_lookup$show[which(
                               hints_floraweb_lookup$variable %in% 
                                   hints_reactive$hints_floraweb)],
                               hints_ukplantatlas_lookup$show[which(
                                   hints_ukplantatlas_lookup$variable %in% 
                                       hints_reactive$hints_ukplantatlas)],
                               hints_reactive$hints_custom))
    })
    
    
    # Render dynamic species list checkboxes
    #hints_species <- sapply(c(hints_floraweb, hints_custom, chorology), firstup)
    #hints_species_ordered <- c("Map", "Chorology")
    
    #checkboxes_species <- reactive({
    #    hints_species_ordered[which(hints_species_ordered %in% hints_species)]
    #})
    
    output$options <- renderUI({
        checkboxGroupInput(inputId = "options", label = "Show:",
                           choices = c("Map","Map UK","Chorology")[
                               which(c("map","mapuk","chorology") %in%
                                   c(hints_reactive$hints_floraweb,
                                     hints_reactive$hints_ukplantatlas,
                                     hints_reactive$chorology))
                                   ])
    })
    
    
    
    
    # 2. Selected species ----
    
    # Dynamic dropdown
    # choice_plants <- reactive({
    #     species_list_reactive$df_data$SPECIES
    # })
    
    output$select_plant <- renderUI({
        selectInput("plant_list", "Plant list",
                    choices = species_list_reactive$df_data$SPECIES,
                    selected = species_list_reactive$df_data$SPECIES[1])
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
        
        # Plant species chosen
        j <- which(isolate(species_list_reactive$df_data)$SPECIES == selected_species)
        
        # Download information with BotanizeR_collect()
        sp_infos <- BotanizeR_collect(
            species_row = isolate(species_list_reactive$df_data)[j, ], 
            image_floraweb = hints_reactive$image_floraweb,
            hints_floraweb = hints_reactive$hints_floraweb[which(hints_reactive$hints_floraweb!="map")],
            image_ukplantatlas = hints_reactive$image_ukplantatlas,
            hints_ukplantatlas = hints_reactive$hints_ukplantatlas[which(hints_reactive$hints_ukplantatlas!="map")],
            hints_custom = NULL, imagelink_custom = NULL,
            image_folders = image_folders,
            file_location = "temporary", only_links = TRUE)
        
        # Photos ----
        output$selected_sp_photo <- renderSlickR({
            if(length(sp_infos$images) == 0){
                sp_infos$images = "no_picture.png"
            }
            imgs <- slick_list(slick_div(sp_infos$images, 
                                         css = htmltools::css(width = "100%", 
                                                              margin.left = "auto", margin.right = "auto",
                                                              margin.bottom = "auto", margin.top = "auto"),
                                         type = "img",links = NULL))
            slickR(imgs, slideId = "slide_species")# + settings(centerMode = TRUE, slidesToShow = 1, )
        })
        
        # Name ----
        output$selected_sp_name <- renderUI({
            HTML(paste("<b>",
                       isolate(species_list_reactive$df_data)[j,"TAXONNAME"],
                       "</b>"))
        })
        

        # Description ----
        output$selected_sp_description <- renderUI({
            floraweb_link <- paste0(
                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                isolate(species_list_reactive$df_data)[j, "NAMNR"],
                "&")
            
            
            ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                        gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(selected_species))))
            
            temp_hints <- c(isolate(hints_reactive$hints_floraweb),
                            isolate(hints_reactive$hints_ukplantatlas),
                            isolate(hints_reactive$hints_custom))
            temp_hints <- temp_hints[which(!temp_hints %in% c("map","mapuk"))]
            
            temp_hints <- paste0(unlist(sapply(sp_infos[names(sp_infos) %in% temp_hints],
                                               function(x) c(x,"</br></br>"))), collapse="")
            
            HTML(paste0(temp_hints,
                        ifelse(length(hints_reactive$hints_floraweb)>0|
                                   length(hints_reactive$hints_ukplantatlas)>0,
                               "<b>Source:</b></br>",""),
                        ifelse(length(hints_reactive$hints_floraweb)>0,
                               paste0("<a href='",
                                      floraweb_link, # https://www.floraweb.de/,
                                      "' target=_blank>FloraWeb</a></br>")
                               ,""),
                        ifelse(length(hints_reactive$hints_ukplantatlas)>0,
                               paste0("<a href='",
                                      ukplantatlas_link, # https://www.brc.ac.uk/,
                                      "' target=_blank>UK & Ireland Plant Atlas</a></br>")
                               ,"")
                        ))
        })
        

        # Map ----
        isolate({
            observe({
                # options <- pmatch(c("Map", "Map UK", "Chorology"), input$options)
                output$selected_sp_map <- renderPlot({
                    par(oma = c(0, 0, 0, 10.5))
                    plot.new()
                    if("Map" %in% input$options & !is.null(hints_floraweb)){
                        # Downloading map only
                        sp_map <- BotanizeR_collect(
                            species_row = isolate(species_list_reactive$df_data)[j, ], 
                            image_floraweb = FALSE,
                            hints_floraweb = ifelse("map" %in% hints_reactive$hints_floraweb, "map", NULL),
                            image_ukplantatlas = FALSE,
                            hints_ukplantatlas = NULL,
                            hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                            file_location = "temporary", only_links = TRUE)
                        
                        par(oma = c(0, 0, 0, 10.5))
                        if(length(sp_map$map)>0){
                            plot(sp_map$map[[1]], pal = sp_map$map[[2]],
                                 key.pos = 4, main = "")
                        }
                    }
                })
            })
        })
        
        # Chorology ----
        isolate({
            observe({
                # options <- pmatch(c("Map", "Chorology"), input$options)
                output$selected_sp_chorology <- renderUI({
                    if("Chorology" %in% input$options &
                       isolate(species_list_reactive$df_data)$NAMNR[j] %in% chorology_list$V1){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              isolate(species_list_reactive$df_data)$NAMNR[j],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    } else if("Chorology" %in% input$options &
                              !(isolate(species_list_reactive$df_data)$NAMNR[j] %in% chorology_list$V1)){
                        tags$img(src = "no_chorology.png",
                                 width = "200px", height = "50px")
                    }
                })
            })
        })
        
    }) # closes observe()
    
    
    # 3. Quiz ----
    
    # Setup reactive values 
    answered_reactive <- reactiveValues(answered = FALSE, cheated = FALSE)
    i <- reactiveValues(i=NA)
    reactive_species <- reactiveValues(species=NA)

    observeEvent(input$newplant, ignoreNULL = FALSE, {
        sp_picture <- 0
        
        if(!is.na(i$i) & !answered_reactive$cheated){
            species_list_reactive$df_data$SCORE[i$i] <- species_list_reactive$df_data$SCORE[i$i] + answered_reactive$answered
        }
        print(paste("SCORE = ",sum(species_list_reactive$df_data$SCORE)))
        
        while (sp_picture == 0) { # If no picture available => new plant
            # random species
            temp1 <- species_list_reactive$df_data
            reactive_species$species <- sample(temp1$SPECIES, 1, 
                                               prob = ((temp1$COUNT - temp1$SCORE + 1)/
                                                           (temp1$SCORE+1))*temp1$INCLUDE)
            i$i <- which(temp1$SPECIES == reactive_species$species)
            print(i$i)
            
            # Download information with BotanizeR_collect()
            sp_quizz <- BotanizeR_collect(
                species_row = temp1[i$i, ], 
                image_floraweb = image_floraweb,
                hints_floraweb = hints_floraweb[which(hints_floraweb!="map")], 
                hints_custom = NULL, imagelink_custom = NULL,
                image_folders = image_folders,
                file_location = "temporary", only_links = TRUE)
            
            if(length(sp_quizz$images) != 0){
                sp_picture <- 1
                
                # Randomly reordering pictures for the quiz
                sp_quizz$images <- sample(sp_quizz$images)
            }
        }
        
        
        # answered <- FALSE
        output$answer_status <- renderUI({
            HTML(paste0("Mark your answer and click 'Submit' or hit 'Enter'!",
                        "<br>", "Click 'New plant' or hit 'Arrow up' for next species.",
                        "</br><br>",
                        "Click 'Answer' or hit 'Arrow down' to get the answer.", "</br>"))
        })
        updateTextInput(session, "sp_answer", "Species name", value = "")
        
        # setting back answer text
        output$real_answer_print <- renderText("")
        # output$fr_common_name <- renderText("")
        
        # counting
        species_list_reactive$df_data$COUNT[i$i] <- species_list_reactive$df_data$COUNT[i$i] + 1
        print(paste("COUNT = ",sum(species_list_reactive$df_data$COUNT)))
        answered_reactive$cheated <- FALSE
        print(paste("cheated = ", answered_reactive$cheated))
        answered_reactive$answered <- FALSE
        print(paste("answered = ", answered_reactive$answered))
        
        
        # setting back checkboxes
        updateCheckboxGroupInput(session,
                                 inputId = "quizz_options",
                                 choices = c(hints_floraweb_lookup$show[which(
                                     hints_floraweb_lookup$variable %in% 
                                         hints_reactive$hints_floraweb)],
                                     hints_ukplantatlas_lookup$show[which(
                                         hints_ukplantatlas_lookup$variable %in% 
                                             hints_reactive$hints_ukplantatlas)],
                                     hints_custom),
                                 selected = NULL)
        
        
        # Photos ----
        
        output$random_slickr <- renderSlickR({
            imgs_quizz <- slick_list(slick_div(sp_quizz$images, css = htmltools::css(width = "100%", 
                                                                                     margin.left = "auto", margin.right = "auto",
                                                                                     margin.bottom = "auto", margin.top = "auto"),
                                               type = "img",links = NULL))
            slickR(imgs_quizz, slideId = "slide_quiz")# + settings(centerMode = TRUE, slidesToShow = 1, )
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
        #isolate({
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
        #})
        
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
                            species_list_reactive$df_data[i$i, "NAMNR"],
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
                            species_row = species_list_reactive$df_data[i$i, ], 
                            image_floraweb = FALSE,
                            hints_floraweb = ifelse("map" %in% hints_floraweb, "map", NULL), 
                            hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                            file_location = "temporary", only_links = TRUE)
                        
                        par(oma = c(0, 0, 0, 10.5))
                        if(length(random_map$map)>0){
                            plot(random_map$map[[1]], pal = random_map$map[[2]],
                                 key.pos = 4, main = "")
                        }
                    }
                })
            })
        })
    })
    
    # Answer ----
    # display text when no answer is provided
    
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
            if (tolower(answer) == tolower(reactive_species$species)){
                output$answer_status <- renderUI(HTML(paste0(
                    "<font color=\"#00CC00\">", "Correct", "</font>")))
                
                # Setting answered
                answered_reactive$answered = TRUE
                print(paste("answered = ", answered_reactive$answered))
                
                
            } else { 
                char_diff <-
                    paste0(adist(tolower(answer), tolower(reactive_species$species)),
                           ifelse(adist(tolower(answer), tolower(reactive_species$species)) > 1,
                                  " characters"," character"),
                           " different")
                
                genus <- species_list_reactive$df_data[i$i, "GENUS"]
                
                if(nchar(answer)>0){
                    genus_correct <- paste0(
                        ifelse(strsplit(tolower(answer), " ")[[1]][1] == tolower(genus),
                               "Genus correct", ""))
                } else {
                    genus_correct <- "" 
                }
                
                output$answer_status <- renderUI(HTML(paste0(
                    "<font color=\"#FF0000\">", char_diff,
                    "</font><font color=\"#00CC00\"><br>",
                    genus_correct, "</font></br>")))
            }
        })
    })
    
    
    
    # Real answer ----
    observeEvent(input$real_answer, {
        output$real_answer_print <- renderText(reactive_species$species)
        
        # French common names
        # fr_test <- as.character(
        #     fr_common[grepl(reactive_species$species,
        #                     fr_common$Nom.scientifique),
        #               "Nom.vernaculaire"])
        # output$fr_common_name <- renderText(fr_test)
        
        if(!answered_reactive$answered){
            answered_reactive$cheated <- TRUE 
            print(paste("cheated ", answered_reactive$cheated))
        }
    })
    
    # Sum.stats ----
    observe({
        # Total counts, unique species and score
        total_count <- sum(species_list_reactive$df_data$COUNT)
        total_species <- sum(species_list_reactive$df_data$COUNT > 0)
        
        total_score <- sum(species_list_reactive$df_data$SCORE)
        if(answered_reactive$answered){
            total_score <- total_score + 1
        }
        
        # Session counts, unique species and score
        session_count <- total_count - counts_reactive$init_count
        session_species <- total_species - counts_reactive$init_count_species
        session_score <- total_score - counts_reactive$init_score
    
    output$stats_barplot <- renderPlot({
        barplot_stats_session <- c(session_count, session_score)
        names(barplot_stats_session) <- c("Count", "Score")
        
        barplot_stats_all <- c(total_count, total_score)
        names(barplot_stats_all) <- c("Count", "Score")
        
        par(mfrow = c(1, 2), lwd = 2)
        barplot(barplot_stats_session, col = c("grey", "chartreuse3"),
                main = "Current session")
        barplot(barplot_stats_all, col = c("grey", "chartreuse3"),
                main = "Total")
    })

    output$stats_text <- renderPrint({
        HTML(paste0("<br>", "In this session, you practised <b>",
                    session_count,
                    "</b> pictures (for ", session_species,
                    " different species) and guessed <b>",
                    session_score, "</b> right.", "</br><br>",
                    "In total, you practised <b>", total_count,
                    "</b> pictures (for ", total_species,
                    " different species) and guessed <b>",
                    total_score, "</b> right.</br>"))
    })
    })
    # Download ----
    observeEvent(input$upanddown_button, {
        showModal(modalDialog(
            title = "Up and Download",
            HTML(paste0("Please navigate to the 'setup' tab to up or download your progress.",
                        "<br>","<br>",
                        "If you ran the quiz in a previous session and you saved your progress, 
                          you can upload your current scores as a .csv file there. You can also 
                          upload a modified species list with another set of species or your own hints.",
                        "<br>","<br>",
                        "Downloading the current species list allows you to save the progress 
                          you made during the quiz and load it the next time you practice to get 
                          species you are not yet familiar with shown more frequently. 
                          You can also modify the downloaded species list according to your needs.")),
            easyClose = TRUE
        ))
    })
    
})
