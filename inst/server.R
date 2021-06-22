
# Packages
library(shiny)
library(BotanizeR)
# library(imager)
library(XML)
library(sf)
library(slickR)
library(htmltools)
library(shinyFiles)
library(httr)
# library(shinyjqui)
# library(shinyjs)

shinyServer(function(input, output, session) {
    
    # 0. Preparation ----
    
    # Load starting config
    source("config.R")
    
    # Load list of species that have a chorology map
    chorology_list <- read.table("NAMNR_chorology.txt")
    
    # Load initial species list
    if(species_list_path == ""){
        data("BotanizeR_species")
    } else {
        BotanizeR_species <- read.csv(paste0(system_path,species_list_path))
    }
    
    # Filter species list for predefined filter column
    if(species_list_selected != "All species"){
        species_list <- BotanizeR_species[which(BotanizeR_species[,species_list_selected]==1),]
    } else {
        species_list <- BotanizeR_species
    }
    
    # Order species list alphabetically 
    species_list <- species_list[order(species_list$SPECIES),]
    
    # Make species list a reactive object
    species_list_reactive <- reactiveValues(df_data = species_list, df_data_0 = species_list)
    species_list_uploaded_reactive <- reactiveValues(df_data = NULL)
    
    # Set reactive initial counts
    counts_reactive <- reactiveValues(init_count = 0,
                                      init_score = 0,
                                      init_count_species = 0,
                                      init_score_species = 0,
                                      omit = FALSE)
    
    # Define lookup tables for hint variables and their labels
    hints_floraweb_lookup <- data.frame(variable = c("German name","family","status","description","habitat","map"),
                                        show = c("German name","Family","Status","Description","Habitat","Map"),
                                        stringsAsFactors = FALSE
    )
    
    hints_ukplantatlas_lookup <- data.frame(variable = c("familyuk","statusuk","ecology","trends","perennation","lifeform","woodiness","clonality","mapuk"),
                                            show = c("Family UK","Status UK","Ecology","Trend","Perennation","Life form","Woodiness","Clonality","Map UK"),
                                            stringsAsFactors = FALSE
    )
    
    # Order hints accordingly
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
                                     hints_custom = hints_custom[
                                         which(hints_custom %in% 
                                                   grep("ownhint", colnames(species_list), 
                                                        value = TRUE))],
                                     imagelinks_custom = imagelinks_custom[
                                         which(imagelinks_custom %in% 
                                                   grep("imagelink", colnames(species_list), 
                                                        value = TRUE))],
                                     chorology = chorology)
    
    # 1. Setup ----
    
    ## Online resources ----
    
    ### Render checkboxes ----
    
    # Floraweb
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

    # UK Plant Atlas
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

    ### Change content of reactive hints ----
    observeEvent(input$floraweb_images, ignoreNULL = FALSE, ignoreInit = TRUE, {
        hints_reactive$image_floraweb <- ("Images" %in% input$floraweb_images)
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
    
    ## Own resources ----

    hints_available <- function(x, grp) {
        grep(grp, colnames(x)[apply(x, 2, function(y) any(!is.na(y) & y!=""))], value = TRUE)
    }
    

    ### Own hints ----
    output$own_hints <- renderUI({
        checkboxGroupInput(inputId = "own_hints", label = "Own hints",
                           choices = isolate(hints_available(species_list_reactive$df_data, "ownhint")),
                           selected = hints_custom[which(hints_custom %in%
                                                             grep("ownhint", colnames(species_list),
                                                                  value = TRUE))])
    })

    # output$own_hints <- renderUI({
    #     checkboxGroupInput(inputId = "own_hints", label = "Own hints",
    #                        choices = grep("ownhint", colnames(species_list),
    #                                               value = TRUE),
    #                        selected = hints_custom[which(hints_custom %in%
    #                                                          grep("ownhint", colnames(species_list),
    #                                                               value = TRUE))])
    # })
    
    observeEvent(input$own_hints, ignoreNULL = FALSE, ignoreInit = TRUE, { # OR ignoreInit = FALSE?
        hints_reactive$hints_custom <- input$own_hints
    })

    
    ## Own image links ----
    output$own_links <- renderUI({
        checkboxGroupInput(inputId = "own_links", label = "Own images",
                           choices = isolate(hints_available(species_list_reactive$df_data, "imagelink")),
                           selected = imagelinks_custom[which(imagelinks_custom %in%
                                                                  grep("imagelink", colnames(species_list),
                                                                       value = TRUE))])
    })

    # output$own_links <- renderUI({
    #     checkboxGroupInput(inputId = "own_links", label = "Own images",
    #                        choices = grep("imagelink", colnames(species_list),
    #                                               value = TRUE),
    #                        selected = imagelinks_custom[which(imagelinks_custom %in%
    #                                                               grep("imagelink", colnames(species_list),
    #                                                                    value = TRUE))])
    # })
    
    observeEvent(input$own_links, ignoreNULL = FALSE, ignoreInit = TRUE, { # OR ignoreInit = FALSE?
        hints_reactive$imagelinks_custom <- input$own_links
    })
    
    
    ### Image folder ----
    shinyDirChoose(input, 'image_folder', roots = c(wd = '.'),
                   filetypes = c('', 'png', 'PNG', 'jpeg', "JPEG", 'jpg', 'JPG'), 
                   allowDirCreate = FALSE)

    # Update reactive image folder
    observeEvent(input$image_folder, {
        if(!is.na(input$image_folder["path"][1])){
            hints_reactive$image_folders[length(hints_reactive$image_folders)+1] <- paste(
                unlist(input$image_folder["path"])[which(unlist(input$image_folder["path"]) != "")], collapse="/")
        }
    })
    
    # list image folders
    output$list_imagefolders <- renderUI({
        HTML(paste0("<i>",
                    paste0("</br>",
                           hints_reactive$image_folders)),
             "</i>")
    })
    
    # Remove last folder
    observeEvent(input$remove_folder, {
        if(length(hints_reactive$image_folders)>0){
            hints_reactive$image_folders <- hints_reactive$image_folders[-length(hints_reactive$image_folders)]
        }
    })

    
    ## Species list ----
    
    ### Chose from drop down ----
    
    # Text note
    output$selectlist_note <- renderUI({
        HTML(paste0("<br>",
                    "Select a species list:"))
    })
    
    # Render drop down
    output$select_specieslist <- renderUI({
        selectInput("select_specieslist", label = NULL,
                    choices = {if (length(species_list_uploaded_reactive$df_data) > 0)
                                   c(species_list_filter,"uploaded") else
                                       species_list_filter},
                    selected = ifelse(length(species_list_uploaded_reactive$df_data) > 0, "uploaded", species_list_selected))
    })

    # output$select_specieslist <- renderUI({
    #     selectInput("select_specieslist", label = NULL,
    #                 choices = species_list_filter,
    #                 selected = species_list_selected)
    # })
    
    # Make condition that input is needed to initialize drop_down
    y <- reactive({
        req(input$select_specieslist)
        input$select_specieslist
    })
    
    # Observe input
    observeEvent(y(), ignoreInit = TRUE, {
        if(input$select_specieslist == "All species"){
            temp_species_list <- BotanizeR_species
        } else if (input$select_specieslist == "uploaded"){
            temp_species_list <- species_list_uploaded_reactive$df_data
        } else {
            temp_species_list <- BotanizeR_species[which(BotanizeR_species[,input$select_specieslist]==1),]
        } 
        
        output$upload_error <- renderUI("")
        output$local_list_error <- renderUI("")
        
        species_list_reactive$df_data <- temp_species_list[order(temp_species_list$SPECIES),]
        species_list_reactive$df_data_0 <- temp_species_list[order(temp_species_list$SPECIES),]
        counts_reactive$init_count <- sum(temp_species_list$COUNT)
        counts_reactive$init_score <- sum(temp_species_list$SCORE)
        counts_reactive$init_count_species <- sum(temp_species_list$COUNT > 0)
        counts_reactive$init_score_species <- sum(temp_species_list$SCORE > 0)
        
        # Avoid that scores are updated when hitting next plant or download or summary
        counts_reactive$omit <- TRUE
        
        # Update ownhint checkboxes
        updateCheckboxGroupInput(session,
                                 inputId = "own_hints", label = "Own hints",
                                 choices = hints_available(temp_species_list, "ownhint"),
                                 selected = hints_reactive$hints_custom)
        
        # Update ownlink checkboxes
        updateCheckboxGroupInput(session,
                                 inputId = "own_links", label = "Own images",
                                 choices = hints_available(temp_species_list, "imagelink"),
                                 selected = hints_reactive$imagelinks_custom)
    })
    
    # Species list summary note
    output$summary_note <- renderUI({
        HTML(paste0("<i>",
                    nrow(species_list_reactive$df_data),
                    " species; ", 
                    sum(species_list_reactive$df_data$INCLUDE),
                    " included; ", 
                    sum(species_list_reactive$df_data$COUNT>0),
                    " practiced.</i>"))
    })

    ### Upload a species list ----
    
    sanitize_input <- function(x) {
        ext <- tools::file_ext(x)
        if(ext == "csv"){
            header <- read.csv(x, header=FALSE, nrows=1)
            if(all(c("TAXONNAME", "SPECIES", "GENUS") %in% header)){
                header <- header[1,] %in% c("NAMNR", "TAXONNAME", "SPECIES", "GENUS",
                                            "COUNT", "SCORE", "ATTEMPTS", "INCLUDE",
                                            grep("ownhint|imagelink", header[1,], value = TRUE))
                
                header <- ifelse(header, NA, "NULL")
                
                species_list_clean <- unique(read.csv(x, colClasses = header, nrows = 6000))
                
                if(nrow(species_list_clean)>0){
                    
                    if(all(apply(species_list_clean[,c('TAXONNAME','SPECIES','GENUS')], 2,function(x) all(!is.na(x) & x != "")))){
                        
                        if(length(which(duplicated(species_list_clean$SPECIES))) == 0){
                            
                            
                            if(!"NAMNR" %in% names(species_list_clean)) species_list_clean$NAMNR <- NA
                            if(!"COUNT" %in% names(species_list_clean)) species_list_clean$COUNT <- 0
                            if(!"SCORE" %in% names(species_list_clean)) species_list_clean$SCORE <- 0
                            if(!"ATTEMPTS" %in% names(species_list_clean)) species_list_clean$ATTEMPTS <- 0
                            if(!"INCLUDE" %in% names(species_list_clean)) species_list_clean$INCLUDE <- 1
                            
                            species_list_clean <- species_list_clean[order(species_list_clean$SPECIES),]
                            
                            if(all(apply(species_list_clean[,c('COUNT','SCORE','ATTEMPTS','INCLUDE')], 2,function(x) is.numeric(x) & all(!is.na(x))))){
                                return(species_list_clean)
                            } else {
                                return("Not all entries of the columns 'COUNT', 'SCORE', 'ATTEMPTS' and 'INCLUDE' are numeric.")
                            }
                        } else {
                            return("Duplicates in 'SPECIES' column found.")
                        }
                    } else {
                        return("Missing entries in at least one of the columns 'TAXONNAME', 'SPECIES' and 'GENUS'.")
                    }
                } else {
                    return("No entries found!")
                }
            } else {
                return("At least one of the columns 'TAXONNAME', 'SPECIES' and 'GENUS' is missing.")
            }
        } else {
            return("Please upload a *.csv file!")
        }
    }
    
    
    upload_text <- "If you ran the quiz in a previous session and you saved your progress, 
                    you can upload your current scores as a .csv file here. You can also upload 
                    a modified species list with another set of species or your own hints."

    output$upload_note <- renderUI({
        HTML(paste0("<br>",upload_text))
    })
    output$upload_error <- renderUI("")
    output$local_list_error <- renderUI("")
    
    # The second upload note in the quiz pop-up only works with its own output
    output$upload_note_2 <- renderUI({
        HTML(paste0("<br>",upload_text))
    })
    output$upload_error_2 <- renderUI("")
    
    
    observeEvent(input$file, {
        file <- input$file$datapath
        req(file)
        try(species_list_uploaded <- sanitize_input(file))
        
        req(species_list_uploaded)
        
        if(is.data.frame(species_list_uploaded)){
            output$upload_error <- renderUI("")
            output$upload_error_2 <- renderUI("")
            
            counts_reactive$omit <- TRUE
            
            species_list_reactive$df_data <- species_list_uploaded
            species_list_reactive$df_data_0 <- species_list_uploaded
            species_list_uploaded_reactive$df_data <- species_list_uploaded
            counts_reactive$init_count <- sum(species_list_uploaded$COUNT)
            counts_reactive$init_score <- sum(species_list_uploaded$SCORE)
            counts_reactive$init_count_species <- sum(species_list_uploaded$COUNT > 0)
            counts_reactive$init_score_species <- sum(species_list_uploaded$SCORE > 0)
            
            # update species list drop down
            updateSelectInput(session,
                           inputId = "select_specieslist", label = NULL,
                           choices = c(species_list_filter,"uploaded"),
                           selected = "uploaded")
            
            # Update ownhint checkboxes  
            updateCheckboxGroupInput(session,
                                     inputId = "own_hints", label = "Own hints",
                                     choices = hints_available(species_list_uploaded, "ownhint"),
                                     selected = hints_reactive$hints_custom)
            
            # Update ownlink checkboxes
            updateCheckboxGroupInput(session,
                                     inputId = "own_links", label = "Own images",
                                     choices = hints_available(species_list_uploaded, "imagelink"),
                                     selected = hints_reactive$imagelinks_custom)
            
        } else if (is.character(species_list_uploaded)){
            output$upload_error <- renderUI({
                HTML(paste0("<i>Species list could not be loaded. ",
                            species_list_uploaded,
                            "</i>"))
            })
        }
    })
    
    
    # The second upload button in the quiz pop-up only works with its own handler
    observeEvent(input$file_2, {
        file <- input$file_2$datapath
        req(file)
        try(species_list_uploaded <- sanitize_input(file))
        
        req(species_list_uploaded)
        
        if(is.data.frame(species_list_uploaded)){
            output$upload_error <- renderUI("")
            output$upload_error_2 <- renderUI({
                HTML(paste0("<i>",
                            nrow(species_list_uploaded),
                            " species; ", 
                            sum(species_list_uploaded$INCLUDE),
                            " included; ", 
                            sum(species_list_uploaded$COUNT>0),
                            " practiced.</i>"))
            })
            
            species_list_reactive$df_data <- species_list_uploaded
            species_list_reactive$df_data_0 <- species_list_uploaded
            species_list_uploaded_reactive$df_data <- species_list_uploaded
            counts_reactive$init_count <- sum(species_list_uploaded$COUNT)
            counts_reactive$init_score <- sum(species_list_uploaded$SCORE)
            counts_reactive$init_count_species <- sum(species_list_uploaded$COUNT > 0)
            counts_reactive$init_score_species <- sum(species_list_uploaded$SCORE > 0)
            
            hints_reactive$hints_custom <- hints_reactive$hints_custom[which(
                hints_reactive$hints_custom %in% colnames(species_list_uploaded))]
            
            hints_reactive$imagelinks_custom  <- hints_reactive$imagelinks_custom[which(
                hints_reactive$imagelinks_custom %in% colnames(species_list_uploaded))]
            
            counts_reactive$omit <- TRUE
            
            # # update specieslist drop down
            updateSelectInput(session,
                              inputId = "select_specieslist", label = NULL,
                              choices = c(species_list_filter,"uploaded"),
                              selected = "uploaded")

            # Update ownhint checkboxes  
            updateCheckboxGroupInput(session,
                                     inputId = "own_hints", label = "Own hints",
                                     choices = hints_available(species_list_uploaded, "ownhint"),
                                     selected = hints_reactive$hints_custom)
             
            # Update ownlink checkboxes
            updateCheckboxGroupInput(session,
                                     inputId = "own_links", label = "Own images",
                                     choices = hints_available(species_list_uploaded, "imagelink"),
                                     selected = hints_reactive$imagelinks_custom)
            
            # click("newplant", asis = TRUE) # gets executed before hints are updated and may cause error due to missing columns
        } else if (is.character(species_list_uploaded)){
            output$upload_error_2 <- renderUI({
                HTML(paste0("<i>Species list could not be loaded. ",
                            species_list_uploaded,
                            "</i>"))
            })
        }
    })
    
    ### Subset species list based on GBIF records for defined coordinates ----
    observeEvent(input$local_list, {
        #print(paste("Longitude:",input$longitude))
        #print(paste("Latitude:",input$latitude))
        
        output$local_list_error <- renderUI("")
        
        counts_reactive$omit <- TRUE
        
        try(species_list_local <- BotanizeR_getlocallist(lat = input$latitude, long = input$longitude, backbone_list = isolate(species_list_reactive$df_data_0)))
        
        if(exists("species_list_local")){
            if(nrow(species_list_local)>0){
                species_list_reactive$df_data <- species_list_local
                counts_reactive$init_count <- sum(species_list_local$COUNT)
                counts_reactive$init_score <- sum(species_list_local$SCORE)
                counts_reactive$init_count_species <- sum(species_list_local$COUNT > 0)
                counts_reactive$init_score_species <- sum(species_list_local$SCORE > 0)
            } else {
                output$local_list_error <- renderUI({
                    HTML("<i>No species from backbone list found for given coordinates!</i>")
                })
            }
        } else {
            output$local_list_error <- renderUI({
                HTML("<i>GBIF occurrences could not be loaded. Check coordinates!</i>")
            })
            
        }
    })
    
    ### Download a species list ----
    output$download <- downloadHandler(
        filename = function(){"BotanizeR_practised.csv"}, 
        content = function(file){
            species_list_save <- species_list_reactive$df_data
            if(!counts_reactive$omit & !answered_reactive$cheated){
                species_list_save$SCORE[isolate(i$i)] <- species_list_save$SCORE[isolate(i$i)] + answered_reactive$answered
            }
            write.csv(species_list_save, file, row.names = FALSE)
        }
    )

    # The second download button in the quiz pop-up only works with its own handler
    output$download_2 <- downloadHandler(
        filename = function(){"BotanizeR_practised.csv"}, 
        content = function(file){
            species_list_save <- species_list_reactive$df_data
            if(!counts_reactive$omit & !answered_reactive$cheated){
                species_list_save$SCORE[isolate(i$i)] <- species_list_save$SCORE[isolate(i$i)] + answered_reactive$answered
            }
            write.csv(species_list_save, file, row.names = FALSE)
        }
    )
    
    
    download_text <- paste0("Downloading the current species list allows you to save the progress 
                                  you made during the quiz and load it the next time you practice to get 
                                  species you are not yet familiar with shown more frequently.",
                                 "<br>",
                                 "You can also download the species list to modify it according to your 
                                  needs and upload it again.")
    
    output$download_note <- renderUI({
        HTML(paste0("<br>",download_text))
    })
    # The second upload note in the quiz pop-up only works with its own output
    output$download_note_2 <- renderUI({
        HTML(paste0("<br>",download_text))
    })
    
    
    
    # 2. Selected species ----
    
    ### Render Options ----
    
    # Dynamic dropdown
    output$select_plant <- renderUI({
        selectizeInput("plant_list", "Plant list",
                       choices = species_list_reactive$df_data$SPECIES,
                       selected = species_list_reactive$df_data$SPECIES[1],
                       options = list(maxOptions = length(species_list_reactive$df_data$SPECIES)))
    })
    
    # Dynamic checkboxes
    output$options <- renderUI({
        checkboxGroupInput(inputId = "options", label = "Show:",
                           choices = c("Map","Map UK","Chorology")[
                               which(c("map","mapuk","chorology") %in%
                                         c(hints_reactive$hints_floraweb,
                                           hints_reactive$hints_ukplantatlas,
                                           hints_reactive$chorology))
                               ])
    })

    # Dynamic map checkboxes
    output$options_maps <- renderUI({
        temp_options <- c("Map","Map UK","Chorology")[
            which(c("map","mapuk","chorology") %in%
                      c(hints_reactive$hints_floraweb,
                        hints_reactive$hints_ukplantatlas,
                        hints_reactive$chorology))
            ]
        
        if(length(temp_options)>0) {
            temp_options <- c("No map", temp_options)
        }
        
        radioButtons(inputId = "options_maps", label = NULL,
                     choices = temp_options,
                     selected = "No map")
    })

    ### Plant list ----

    # Previous plant
    observeEvent(input$previous_plant, {
        current_species <- which(species_list_reactive$df_data$SPECIES == input$plant_list)
        
        if(current_species > 1){
            updateSelectizeInput(session, "plant_list",
                                 choices = species_list_reactive$df_data$SPECIES,
                                 selected = species_list_reactive$df_data$SPECIES[current_species - 1],
                                 options = list(maxOptions = length(species_list_reactive$df_data$SPECIES)))
        }
    })
    
    # Next plant
    observeEvent(input$next_plant, {
        current_species <- which(species_list_reactive$df_data$SPECIES == input$plant_list)
        
        if(current_species < length(species_list_reactive$df_data$SPECIES)){
            updateSelectizeInput(session, "plant_list",
                                 choices = species_list_reactive$df_data$SPECIES,#[current_species + 1],
                                 selected = species_list_reactive$df_data$SPECIES[current_species + 1],
                                 options = list(maxOptions = length(species_list_reactive$df_data$SPECIES)))
        }
        
    })
    
    observe({
        req(input$plant_list)

        selected_species <- input$plant_list
        print(paste("List:",selected_species))
        
        # Plant species chosen
        j <- which(isolate(species_list_reactive$df_data)$SPECIES == selected_species)
        
        # Download information with BotanizeR_collect()
        sp_infos <- BotanizeR_collect(
            species_row = isolate(species_list_reactive$df_data)[j, ], 
            image_floraweb = isolate(hints_reactive$image_floraweb),
            hints_floraweb = isolate(hints_reactive$hints_floraweb[which(
                hints_reactive$hints_floraweb!="map")]),
            image_ukplantatlas = isolate(hints_reactive$image_ukplantatlas),
            hints_ukplantatlas = isolate(hints_reactive$hints_ukplantatlas[which(
                hints_reactive$hints_ukplantatlas!="mapuk")]),
            hints_custom = isolate(hints_reactive$hints_custom), 
            imagelinks_custom = isolate(hints_reactive$imagelinks_custom),
            image_folders = isolate(paste0(system_path,hints_reactive$image_folders,sep="")),
            only_links = TRUE)
        
        ### Photos ----
        output$selected_sp_photo <- renderSlickR({
            if(length(sp_infos$images) == 0){
                sp_infos$images = "no_picture.png"
            }
            imgs <- slick_list(slick_div(sp_infos$images, 
                                         css = htmltools::css(margin.left = "auto",
                                                              margin.right = "auto",
                                                              type = "img",links = NULL)))
            slickR(imgs, slideId = "slide_species") # + settings(adaptiveHeight = TRUE)
        })
        
        # Image credits
        output$selected_image_credits <- renderUI({
            sources <- c(
                ifelse(isolate(hints_reactive$image_floraweb), 
                       "<a href='https://www.floraweb.de/' target=_blank>FloraWeb</a>", 
                       NA),
                ifelse(isolate(hints_reactive$image_ukplantatlas), 
                       "<a href='https://www.brc.ac.uk/plantatlas/' 
                       target=_blank>UK & Ireland Plant Atlas</a>", 
                       NA),
                ifelse(length(isolate(hints_reactive$image_folders)) > 0, 
                       "customized image folders", 
                       NA)
            )
            
            sources <- sources[!is.na(sources)]
            
            if (length(sources)>1){
                sources <- sapply(sources, function(x) c(x,", "))
                sources <- sources[-(length(sources))]
                sources[length(sources)-1] <- " and "
            }
            
            sources <- ifelse(length(sources)>0,
                              paste0("Available images sourced from ",
                                     paste(sources, collapse = ""),
                                     ".</br>See 'About' tab for more details."),
                              "") 
            
            HTML(sources)
        })
        
        ### Name ----
        output$selected_sp_name <- renderUI({
            HTML(paste("<b>",
                       species_list_reactive$df_data[j,"TAXONNAME"],
                       "</b>"))
        })
        
        ### Description ----
        output$selected_sp_description <- renderUI({
            floraweb_link <- paste0(
                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                species_list_reactive$df_data[j, "NAMNR"],
                "&")

            ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                        gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(selected_species))))
            
            temp_hints_floraweb <- hints_reactive$hints_floraweb[which(hints_reactive$hints_floraweb != "map")]
            temp_hints_ukplantatlas <- hints_reactive$hints_ukplantatlas[which(hints_reactive$hints_ukplantatlas != "mapuk")]


            HTML(paste0(paste0(unlist(sapply(sp_infos[names(sp_infos) %in% temp_hints_floraweb],
                                             function(x) c(x,"</br></br>"))), collapse=""),
                        ifelse(length(temp_hints_floraweb)>0,
                               paste0("<b>Source:</b></br><a href='",
                                      floraweb_link, # https://www.floraweb.de/,
                                      "' target=_blank>FloraWeb</a></br></br>")
                               ,""),
                        paste0(unlist(sapply(sp_infos[names(sp_infos) %in% temp_hints_ukplantatlas],
                                            function(x) c(x,"</br></br>"))), collapse=""),
                        ifelse(length(temp_hints_ukplantatlas)>0,
                               paste0("<b>Source:</b></br><a href='",
                                      ukplantatlas_link, # https://www.brc.ac.uk/,
                                      "' target=_blank>UK & Ireland Plant Atlas</a></br></br>")
                               ,""),
                        paste0(unlist(sapply(sp_infos[names(sp_infos) %in% hints_reactive$hints_custom],
                                             function(x) c(x,"</br></br>"))), collapse="")
                        ))
        })

        output$selected_map_text <- renderUI({""})

        ### Map ----
        observe({
            output$selected_sp_map <- renderUI({
                if("Map" %in% input$options_maps){
                    # print(paste("List",input$options_maps))
                    map <- BotanizeR_collect(
                        species_row = isolate(species_list_reactive$df_data[j, ]), 
                        image_floraweb = FALSE, hints_floraweb = "map",
                        image_ukplantatlas = FALSE, hints_ukplantatlas = NULL,                    
                        hints_custom = NULL, imagelinks_custom = NULL, 
                        image_folders = NULL, only_links = TRUE)
                    
                    if(length(map$map)>0){
                        output$selected_map_text <- renderUI({
                            floraweb_link <- paste0(
                                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                                isolate(species_list_reactive$df_data)[j, "NAMNR"],
                                "&")
                            HTML(paste0("Map source: <a href='",
                                        floraweb_link, # https://www.floraweb.de/,
                                        "' target=_blank>FloraWeb</a></br></br>"))
                        })
                        output$plot_sp_map <- renderPlot({
                            par(oma = c(0, 0, 0, 11))
                            plot(map$map[[1]], pal = map$map[[2]],
                                 key.pos = 4, main = "")
                        })
                        plotOutput("plot_sp_map")
                    } else {
                        "No distribution map for Germany available!"
                    }
                } else if ("Map UK" %in% input$options_maps){
                    # print(paste("List",input$options_maps))
                    map <- BotanizeR_collect(
                        species_row = isolate(species_list_reactive$df_data[j, ]), 
                        image_floraweb = FALSE, hints_floraweb = NULL,
                        image_ukplantatlas = FALSE, hints_ukplantatlas = "mapuk",                    
                        hints_custom = NULL, imagelinks_custom = NULL, 
                        image_folders = NULL, only_links = TRUE)
                    
                    if(length(map$mapuk)>0){
                        output$selected_map_text <- renderUI({
                            ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                                        gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(selected_species))))
                            
                            HTML(paste0("Map from <i>New Atlas</i> by the Botanical Society of Britain and Ireland (blue: native, red: introduced). </br>For more details see: <a href='",
                                        ukplantatlas_link, # https://www.brc.ac.uk/,
                                        "' target=_blank>UK & Ireland Plant Atlas</a></br></br>"))
                        })
                        
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = map$mapuk,
                                 width = "500px")
                        
                    } else {
                        "No distribution map for the UK and Ireland available!"
                    }
                } else if ("No map" %in% input$options_maps){
                    # m$map <- TRUE
                    output$selected_map_text <- renderUI({""})
                    ""
                }
            })
        })

        ### Chorology ----
        isolate({
            observe({
                # options <- pmatch(c("Map", "Chorology"), input$options)
                output$selected_sp_chorology <- renderUI({
                    if("Chorology" %in% input$options_maps &
                       isolate(species_list_reactive$df_data)$NAMNR[j] %in% chorology_list$V1){
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = paste0("https://www.floraweb.de/bilder/areale/a",
                                              isolate(species_list_reactive$df_data)$NAMNR[j],
                                              ".GIF"),
                                 width = "400px", height = "300px")
                    } else if("Chorology" %in% input$options_maps &
                              !(isolate(species_list_reactive$df_data)$NAMNR[j] %in% chorology_list$V1)){
                        tags$img(src = "no_chorology.png",
                                 width = "200px", height = "50px")
                    }
                })
            })
        })
        
    }) # closes observe()

    
    # 3. Quiz ----

    ### Render quiz checkboxes ----
    
    name_hints <- function(x) {
        setNames(x, gsub("_"," ",gsub("ownhint_","",x)))
    }
    
    output$quiz_options <- renderUI({
        checkboxGroupInput(inputId = "quiz_options", label = "Show:",
                           choices = name_hints(c(hints_floraweb_lookup$show[which(
                               hints_floraweb_lookup$variable %in% 
                                   hints_reactive$hints_floraweb & 
                                   hints_floraweb_lookup$show != "Map")],
                               hints_ukplantatlas_lookup$show[which(
                                   hints_ukplantatlas_lookup$variable %in% 
                                       hints_reactive$hints_ukplantatlas & 
                                       hints_ukplantatlas_lookup$show != "Map UK")],
                               hints_reactive$hints_custom)))
    })

    # Dynamic map checkboxes
    output$quiz_options_maps <- renderUI({
        temp_options <- c("Map","Map UK")[which(c("map","mapuk") %in%
                                                    c(hints_reactive$hints_floraweb,
                                                      hints_reactive$hints_ukplantatlas))]
        if(length(temp_options)>0) {
            temp_options <- c("No map", temp_options)
        }
        
        radioButtons(inputId = "quiz_options_maps", label = NULL,
                     choices = temp_options,
                     selected = "No map")
    })
    
    
    # Setup reactive values 
    answered_reactive <- reactiveValues(answered = FALSE, cheated = FALSE)
    i <- reactiveValues(i=NA)
    reactive_species <- reactiveValues(species=NA)
    
    
    # Make hints a reactive object to avoid plotting infos of wrong species
    sp_quiz_reactive <- reactiveValues(sp_quiz=NA)
    
    # Workaround to avoid printing map before radiobuttons are set back to "no map"
    m <- reactiveValues(map=TRUE) #,hints=TRUE)
    

    # New plant observe
    observeEvent(input$newplant, ignoreNULL = FALSE, {
        
        # set map to false to not plot one before radiobuttons are set
        m$map <- FALSE
        # m$hints <- FALSE
        
        # setting back checkboxes
        updateCheckboxGroupInput(session,
                                 inputId = "quiz_options",
                                 choices = name_hints(c(hints_floraweb_lookup$show[which(
                                     hints_floraweb_lookup$variable %in% 
                                         hints_reactive$hints_floraweb & 
                                         hints_floraweb_lookup$show != "Map")],
                                     hints_ukplantatlas_lookup$show[which(
                                         hints_ukplantatlas_lookup$variable %in% 
                                             hints_reactive$hints_ukplantatlas & 
                                             hints_ukplantatlas_lookup$show != "Map UK")],
                                     hints_reactive$hints_custom)),
                                 selected = NULL)
        
        temp_options <- c("Map","Map UK")[which(c("map","mapuk") %in%
                                                    c(hints_reactive$hints_floraweb,
                                                      hints_reactive$hints_ukplantatlas))]
        if(length(temp_options)>0) {
            temp_options <- c("No map", temp_options)
        }
        
        updateRadioButtons(session,
                           inputId = "quiz_options_maps",
                           choices = temp_options,
                           selected = "No map")
        
        
        # Add score
        if(!counts_reactive$omit & !is.na(isolate(i$i)) & !answered_reactive$cheated){
            species_list_reactive$df_data$SCORE[isolate(i$i)] <- species_list_reactive$df_data$SCORE[isolate(i$i)] + answered_reactive$answered
        }
        
        counts_reactive$omit <- FALSE
        
        print(paste("SCORE =",sum(species_list_reactive$df_data$SCORE)-isolate(counts_reactive$init_score)))
        
        
        # Choose a species
        sp_picture <- 0
        k <- 0
        
        while (sp_picture == 0 & k <= 10) { # If no picture available => new plant
            
            # control for bad choice of species list and image hints (no images)
            k <- k + 1
            
            # random species
            temp_data <- isolate(species_list_reactive$df_data)
            i$i <- sample(1:nrow(temp_data), 1, prob = ((temp_data$COUNT - temp_data$SCORE + 1)/
                                                 (temp_data$SCORE+1))*temp_data$INCLUDE)
            
            temp_row <- temp_data[isolate(i$i),]
            
            reactive_species$species <- temp_row$SPECIES
            print(paste0(isolate(i$i),": ", isolate(reactive_species$species)))
            
            # Download information with BotanizeR_collect()
            sp_quiz <- BotanizeR_collect(
                species_row = temp_row, 
                image_floraweb = hints_reactive$image_floraweb,
                hints_floraweb = hints_reactive$hints_floraweb[which(
                    hints_reactive$hints_floraweb!="map")],
                image_ukplantatlas = hints_reactive$image_ukplantatlas,
                hints_ukplantatlas = hints_reactive$hints_ukplantatlas[which(
                    hints_reactive$hints_ukplantatlas!="mapuk")],
                hints_custom = hints_reactive$hints_custom, 
                imagelinks_custom = hints_reactive$imagelinks_custom,
                image_folders = paste0(system_path,hints_reactive$image_folders),
                only_links = TRUE, image_required = TRUE)
            
            if(length(sp_quiz$images) != 0){
                sp_picture <- 1
                
                # Randomly reordering pictures for the quiz
                sp_quiz$images <- sample(sp_quiz$images)
                
                # Replace Species and genus names in hints
                sp_quiz[names(sp_quiz) %in% c(isolate(hints_reactive$hints_floraweb),
                                              isolate(hints_reactive$hints_ukplantatlas))] <- lapply(sp_quiz[
                                                  names(sp_quiz) %in% c(isolate(hints_reactive$hints_floraweb),
                                                                        isolate(hints_reactive$hints_ukplantatlas))],
                                                  function(x){
                                                      
                                                      x <- gsub(paste0("([\\.\\:\\!\\?])( )(",temp_row$SPECIES,")"), "\\1 This species",x) # ". Fagus sylvatica" <- ". This species"
                                                      x <- gsub(paste0(" ",temp_row$SPECIES), " this species",x) # " Fagus sylvatica" <- " this species"
                                                      x <- gsub(temp_row$SPECIES, "This species",x) # "Fagus sylvatica" <- "This species"
                                                      
                                                      x <- gsub(paste0("([\\.\\:\\!\\?])( )(",
                                                                       gsub(temp_row$GENUS, paste0(substring(temp_row$SPECIES, 1, 1),"."), 
                                                                            temp_row$SPECIES),")"), "\\1 This species",x) # ". F. sylvatica" <- ". This species"
                                                      
                                                      x <- gsub(paste0(" ",
                                                                       gsub(temp_row$GENUS, paste0(substring(temp_row$SPECIES, 1, 1),"."), 
                                                                            temp_row$SPECIES)), " this species",x) # " F. sylvatica" <- " this species"
                                                      
                                                      x <- gsub(gsub(temp_row$GENUS, paste0(substring(temp_row$SPECIES, 1, 1),"."), 
                                                                     temp_row$SPECIES),
                                                                "This species",x) # "F. sylvatica" <- "This species"
                                                      
                                                      x <- gsub(paste0("([\\.\\:\\!\\?])( )(",temp_row$GENUS,")([ \\.\\,\\;\\:\\!\\?])"), "\\1 This genus\\4",x) # ". Fagus " <- ". This genus "
                                                      x <- gsub(paste0("( )(",temp_row$GENUS,")([ \\.\\,\\;\\:\\!\\?])"), " this genus\\3",x) # " Fagus " <- " this genus "
                                                      x <- gsub(paste0("(",temp_row$GENUS,")([ \\.\\,\\;\\:\\!\\?])"), "This genus\\2",x) # "Fagus " <- "This genus "
                                                      
                                                  })
                
                sp_quiz[names(sp_quiz) %in% isolate(hints_reactive$hints_floraweb)] <- lapply(sp_quiz[
                    names(sp_quiz) %in% isolate(hints_reactive$hints_floraweb)], function(x){
                        
                        x <- gsub("This species","Diese Art",x)
                        x <- gsub("this species","diese Art",x)
                        x <- gsub("This genus","Diese Gattung",x)
                        x <- gsub("this genus","diese Gattung",x)
                        
                    })
                
                # Make descriptive hints reactive
                sp_quiz_reactive$sp_quiz <- sp_quiz[names(sp_quiz) != "images"]
            }
        }


        output$answer_status <- renderUI({
            HTML(paste0("Mark your answer and click 'Submit' or hit 'Enter'!",
                        "<br>",
                        "Click 'Answer' or hit 'Arrow down' to retrieve answer.",
                        "<br>", 
                        "Click 'New plant' or hit 'Arrow up' for next species."))
        })
        
        updateTextInput(session, "sp_answer", "Species name", value = "")
        
        # setting back answer text
        output$real_answer_print <- renderText("")

        # counting
        species_list_reactive$df_data$COUNT[isolate(i$i)] <- species_list_reactive$df_data$COUNT[isolate(i$i)] + 1
        print(paste("COUNT =",sum(species_list_reactive$df_data$COUNT)-isolate(counts_reactive$init_count)))
        answered_reactive$cheated <- FALSE
        # print(paste("cheated =", answered_reactive$cheated))
        answered_reactive$answered <- FALSE
        # print(paste("answered =", answered_reactive$answered))



        ### Photos ----
        
        output$random_slickr <- renderSlickR({
            if(length(sp_quiz$images) == 0){
                sp_quiz$images = "no_pictures.png"
            }
            imgs <- slick_list(slick_div(sp_quiz$images, 
                                         css = htmltools::css(margin.left = "auto",
                                                              margin.right = "auto",
                                                              type = "img",links = NULL)))
            slickR(imgs, slideId = "slide_quiz") # + settings(adaptiveHeight = TRUE)
        })

        ### Image credits
        
        # temp objects to avoid immediate updating:
        temp_image_floraweb <- hints_reactive$image_floraweb
        temp_image_ukplantatlas <- hints_reactive$image_ukplantatlas
        temp_image_folders <- hints_reactive$image_folders
        
        output$random_image_credits <- renderUI({
            sources <- c(
                ifelse(temp_image_floraweb, 
                       "<a href='https://www.floraweb.de/' target=_blank>FloraWeb</a>", 
                       NA),
                ifelse(temp_image_ukplantatlas, 
                       "<a href='https://www.brc.ac.uk/plantatlas/' 
                       target=_blank>UK & Ireland Plant Atlas</a>", 
                       NA),
                ifelse(length(temp_image_folders) > 0, 
                       "customized image folders", 
                       NA)
            )
            
            sources <- sources[!is.na(sources)]
            
            if (length(sources)>1){
                sources <- sapply(sources, function(x) c(x,", "))
                sources <- sources[-(length(sources))]
                sources[length(sources)-1] <- " and "
            }
            
            sources <- ifelse(length(sources)>0,
                              paste0("Available images sourced from ",
                                     paste(sources, collapse = ""),
                                     ".</br>See 'About' tab for more details."),
                              "") 
            
            HTML(sources)
        })
        
        ### Description ----
        
        observe({
            
            temp_hints_floraweb <- hints_floraweb_lookup$variable[which(hints_floraweb_lookup$show %in% input$quiz_options)]
            temp_hints_floraweb <- temp_hints_floraweb[which(temp_hints_floraweb %in% names(isolate(sp_quiz_reactive$sp_quiz)))]
            
            temp_hints_ukplantatlas <- hints_ukplantatlas_lookup$variable[which(hints_ukplantatlas_lookup$show %in% input$quiz_options)]
            temp_hints_ukplantatlas <- temp_hints_ukplantatlas[which(temp_hints_ukplantatlas %in% names(isolate(sp_quiz_reactive$sp_quiz)))]
            
            temp_hints_custom <- hints_reactive$hints_custom[which(hints_reactive$hints_custom %in% input$quiz_options)]

            # if(m$hints){
            output$quiz_sp_description <- renderUI({
                floraweb_link <- paste0(
                    "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                    isolate(species_list_reactive$df_data)[isolate(i$i), "NAMNR"],
                    "&")

                ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                            gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(isolate(reactive_species$species)))))
                
                HTML(paste0(paste0(unlist(sapply(isolate(sp_quiz_reactive$sp_quiz)[names(isolate(sp_quiz_reactive$sp_quiz)) %in% temp_hints_floraweb],
                                                 function(x) c(x,"</br></br>"))), collapse=""),
                            ifelse(length(temp_hints_floraweb)>0,
                                   paste0("<b>Source:</b></br><a href='",
                                          floraweb_link, # https://www.floraweb.de/,
                                          "' target=_blank>FloraWeb</a></br></br>")
                                   ,""),
                            paste0(unlist(sapply(isolate(sp_quiz_reactive$sp_quiz)[names(isolate(sp_quiz_reactive$sp_quiz)) %in% temp_hints_ukplantatlas],
                                                 function(x) c(x,"</br></br>"))), collapse=""),
                            ifelse(length(temp_hints_ukplantatlas)>0,
                                   paste0("<b>Source:</b></br><a href='",
                                          ukplantatlas_link, # https://www.brc.ac.uk/,
                                          "' target=_blank>UK & Ireland Plant Atlas</a></br></br>")
                                   ,""),
                            paste0(unlist(sapply(isolate(sp_quiz_reactive$sp_quiz)[names(isolate(sp_quiz_reactive$sp_quiz)) %in% temp_hints_custom],
                                                 function(x) c(x,"</br></br>"))), collapse="")
                ))
            }) 
            # } else {
            #     m$hints <- TRUE
            #     output$quiz_sp_description <- renderUI({""})
            #     #""
            # } 
        })
        
        output$random_map_text <- renderUI({""})
        
        ### Map ----
        observeEvent(input$quiz_options_maps, ignoreInit = TRUE, {
            output$random_map <- renderUI({
                
                if("Map" %in% input$quiz_options_maps & m$map){
                    random_map <- BotanizeR_collect(
                        species_row = isolate(species_list_reactive$df_data)[isolate(i$i), ], 
                        image_floraweb = FALSE, hints_floraweb = "map",
                        image_ukplantatlas = FALSE, hints_ukplantatlas = NULL,                    
                        hints_custom = NULL, imagelinks_custom = NULL, 
                        image_folders = NULL, only_links = TRUE)
                    
                    if(length(random_map$map)>0){
                        output$random_map_text <- renderUI({
                            floraweb_link <- paste0(
                                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                                isolate(species_list_reactive$df_data)[isolate(i$i), "NAMNR"],
                                "&")
                            HTML(paste0("Map source: <a href='",
                                        floraweb_link, # https://www.floraweb.de/,
                                        "' target=_blank>FloraWeb</a></br>"))
                        })
                        output$plot_map <- renderPlot({
                            par(oma = c(0, 0, 0, 11))
                            plot(random_map$map[[1]], pal = random_map$map[[2]],
                                 key.pos = 4, main = "")
                        })
                        plotOutput("plot_map")
                    } else {
                        output$random_map_text <- renderUI({""})
                        "No distribution map for Germany available!"
                    }
                } else if ("Map UK" %in% input$quiz_options_maps & m$map){
                    random_map <- BotanizeR_collect(
                        species_row = isolate(species_list_reactive$df_data)[isolate(i$i), ], 
                        image_floraweb = FALSE, hints_floraweb = NULL,
                        image_ukplantatlas = FALSE, hints_ukplantatlas = "mapuk",                    
                        hints_custom = NULL, imagelinks_custom = NULL, 
                        image_folders = NULL, only_links = TRUE)
                    
                    if(length(random_map$mapuk)>0){
                        output$random_map_text <- renderUI({
                            ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                                        gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(isolate(reactive_species$species)))))
                            
                            HTML(paste0("Map from <i>New Atlas</i> by the Botanical Society of Britain and Ireland (blue: native, red: introduced). </br>For more details see: <a href='",
                                        ukplantatlas_link, # https://www.brc.ac.uk/,
                                        "' target=_blank>UK & Ireland Plant Atlas</a></br>"))
                        })
                        
                        par(mar = rep(0.5, 4), oma = rep(0, 4))
                        tags$img(src = random_map$mapuk,
                                 width = "500px")
                        
                    } else {
                        output$random_map_text <- renderUI({""})
                        "No distribution map for the UK and Ireland available!"
                    }
                } else if ("No map" %in% input$quiz_options_maps){
                    m$map <- TRUE
                    output$random_map_text <- renderUI({""})
                    ""
                }
            })
        })
    })

    ### Answer ----
    # display text when no answer is provided
    
    # Providing an answer simple version
    observe({
        output$answer_status <- renderUI({
            HTML(paste0("Mark your answer and click 'Submit' or hit 'Enter'!",
                        "<br>",
                        "Click 'Answer' or hit 'Arrow down' to retrieve answer.",
                        "<br>", 
                        "Click 'New plant' or hit 'Arrow up' for next species."))
        })
        observeEvent(input$submit, {
            isolate({
                answer <- as.character(input$sp_answer)
            })
            if (tolower(answer) == tolower(isolate(reactive_species$species))){
                output$answer_status <- renderUI(
                        HTML(paste0(
                            "<p style='border:3px; border-style:solid;
                            border-color:#38772d; padding: 1em;
                            background-color:#73f75b;
                            box-shadow: 3px 5px #666666;
                            text-align: center;
                            max-width: 300px'>
                            <font size=5 color=\"#38772d\"><b>",
                            "Correct",
                            "</font></b></p>"))
                        )
                
                # Setting answered
                if(!answered_reactive$answered & !answered_reactive$cheated){
                    answered_reactive$answered = TRUE
                    print(paste("answered =", answered_reactive$answered))
                }
                
                # enable checkboxes
                updateCheckboxGroupInput(session,
                                         inputId = "quiz_options",
                                         choices = name_hints(c(hints_floraweb_lookup$show[which(
                                             hints_floraweb_lookup$variable %in% 
                                                 hints_reactive$hints_floraweb & 
                                                 hints_floraweb_lookup$show != "Map")],
                                             hints_ukplantatlas_lookup$show[which(
                                                 hints_ukplantatlas_lookup$variable %in% 
                                                     hints_reactive$hints_ukplantatlas & 
                                                     hints_ukplantatlas_lookup$show != "Map UK")],
                                             hints_reactive$hints_custom)),
                                         selected = c(hints_floraweb_lookup$show[which(
                                             hints_floraweb_lookup$variable %in% 
                                                 hints_reactive$hints_floraweb & 
                                                 hints_floraweb_lookup$show != "Map")],
                                             hints_ukplantatlas_lookup$show[which(
                                                 hints_ukplantatlas_lookup$variable %in% 
                                                     hints_reactive$hints_ukplantatlas & 
                                                     hints_ukplantatlas_lookup$show != "Map UK")],
                                             hints_reactive$hints_custom))
                
            } else { 
                char_diff <-
                    paste0(adist(tolower(answer), tolower(isolate(reactive_species$species))),
                           ifelse(adist(tolower(answer), tolower(isolate(reactive_species$species))) > 1,
                                  " characters"," character"),
                           " different")
                
                genus <- species_list_reactive$df_data[isolate(i$i), "GENUS"]
                
                if(nchar(answer)>0){
                    genus_correct <- paste0(
                        ifelse(strsplit(tolower(answer), " ")[[1]][1] == tolower(genus),
                               "Genus correct<br>", "<br>"))
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
    
    ### Real answer ----
    observeEvent(input$real_answer, {
        # updateTextInput(session, "sp_answer", "Species name", value = isolate(reactive_species$species))
        # output$real_answer_print <- renderText(isolate(reactive_species$species))
        output$real_answer_print <- renderText(isolate(species_list_reactive$df_data$TAXONNAME[
            which(isolate(species_list_reactive$df_data$SPECIES) == isolate(reactive_species$species))]))
        
        if(!answered_reactive$answered & !answered_reactive$cheated){
            answered_reactive$cheated <- TRUE 
            print(paste("cheated =", answered_reactive$cheated))
            
            # enable checkboxes
            updateCheckboxGroupInput(session,
                                     inputId = "quiz_options",
                                     choices = name_hints(c(hints_floraweb_lookup$show[which(
                                         hints_floraweb_lookup$variable %in% 
                                             hints_reactive$hints_floraweb & 
                                             hints_floraweb_lookup$show != "Map")],
                                         hints_ukplantatlas_lookup$show[which(
                                             hints_ukplantatlas_lookup$variable %in% 
                                                 hints_reactive$hints_ukplantatlas & 
                                                 hints_ukplantatlas_lookup$show != "Map UK")],
                                         hints_reactive$hints_custom)),
                                     selected = c(hints_floraweb_lookup$show[which(
                                         hints_floraweb_lookup$variable %in% 
                                             hints_reactive$hints_floraweb & 
                                             hints_floraweb_lookup$show != "Map")],
                                         hints_ukplantatlas_lookup$show[which(
                                             hints_ukplantatlas_lookup$variable %in% 
                                                 hints_reactive$hints_ukplantatlas & 
                                                 hints_ukplantatlas_lookup$show != "Map UK")],
                                         hints_reactive$hints_custom))
        }
    })
    
    ### Summary statistics ----
    observeEvent(input$sumstats_button, {
        # Total counts, unique species and score
        no_species <- sum(species_list_reactive$df_data$INCLUDE > 0)
        no_species_right <- sum(species_list_reactive$df_data$INCLUDE > 0 &
                                species_list_reactive$df_data$SCORE > 0)
        
        if(!counts_reactive$omit & !answered_reactive$cheated & answered_reactive$answered){
            if(species_list_reactive$df_data$SCORE[
                which(species_list_reactive$df_data$SPECIES == isolate(reactive_species$species))] == 0){
                no_species_right <- no_species_right + 1
            }
        }
        
        total_count <- sum(species_list_reactive$df_data$COUNT)
        total_species <- sum(species_list_reactive$df_data$COUNT > 0)
        
        total_score <- sum(species_list_reactive$df_data$SCORE)
        
        if(!counts_reactive$omit & !answered_reactive$cheated & answered_reactive$answered){
            total_score <- total_score + 1
        }
        
        
        
        # Session counts, unique species and score
        session_count <- total_count - counts_reactive$init_count
        # session_species <- total_species - counts_reactive$init_count_species # identity plays a role here
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
                        "</b> species ", 
                        #ifelse(session_species>1,
                        #       paste0("(",session_species,
                        #              " different ones) "), ""),
                        "and got <b>", session_score, "</b> right.", "</br><br>",
                        "In total, you practised <b>", total_species,
                        "</b> unique species out of <b>",no_species, 
                        "</b> ones and got <b>", no_species_right, 
                        "</b> of them right at least once.</br>"))
        })
        
        
        twitter_text <- paste0("Hey, I just practised ",session_count,
                               " species with %23BotanizeR and got ",session_score,
                               " right! Try it out here: ")
        twitter_text <- gsub(" ","%20",twitter_text)
        url <- paste0("https://twitter.com/intent/tweet?text=",
                      twitter_text,
                      "&url=",
                      BotanizeR_URL)
        
        showModal(
            # shinyjqui::draggableModalDialog(
            modalDialog(
                title = "Session summary statistics",
                size = "l",
                uiOutput("stats_text"),
                plotOutput("stats_barplot"),
                footer = tagList(
                    actionButton("twitter_share",
                                 label = "Share",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url)),
                modalButton('Close'))
            )
        )
    })
})
