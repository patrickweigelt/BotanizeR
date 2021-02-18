
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


shinyServer(function(input, output, session) {
    
    # 0. Preparation ----
    
    # Load starting config
    source("config.R")
    
    # Load list of species that have a chorology map
    chorology_list <- read.table("NAMNR_chorology.txt")
    
    # Load initial species list
    if(species_list_selected == "Germany_all"){
        data(floraweb_species)
        species_list <- floraweb_species
    }  else if (species_list_selected == "Germany_summer"){
        data(floraweb_species)
        species_list <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                                   floraweb_species$BioDiv2005==1), ]
    } else if (species_list_selected == "Germany_winter"){
        data(floraweb_species)
        species_list <- floraweb_species[which(floraweb_species$WINTER==1), ]
    } else if (species_list_selected == "UK&Ireland_all"){
        data(ukplantatlas_species)
        species_list <- ukplantatlas_species
    }
    
    # Order species list alphabetically 
    species_list <- species_list[order(species_list$SPECIES),]
    
    # Make species list a reactive object
    species_list_reactive <- reactiveValues(df_data = species_list)
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
                                         which(hints_custom %in% colnames(species_list)
                                               & !hints_custom %in% c(hints_custom_omit,
                                                                      grep("imagelink", colnames(species_list), 
                                                                           value = TRUE)))],
                                     chorology = chorology)

    # French common names
    # fr_common <- read.table("nom_vernaculaires_cleaned.csv",
    #                        header = TRUE, sep = "\t", fill = TRUE)
    
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

    ### Own hints ----
    output$own_hints <- renderUI({
        checkboxGroupInput(inputId = "own_hints", label = "Own hints",
                           choices = colnames(species_list)[
                               which(!colnames(species_list) 
                                     %in% c(hints_custom_omit,
                                            grep("imagelink", 
                                                 colnames(species_list), 
                                                 value = TRUE)))],
                           selected = hints_custom[
                               which(hints_custom %in% colnames(species_list)
                                     & !hints_custom %in% c(hints_custom_omit,
                                                            grep("imagelink", colnames(species_list), 
                                                                 value = TRUE)))])
    })
    
    observeEvent(input$own_hints, ignoreNULL = FALSE, ignoreInit = TRUE, {
        hints_reactive$hints_custom <- input$own_hints
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
                    choices = c("Germany_all","Germany_winter","Germany_summer","UK&Ireland_all"),
                    selected = species_list_selected)
    })

    # Make condition that input is needed to initialize drop_down
    y <- reactive({
        req(input$select_specieslist)
        input$select_specieslist
    })
    
    # Observe input
    observeEvent(y(), ignoreInit = TRUE, {
        if(input$select_specieslist == "Germany_all"){
            data(floraweb_species)
            temp_species_list <- floraweb_species
        }  else if (input$select_specieslist == "Germany_summer"){
            data(floraweb_species)
            temp_species_list <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                                            floraweb_species$BioDiv2005==1), ]
        } else if (input$select_specieslist == "Germany_winter"){
            data(floraweb_species)
            temp_species_list <- floraweb_species[which(floraweb_species$WINTER==1), ]
        } else if (input$select_specieslist == "UK&Ireland_all"){
            data(ukplantatlas_species)
            temp_species_list <- ukplantatlas_species
        } else if (input$select_specieslist == "uploaded"){
            temp_species_list <- species_list_uploaded_reactive$df_data
        }
        
        species_list_reactive$df_data <- temp_species_list[order(temp_species_list$SPECIES),]
        counts_reactive$init_count <- sum(temp_species_list$COUNT)
        counts_reactive$init_score <- sum(temp_species_list$SCORE)
        counts_reactive$init_count_species <- sum(temp_species_list$COUNT > 0)
        counts_reactive$init_score_species <- sum(temp_species_list$SCORE > 0)
        
        # Avoid that scores are updated when hitting next plant or download or summary
        counts_reactive$omit <- TRUE
        
        # Update ownhint checkboxes
        updateCheckboxGroupInput(session,
                                 inputId = "own_hints", label = "Own hints",
                                 choices = colnames(temp_species_list)[
                                     which(!colnames(temp_species_list) 
                                        %in% c(hints_custom_omit,
                                               grep("imagelink", 
                                                    colnames(temp_species_list), 
                                                    value = TRUE)))],
                                 selected = hints_reactive$hints_custom)
    })
    
    # Species list summary note
    output$summary_note <- renderUI({
        HTML(paste0("<i>",
                    nrow(species_list_reactive$df_data),
                    " species; ", 
                    sum(species_list_reactive$df_data$COUNT),
                    " practiced.</i>"))
    })

    ### Upload a species list ----
    output$upload_note <- renderUI({
        HTML(paste0("<br>",
                    "If you ran the quiz in a previous session and you saved your progress, 
                    you can upload your current scores as a .csv file here. You can also upload 
                    a modified species list with another set of species or your own hints."))
    })

    observeEvent(input$file, {
        species_list_uploaded <- read.csv(input$file$datapath)
        species_list_uploaded <- species_list_uploaded[order(species_list_uploaded$SPECIES),]
        # write control and note for right columns in dataframe
        species_list_reactive$df_data <- species_list_uploaded
        species_list_uploaded_reactive$df_data <- species_list_uploaded
        counts_reactive$init_count <- sum(species_list_uploaded$COUNT)
        counts_reactive$init_score <- sum(species_list_uploaded$SCORE)
        counts_reactive$init_count_species <- sum(species_list_uploaded$COUNT > 0)
        counts_reactive$init_score_species <- sum(species_list_uploaded$SCORE > 0)
        
        # update specieslist drop down
        updateSelectInput(session,
                          inputId = "select_specieslist", label = NULL,
                          choices = c("Germany_all","Germany_winter","Germany_summer","UK&Ireland_all","uploaded"),
                          selected = "uploaded")
    })

    ### Download a species list ----
    output$download <- downloadHandler(
        filename = function(){"BotanizeR_practised.csv"}, 
        content = function(file){
            species_list_save <- species_list_reactive$df_data
            if(!counts_reactive$omit & !answered_reactive$cheated){
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

    
    # 2. Selected species ----
    
    ### Render Options ----
    
    # Dynamic dropdown
    
    # choice_plants <- reactive({
    #     species_list_reactive$df_data$SPECIES
    # })
    
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

        # Plant species chosen
        j <- which(isolate(species_list_reactive$df_data)$SPECIES == selected_species)
        
        # Download information with BotanizeR_collect()
        sp_infos <- BotanizeR_collect(
            species_row = isolate(species_list_reactive$df_data)[j, ], 
            image_floraweb = hints_reactive$image_floraweb,
            hints_floraweb = hints_reactive$hints_floraweb[which(
                hints_reactive$hints_floraweb!="map")],
            image_ukplantatlas = hints_reactive$image_ukplantatlas,
            hints_ukplantatlas = hints_reactive$hints_ukplantatlas,
            hints_custom = hints_reactive$hints_custom, 
            imagelink_custom = NULL,
            image_folders = paste0(system_path,hints_reactive$image_folders,sep=""),
            file_location = "temporary", only_links = TRUE)
        
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
                ifelse(hints_reactive$image_floraweb, 
                       "<a href='https://www.floraweb.de/' target=_blank>FloraWeb</a>", 
                       NA),
                ifelse(hints_reactive$image_ukplantatlas, 
                       "<a href='https://www.brc.ac.uk/plantatlas/' 
                       target=_blank>UK & Ireland Plant Atlas</a>", 
                       NA),
                ifelse(length(hints_reactive$image_folders) > 0, 
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
                    print(paste("List",input$options_maps))
                    map <- BotanizeR_collect(
                        species_row = isolate(species_list_reactive$df_data[j, ]), 
                        image_floraweb = FALSE,
                        hints_floraweb = "map",
                        image_ukplantatlas = FALSE,
                        hints_ukplantatlas = NULL,                    
                        hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                        file_location = "temporary", only_links = TRUE)
                    
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
                    print(paste("List",input$options_maps))
                    map <- BotanizeR_collect(
                        species_row = isolate(species_list_reactive$df_data[j, ]), 
                        image_floraweb = FALSE,
                        hints_floraweb = NULL,
                        image_ukplantatlas = FALSE,
                        hints_ukplantatlas = "mapuk",                    
                        hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                        file_location = "temporary", only_links = TRUE)
                    
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
                    m$map <- TRUE
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
    
    output$quiz_options <- renderUI({
        checkboxGroupInput(inputId = "quiz_options", label = "Show:",
                           choices = c(hints_floraweb_lookup$show[which(
                               hints_floraweb_lookup$variable %in% 
                                   hints_reactive$hints_floraweb & 
                                   hints_floraweb_lookup$show != "Map")],
                               hints_ukplantatlas_lookup$show[which(
                                   hints_ukplantatlas_lookup$variable %in% 
                                       hints_reactive$hints_ukplantatlas & 
                                       hints_ukplantatlas_lookup$show != "Map UK")],
                               hints_reactive$hints_custom))
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
    
    # Workaround to avoid printing map before radiobuttons are set back to "no map"
    m <- reactiveValues(map=TRUE)

    
    # New plant observe
    observeEvent(input$newplant, ignoreNULL = FALSE, {
        
        # set map to false to not plot one before radiobuttons are set
        m$map <- FALSE
        
        sp_picture <- 0
        k <- 0
        
        if(!counts_reactive$omit & !is.na(i$i) & !answered_reactive$cheated){
            species_list_reactive$df_data$SCORE[i$i] <- species_list_reactive$df_data$SCORE[i$i] + answered_reactive$answered
        }
        
        counts_reactive$omit <- FALSE
        
        print(paste("SCORE = ",sum(species_list_reactive$df_data$SCORE)))
        
        while (sp_picture == 0 & k <= 10) { # If no picture available => new plant
            
            # control for bad choice of species list and image hints (no images)
            k <- k + 1
            
            # random species
            temp1 <- species_list_reactive$df_data
            reactive_species$species <- sample(temp1$SPECIES, 1, 
                                               prob = ((temp1$COUNT - temp1$SCORE + 1)/
                                                           (temp1$SCORE+1))*temp1$INCLUDE)
            i$i <- which(temp1$SPECIES == reactive_species$species)
            print(i$i)
            
            # Download information with BotanizeR_collect()
            sp_quiz <- BotanizeR_collect(
                species_row = temp1[i$i, ], 
                image_floraweb = hints_reactive$image_floraweb,
                hints_floraweb = hints_reactive$hints_floraweb[which(
                    hints_reactive$hints_floraweb!="map")],
                image_ukplantatlas = hints_reactive$image_ukplantatlas,
                hints_ukplantatlas = hints_reactive$hints_ukplantatlas,
                hints_custom = hints_reactive$hints_custom, 
                imagelink_custom = NULL,
                image_folders = paste0(system_path,hints_reactive$image_folders),
                file_location = "temporary", only_links = TRUE,
                image_required = TRUE)
            
            if(length(sp_quiz$images) != 0){
                sp_picture <- 1
                
                # Randomly reordering pictures for the quiz
                sp_quiz$images <- sample(sp_quiz$images)
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
                                 inputId = "quiz_options",
                                 choices = c(hints_floraweb_lookup$show[which(
                                     hints_floraweb_lookup$variable %in% 
                                         hints_reactive$hints_floraweb & 
                                         hints_floraweb_lookup$show != "Map")],
                                     hints_ukplantatlas_lookup$show[which(
                                         hints_ukplantatlas_lookup$variable %in% 
                                             hints_reactive$hints_ukplantatlas & 
                                             hints_ukplantatlas_lookup$show != "Map UK")],
                                     hints_reactive$hints_custom),
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
            temp_hints_floraweb <- temp_hints_floraweb[which(temp_hints_floraweb %in% names(sp_quiz))]
            
            temp_hints_ukplantatlas <- hints_ukplantatlas_lookup$variable[which(hints_ukplantatlas_lookup$show %in% input$quiz_options)]
            temp_hints_ukplantatlas <- temp_hints_ukplantatlas[which(temp_hints_ukplantatlas %in% names(sp_quiz))]
            
            temp_hints_custom <- hints_reactive$hints_custom[which(hints_reactive$hints_custom %in% input$quiz_options)]
            
            output$quiz_sp_description <- renderUI({
                floraweb_link <- paste0(
                    "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                    isolate(species_list_reactive$df_data)[i$i, "NAMNR"],
                    "&")

                ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                            gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(reactive_species$species))))
                
                HTML(paste0(paste0(unlist(sapply(sp_quiz[names(sp_quiz) %in% temp_hints_floraweb],
                                                 function(x) c(x,"</br></br>"))), collapse=""),
                            ifelse(length(temp_hints_floraweb)>0,
                                   paste0("<b>Source:</b></br><a href='",
                                          floraweb_link, # https://www.floraweb.de/,
                                          "' target=_blank>FloraWeb</a></br></br>")
                                   ,""),
                            paste0(unlist(sapply(sp_quiz[names(sp_quiz) %in% temp_hints_ukplantatlas],
                                                 function(x) c(x,"</br></br>"))), collapse=""),
                            ifelse(length(temp_hints_ukplantatlas)>0,
                                   paste0("<b>Source:</b></br><a href='",
                                          ukplantatlas_link, # https://www.brc.ac.uk/,
                                          "' target=_blank>UK & Ireland Plant Atlas</a></br></br>")
                                   ,""),
                            paste0(unlist(sapply(sp_quiz[names(sp_quiz) %in% temp_hints_custom],
                                                 function(x) c(x,"</br></br>"))), collapse="")
                ))
            })
        })
        
        output$random_map_text <- renderUI({""})
        
        ### Map ----
        observeEvent(input$quiz_options_maps, ignoreInit = TRUE, {
            output$random_map <- renderUI({
                
                if("Map" %in% input$quiz_options_maps & m$map){
                    random_map <- BotanizeR_collect(
                        species_row = species_list_reactive$df_data[i$i, ], 
                        image_floraweb = FALSE,
                        hints_floraweb = "map",
                        image_ukplantatlas = FALSE,
                        hints_ukplantatlas = NULL,                    
                        hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                        file_location = "temporary", only_links = TRUE)
                    
                    if(length(random_map$map)>0){
                        output$random_map_text <- renderUI({
                            floraweb_link <- paste0(
                                "https://www.floraweb.de/pflanzenarten/artenhome.xsql?suchnr=",
                                isolate(species_list_reactive$df_data)[i$i, "NAMNR"],
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
                        species_row = species_list_reactive$df_data[i$i, ], 
                        image_floraweb = FALSE,
                        hints_floraweb = NULL,
                        image_ukplantatlas = FALSE,
                        hints_ukplantatlas = "mapuk",                    
                        hints_custom = NULL, imagelink_custom = NULL, image_folders = NULL,
                        file_location = "temporary", only_links = TRUE)
                    
                    if(length(random_map$mapuk)>0){
                        output$random_map_text <- renderUI({
                            ukplantatlas_link <- paste0("https://www.brc.ac.uk/plantatlas/plant/",
                                                        gsub("[\\.\\(\\)]","",gsub(" ","-",tolower(reactive_species$species))))
                            
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
                        "<br>", "Click 'New plant' or hit 'Arrow up' for next species.",
                        "</br><br>",
                        "Click 'Answer' or hit 'Arrow down' to get the answer.", "</br>"))
        })
        observeEvent(input$submit, {
            isolate({
                answer <- as.character(input$sp_answer)
            })
            if (tolower(answer) == tolower(reactive_species$species)){
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
                answered_reactive$answered = TRUE
                print(paste("answered = ", answered_reactive$answered))
                
                # enable checkboxes
                updateCheckboxGroupInput(session,
                                         inputId = "quiz_options",
                                         choices = c(hints_floraweb_lookup$show[which(
                                             hints_floraweb_lookup$variable %in% 
                                                 hints_reactive$hints_floraweb & 
                                                 hints_floraweb_lookup$show != "Map")],
                                             hints_ukplantatlas_lookup$show[which(
                                                 hints_ukplantatlas_lookup$variable %in% 
                                                     hints_reactive$hints_ukplantatlas & 
                                                     hints_ukplantatlas_lookup$show != "Map UK")],
                                             hints_reactive$hints_custom),
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
    
    ### Real answer ----
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
            
            # enable checkboxes
            updateCheckboxGroupInput(session,
                                     inputId = "quiz_options",
                                     choices = c(hints_floraweb_lookup$show[which(
                                         hints_floraweb_lookup$variable %in% 
                                             hints_reactive$hints_floraweb & 
                                             hints_floraweb_lookup$show != "Map")],
                                         hints_ukplantatlas_lookup$show[which(
                                             hints_ukplantatlas_lookup$variable %in% 
                                                 hints_reactive$hints_ukplantatlas & 
                                                 hints_ukplantatlas_lookup$show != "Map UK")],
                                         hints_reactive$hints_custom),
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
    
    ### Sum.stats ----
    observe({
        # Total counts, unique species and score
        total_count <- sum(species_list_reactive$df_data$COUNT)
        total_species <- sum(species_list_reactive$df_data$COUNT > 0)
        
        total_score <- sum(species_list_reactive$df_data$SCORE)
        if(!counts_reactive$omit & !answered_reactive$cheated & answered_reactive$answered){
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
    ### Download ----
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
