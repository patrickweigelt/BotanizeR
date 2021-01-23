
# Packages
library(shiny)
library(shinythemes)
# library(shinyjs)
library(BotanizeR)
# library(imager)
#library(XML)
library(slickR)

# Data
# data(floraweb_species)
# species_list <- floraweb_species[which(floraweb_species$SUMMER==1 |
#                                                floraweb_species$BioDiv2005==1), ]
# species_list <- read.csv("floraweb_species_winter.csv") # for winter list
# 
# species_list <- species_list[order(species_list$SPECIES),c(1:14)]
# 
# plant_list <- species_list$SPECIES

# Source text for the "About" panel
tabPanelAbout = source("About.R")$value

# UI
navbarPage(title = div(
    HTML('<span style="font-size:180%;color:white;font-weight:bold;"> BotanizeR</span></a>'),
    # Team logo 
    tags$script(HTML("var header = $('.navbar > .container-fluid');
             header.append('<div style=\"float:right\"><a href=\"https://www.uni-goettingen.de/en/128741.html\"><img src=\"biodiv_gottingen_logo.png\" alt=\"alt\" style=\"float:right; width:140px;height:80px;padding-top:10px;\"> </a></div>');console.log(header)")),
    tags$style(style = 'position:absolute; right:42px;'),
    tags$style(HTML("#panel1{font-size: 25px}")),
    tags$style(HTML("#panel2{font-size: 25px}")),
    tags$style(HTML("#panel_about{font-size: 25px}"))
),
theme = shinytheme("flatly"),
windowTitle = "BotanizeR",

## Species list ---------------------------------------------------------------
tabPanel(h1(id = "panel1", "Species"),
         fluidRow(column(4,
                         # radioButtons("radio", label = h3("Radio buttons"),
                         #              choices = list("Plant list" = "plant_list",
                         #                             "Random plant" = "random_plant"), 
                         #              selected = 1),
                         # uiOutput("list_or_random"),
                         # br(),
                         uiOutput("select_plant"),
                         # selectInput("plant_list", "Plant list",
                         #            choices = plant_list,
                         #            selected = plant_list[1]),
                         # br(),
                         # actionButton("random_plant", "Random plant"),
                         br(),
                         checkboxGroupInput(inputId = "options",                # comment out for winter
                                           label = "Show:",                     # comment out for winter
                                           choices = list("Map", "Chorology"))  # comment out for winter
         ),
         column(4,
                # splitLayout(cellWidths = c("100%"),
                #             uiOutput("selected_sp_photo")),
                br(),
                slickROutput("selected_sp_photo", width = "400px", height = "500px")
         ),
         column(4,
                htmlOutput("selected_sp_german"),
                br(),
                textOutput("selected_sp_family"),
                br(),
                textOutput("selected_sp_status"),
                br(),
                htmlOutput("selected_sp_description"),
                br(),
                textOutput("selected_sp_habitat"),
                br(),
                plotOutput("selected_sp_map"),
                br(),
                uiOutput("selected_sp_chorology")
         ))
),

## Quizz ----------------------------------------------------------------------
tabPanel(
    h1(id = "panel2", "Quiz"),
    fluidRow(
        column(4,
               h5(textOutput("Score")),
               br(),
               checkboxGroupInput(inputId = "quizz_options", label = "Show:",
                                  choices = list("German name","Family","Status"
                                                 ,"Description","Habitat","Map",# comment out for winter
                                                 "Chorology"                    # comment out for winter
                                                 )),
               br(),
               tags$script(' $(document).on("keydown", function (e) {
                                                  Shiny.onInputChange("lastkeypresscode", e.keyCode);
                                                  });
                                                  '),
               textInput("sp_answer", label = "Species name"),
               h5(textOutput("nb_tries"), style = "font-weight=500; color: #000000;"),
               br(),
               p(htmlOutput("answer_status"), style = "font-weight=500; color: #000000;"),
               br(),
               h5(textOutput("real_answer_print"), style = "color: green; font-style: bold"),
               br(),
               tags$head(tags$script(src = "BotanizeR_buttons.js")),
               actionButton("submit", "Submit"),
               actionButton("real_answer", "Answer"),
               # br(),
               actionButton("newplant", "New plant"),
               div(style = "height:20px")
        ),
        
        # Second part of the page with the picture
        column(4,
               # uiOutput("random_sp"),
               slickROutput("random_slickr", width = "400px", height = "500px"),
               br(),
               plotOutput("random_map"),
               br(),
               h5(textOutput("score"))),
        
        # Third part with other indices
        column(4,
               textOutput("random_german"),
               br(),
               textOutput("random_family"),
               br(),
               textOutput("random_status"),
               br(),
               htmlOutput("random_description"),
               br(),
               textOutput("random_habitat"),
               br(),
               uiOutput("random_chorology"),
               br(),
               fileInput("file", "Upload file"),
               br(),
               downloadButton("download","Download your progress")#,
               # br(),
               # dataTableOutput("progress")
               #, tableOutput("df_data_out")
               )
    )
    
),

## About ----------------------------------------------------------------------
tabPanelAbout()
)

