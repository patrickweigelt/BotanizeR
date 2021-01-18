
# Packages
library(shiny)
library(shinythemes)
# library(shinyjs)
library(BotanizeR)
# library(imager)
#library(XML)
library(slickR)

# Data
data(floraweb_species)
floraweb_species <- floraweb_species[which(floraweb_species$SUMMER==1 |
                                               floraweb_species$BioDiv2005==1), ]
species_list <- floraweb_species
plant_list <- floraweb_species$SPECIES

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
tabPanel(h1(id = "panel1", "Species list"),
         fluidRow(column(4,
                         selectInput("plant_list", "Plant list",
                                     choices = plant_list,
                                     # selectize = FALSE,
                                     selected = "Acer campestre"),
                         br(),
                         checkboxGroupInput(inputId = "options",
                                            label = "Show:",
                                            choices = list("Map", "Chorology"))
         ),
         column(4,
                # splitLayout(cellWidths = c("100%"),
                #             uiOutput("selected_sp_photo")),
                br(),
                slickROutput("slickr", width = "320")
         ),
         column(4,
                htmlOutput("selected_sp_description"),
                br(),
                textOutput("selected_sp_habitat"),
                br(),
                textOutput("selected_sp_family"),
                br(),
                textOutput("selected_sp_status"),
                br(),
                textOutput("selected_sp_german"),
                br(),
                plotOutput("selected_sp_map"),
                br(),
                uiOutput("selected_sp_chorology")
         ))
),

## Quizz ----------------------------------------------------------------------
tabPanel(
    h1(id = "panel2", "Quizz"),
    fluidRow(
        column(4,
               h5(textOutput("Score")),
               br(),
               checkboxGroupInput(inputId = "quizz_options", label = "Show:",
                                  choices = list("Description", "Status",
                                                 "Family", "Habitat",
                                                 "German name", "Map",
                                                 "Chorology")),
               br(),
               tags$script(' $(document).on("keydown", function (e) {
                                                  Shiny.onInputChange("lastkeypresscode", e.keyCode);
                                                  });
                                                  '),
               textInput("sp_answer", label = "Species name"),
               h5(textOutput("nb_tries"), style = "font-weight=500; color: #000000;"),
               br(),
               p(uiOutput("answer_status"), style = "font-weight=500; color: #000000;"),
               br(),
               h5(textOutput("real_answer"), style = "color: green; font-style: bold"),
               br(),
               actionButton("submit", "Submit"),
               actionButton("real_answer", "Answer"),
               br(),
               tags$head(tags$script(src = "enter_button.js")),
               actionButton("newplant", "New plant")
        ),
        
        # Second part of the page with the picture
        column(4,
               # uiOutput("random_sp"),
               slickROutput("random_slickr", width = "320"),
               br(),
               plotOutput("random_map"),
               br(),
               h5(textOutput("score"))),
        
        # Third part with other indices
        column(4,
               htmlOutput("random_description"),
               br(),
               textOutput("random_habitat"),
               br(),
               textOutput("random_family"),
               br(),
               textOutput("random_status"),
               br(),
               textOutput("random_german"),
               br(),
               uiOutput("random_chorology"),
               br(),
               downloadButton("download","Download your progress")#,
               # br(),
               # dataTableOutput("progress")
               )
    )
    
),

## About ----------------------------------------------------------------------
tabPanelAbout()
)

