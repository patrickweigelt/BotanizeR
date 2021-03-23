
# Packages
library(shiny)
library(shinythemes)
# library(shinyjs)
library(BotanizeR)
library(slickR)
library(shinyFiles)
library(shinyBS)

# Load starting config
source("config.R")

# Source text for the "About" panel
tabPanelAbout = source("About.R")$value

# ArrowLeft and Right
js_pr_next <- '$(document).keyup(function(event) {
    if (event.key == "ArrowLeft") {
        $("#previous_plant").click();
    }
    if (event.key == "ArrowRight") {
        $("#next_plant").click();
    }
});'

# UI
navbarPage(title = div(
    HTML('<span style="font-size:180%;color:white;font-weight:bold;"> BotanizeR</span></a>'),
    # Team logo 
    tags$script(HTML("var header = $('.navbar > .container-fluid');
             header.append('<div style=\"float:right\"><a href=\"https://www.uni-goettingen.de/en/128741.html\"><img src=\"biodiv_gottingen_logo.png\" alt=\"alt\" style=\"float:right; width:140px;height:80px;padding-top:10px;\"> </a></div>');console.log(header)")),
    tags$style(style = 'position:absolute; right:42px;'),
    tags$style(HTML("#panel1{font-size: 25px}",
                    "#panel2{font-size: 25px}",
                    "#panel3{font-size: 25px}",
                    "#panel_about{font-size: 25px}")),
    tags$head(
        tags$style(HTML(
            " .slick-slide {
               padding: 0 5px;
               box-sizing: border-box;
            }
            .slick-slide img {
               max-width: 100%;
               height: auto;
               min-width: 200px;
               min-height: 150px;
               max-height: 600px;
             }

             @media(max-width:800px) {
               .slick-slide img {
                 width: 100%;
               }
             }")))
),
theme = shinytheme("flatly"),
windowTitle = "BotanizeR",
selected = h1(id = "panel2", "Quiz"),

## Species list ---------------------------------------------------------------
tabPanel(h1(id = "panel1", "Species"),
         fluidRow(column(3,
                         # radioButtons("radio", label = h3("Radio buttons"),
                         #              choices = list("Plant list" = "plant_list",
                         #                             "Random plant" = "random_plant"), 
                         #              selected = 1),
                         # br(),
                         em(uiOutput("select_plant")),
                         # br(),
                         actionButton("previous_plant", label = "Previous species"),
                         actionButton("next_plant", label = "Next species"),
                         # tags$head(tags$script(HTML(js_pr_next))),
                         br(),
                         br(),
                         uiOutput(outputId = "options_maps")
         ),
         column(6, style="min-width: 250px",
                # splitLayout(cellWidths = c("100%"),
                #             uiOutput("selected_sp_photo")),
                # br(),
                slickROutput("selected_sp_photo", width = "93%", height = "600px"),
                # tags$style('div#selected_sp_photo:hover {
                #  transform: scale(1.5);
                #  transform-origin: top left;
                # }'
                #            )
                div(uiOutput("selected_image_credits"), style="text-align: center;"),
                br(),
                div(uiOutput("selected_sp_map"), style="text-align: center;"),
                div(uiOutput("selected_map_text"), style="text-align: center;")
         ),
         column(3,
                htmlOutput("selected_sp_name"),
                br(),
                htmlOutput("selected_sp_description"),
                br(),
                uiOutput("selected_sp_chorology")
         ))
),

## Quiz -----------------------------------------------------------------------
tabPanel(
    h1(id = "panel2", "Quiz"),
    fluidRow(#useShinyjs(),  # Set up shinyjs
        column(3,
               h5(textOutput("Score")),
               br(),
               uiOutput(outputId = "quiz_options"),
               br(),
               uiOutput(outputId = "quiz_options_maps"),
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
               h5(em(textOutput("real_answer_print"), style = "color: green; font-style: bold")),
               br(),
               tags$head(tags$script(src = "BotanizeR_buttons.js")),
               actionButton("submit", "Submit"),
               actionButton("real_answer", "Answer"),
               actionButton("newplant", "New plant"),
               div(style = "height:20px")
        ),
        
        # Second part of the page with the picture
        column(6, style="min-width: 250px", 
               slickROutput("random_slickr", width = "93%", height = "600px"),
               div(uiOutput("random_image_credits"), style="text-align: center;"),
               br(),
               div(uiOutput("random_map"), style="text-align: center;"),
               div(uiOutput("random_map_text"), style="text-align: center;"),
               br(),
               h5(textOutput("score"))),
        
        # Third part with other indices
        column(3,
               htmlOutput("quiz_sp_description"),
               br(),br(),
               actionButton("sumstats_button", "Statistics"),
               actionButton("upanddown_button", "Upload/Download progress"),
               bsModal(id = "sumstats_modal",
                       title = "Session information",
                       trigger = "sumstats_button",
                       size = "large", uiOutput("stats_text"),
                       plotOutput("stats_barplot")),
               bsModal(id = "upanddown_modal",
                       title = "Up and Download",
                       trigger = "upanddown_button",
                       size = "large",
                       htmlOutput("upload_note_2"),
                       fileInput("file_2", ""),
                       htmlOutput("upload_error_2"),
                       htmlOutput("download_note_2"),
                       br(),
                       downloadButton("download_2","Download your progress")
               )
        )
    )
    
),

## Setup ----------------------------------------------------------------------
if(setup){tabPanel(
    h1(id = "panel3", "Setup"),
    fluidRow(
        column(4,
               h4("Species list"),
               htmlOutput("selectlist_note"),
               uiOutput("select_specieslist"),
               htmlOutput("summary_note"),
               htmlOutput("upload_note"),
               fileInput("file", "", accept = ".csv"),
               htmlOutput("upload_error"),
               htmlOutput("download_note"),
               br(),
               downloadButton("download","Download your progress")
        ),
        column(4,
               h4("Custom material"),
               br(),
               uiOutput(outputId = "own_hints"),
               br(),
               uiOutput(outputId = "own_links"),
               br(),
               h5("Image folders"),
               shinyDirButton('image_folder', 'Select a folder', 'Please select a folder', FALSE),
               actionButton("remove_folder", "Remove last"),
               htmlOutput("list_imagefolders"),
        ),
        if(online_ressources) {column(4,
               h4("Online resources"),
               br(),
               uiOutput(outputId = "floraweb_images"),
               uiOutput(outputId = "floraweb_hints"),
               uiOutput(outputId = "chorology_hint"),
               uiOutput(outputId = "ukplantatlas_images"),
               uiOutput(outputId = "ukplantatlas_hints")
        )}
    )
)} else {
  tabPanelAbout()  
},

## About ----------------------------------------------------------------------
if(setup) tabPanelAbout()
)
