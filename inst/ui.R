## Head ------------------------------------------------------------------------

# Packages
library(BotanizeR)

# Load starting configuration
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
  HTML('<div style="font-size:200%;
                     color:white;
                     font-weight:bold;
                     padding-top:14px">
        BotanizeR</div>'),
  # Team logo 
  tags$script(
    HTML(
      paste0("var header = $('.navbar > .container-fluid');",
             "header.append('<div style=\"float:right\">",
             "<a href=\"https://www.uni-goettingen.de/en/128741.html\"",
             "target=\"_blank\">",
             "<img src=\"biodiv_gottingen_logo.png\" alt=\"alt\" ",
             "style=\"float:right; height:80px;padding-top:10px;\"> </a>",
             "<a href=\"https://twitter.com/intent/tweet?text=",
             "Do%20you%20want%20to%20practise%20your%20plant%20identification",
             "%20skills?%20Try%20out%20the%20%23BotanizeR%20Shiny%20app%20at:",
             "%20&url=", BotanizeR_URL, 
             "\" target=\"_blank\"><img src=\"twitter_bird_logo.png\" ",
             "alt=\"alt\" style=\"float:right; height:35px;padding-top:10px;",
             "padding-right:20px;\"> </a></div>');console.log(header)"))),
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
             }"))),
  tags$head(HTML(
    ifelse(analytics==FALSE, "", paste(
      "<!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src='https://www.googletagmanager.com/gtag/js?id=", 
      analytics, 
      "'></script>
        <script>
        window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', '", 
      analytics,
      "');
      </script>"
    ))))
),
theme = shinytheme("flatly"),
windowTitle = "BotanizeR",
selected = h1(id = "panel2", "Quiz"),

## Species list ----------------------------------------------------------------
tabPanel(h1(id = "panel1", "Species"),
         fluidRow(column(3,
                         em(uiOutput("select_plant")),
                         actionButton("previous_plant", 
                                      label = "Previous species"),
                         actionButton("next_plant", 
                                      label = "Next species"),
                         # tags$head(tags$script(HTML(js_pr_next))),
                         br(),
                         br(),
                         uiOutput(outputId = "options_maps"),
                         br()
         ),
         column(6, style="min-width: 250px",
                slickROutput("selected_sp_photo", 
                             width = "93%", height = "600px"),
                # tags$style('div#selected_sp_photo:hover {
                #  transform: scale(1.5);
                #  transform-origin: top left;
                # }'
                #)
                div(uiOutput("selected_image_credits"), 
                    style="text-align: center;"),
                br(),
                div(uiOutput("selected_sp_map"), 
                    style="text-align: center;"),
                div(uiOutput("selected_map_text"), 
                    style="text-align: center;"),
                br()
         ),
         column(3,
                htmlOutput("selected_sp_name"),
                br(),
                htmlOutput("selected_sp_description"),
                br(),
                uiOutput("selected_sp_chorology")
         ))
),

## Quiz ------------------------------------------------------------------------
tabPanel(
  h1(id = "panel2", "Quiz"),
  fluidRow(#useShinyjs(),  # Set up shinyjs
    column(3,
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
           div(p(htmlOutput("answer_status"), 
                 style = "font-weight=500; color: #000000;"),
               style = "min-height:75px"),
           div(h5(em(textOutput("real_answer_print"), 
                     style = "color: green; font-style: bold")),
               style = "height:40px; text-indent:20px; 
                        display:table-cell; vertical-align:middle"),
           tags$head(tags$script(src = "BotanizeR_buttons.js")),
           actionButton("submit", "Submit"),
           actionButton("real_answer", "Answer"),
           actionButton("newplant", "New plant"),
           div(style = "height:30px")
    ),
    
    # Second part of the page with the picture
    column(6, style="min-width: 250px", 
           slickROutput("random_slickr", width = "93%", height = "600px"),
           div(uiOutput("random_image_credits"), style="text-align: center;"),
           br(),
           div(uiOutput("random_map"), style="text-align: center;"),
           div(uiOutput("random_map_text"), style="text-align: center;"),
           br()
    ),
    
    # Third part with other indices
    column(3,
           br(),
           actionButton("sumstats_button", "Statistics"),
           actionButton("upanddown_button", "Upload/Download progress"),
           br(),br(),br(),
           htmlOutput("quiz_sp_description"),
           br(),
           bsModal(id = "upanddown_modal",
                   title = "Up and Download",
                   trigger = "upanddown_button",
                   size = "large",
                   htmlOutput("upload_note_2"),
                   fileInput("file_2", ""),
                   htmlOutput("upload_error_2"),
                   htmlOutput("download_note_2"),
                   br(),
                   downloadButton("download_2","Download your progress")),
           br()
    )
  )
  
),

## Setup -----------------------------------------------------------------------
if(setup){tabPanel(
  h1(id = "panel3", "Setup"),
  fluidRow(
    column(4, style="padding-right: 8%;",
           h4("Species list"),
           br(),
           HTML("<b>Select a species list</b>"),
           br(),br(),
           htmlOutput("selectlist_note"),
           br(),
           uiOutput("select_specieslist"),
           htmlOutput("summary_note"),
           br(),
           HTML("<b>Upload a species list</b>"),
           br(),
           htmlOutput("upload_note"),
           fileInput("file", "", accept = ".csv"),
           htmlOutput("upload_error"),
           if(gbif)HTML("<br><b>Subset by local species</b><br><br>"),
           if(gbif)htmlOutput("locallist_note"),
           if(gbif)br(),
           if(gbif){
             fluidRow(
               column(width = 3, style="min-width: 125px",
                      numericInput("longitude", "Longitude:", 9.93558, 
                                   step = 0.00001, min = -180, max = 180)
               ),
               column(width = 3, style="min-width: 125px",
                      numericInput("latitude", "Latitude:", 51.53290, 
                                   step = 0.00001, min = -89, max = 89)
               ),
               column(width = 3, style="min-width: 125px",
                      numericInput("radius", "Radius:", 1, 
                                   step = 0.01, min = 0.01, max = 100)
               ),
               column(width = 3, style="min-width: 125px; margin-top: 25px;",
                      actionButton("local_list", "Subset list")
               )
             )},
           if(gbif) htmlOutput("local_list_error"),
           br(),
           HTML("<b>Download the species list</b>"),
           br(),
           htmlOutput("download_note"),
           br(),
           downloadButton("download","Download your progress"),
           br()
    ),
    column(4, style="padding-right: 8%;",
           h4("Custom material"),
           br(),
           HTML("Choose here which of the species characteristics and image 
                    links available in the species list (e.g. columns 
                    ownhint_Growth_form and imagelink_2) and images from which 
                    image folders on the shiny server to show in the species 
                    overview and quiz."),
           br(),
           br(),
           uiOutput(outputId = "own_hints"),
           br(),
           uiOutput(outputId = "own_links"),
           br(),
           HTML("<b>Image folders</b>"), br(),
           shinyDirButton('image_folder', 'Select a folder', 
                          'Please select a folder', FALSE),
           actionButton("remove_folder", "Remove last"),
           htmlOutput("list_imagefolders"),
           br(),
           h4("Quiz Controls"),
           br(),
           HTML("Shall all species in the quiz be shown with equal 
                     probabilities (uniform) or based on previous successes and 
                    failures (dynamic)?"),
           br(),
           br(),
           radioButtons("quiz_probs", "Species sample probabilities",
                        choices = c("dynamic","uniform"),
                        selected = ifelse(dynamic_probabilities, "dynamic", 
                                          "uniform")),
           br()
    ),
    if(online_ressources) {
      column(4, style="padding-right: 8%;",
             h4("Online resources"),
             br(),
             HTML(
               paste0("BotanizeR offers links to images, distribution maps 
                         and chorological and functional characteristics from ",
                      "<a href='https://www.floraweb.de/' 
                        target=_blank>FloraWeb</a>",
                      " and the ",
                      "<a href='https://www.brc.ac.uk/plantatlas/' 
                        target=_blank>UK & Ireland Plant Atlas</a>",
                      ". Choose here which of the resources to show in the 
                        species overview and quiz.")),
             br(),
             br(),
             uiOutput(outputId = "floraweb_images"),
             uiOutput(outputId = "floraweb_hints"),
             uiOutput(outputId = "chorology_hint"),
             actionLink("selectall_fw","Select all"), 
             HTML(" / "), 
             actionLink("unselectall_fw","Unselect all"),
             br(),
             br(),
             uiOutput(outputId = "ukplantatlas_images"),
             uiOutput(outputId = "ukplantatlas_hints"),
             actionLink("selectall_uk","Select all"), 
             HTML(" / "), 
             actionLink("unselectall_uk","Unselect all"),
             br(),
             br()
      )}
  )
)} else {
  tabPanelAbout()  
},

## About -----------------------------------------------------------------------
if(setup) tabPanelAbout()
)
