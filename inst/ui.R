
library(shiny)
library(shinythemes)
# library(shinyjs)
library(BotanizeR)
library(imager)
library(XML)

# UI
navbarPage(title = div(
    HTML('<span style="font-size:120%;color:white;font-weight:bold;"> Botanic quizz&nbsp;&nbsp;</span></a>')
),
theme = shinytheme("flatly"),
windowTitle = "Botanic quizz",

## Main page ------------------------------------------------------------------
tabPanel(
    # Application title
    titlePanel("Main page"),
    fluidRow(
        column(4,
               h5(textOutput("Score")),
               br(),
               checkboxGroupInput(inputId = "options", label = "Show:",
                                  choices = list("Description", "Status",
                                                 "Family", "Habitat",
                                                 "German name", "Map")),
               br(),
               textInput("sp_answer", label = "Species name"),
               h5(textOutput("nb_tries"), style = "font-weight=500; color: #000000;"),
               br(),
               p(textOutput("status1"), style = "font-weight=500; color: #000000;"),
               h5(textOutput("status2"), style = "font-weight=500; color: #00CC00;"),
               h5(textOutput("status3"), style = "font-weight=500; color: #FF0000;"),
               br(),
               actionButton("submit", "Submit"),
               actionButton("newplant", "New plant")),
        
        # Second part of the page with the picture
        column(8, plotOutput("random_sp"))),
    
)

## About ----------------------------------------------------------------------
# tabPanelAbout()
)

