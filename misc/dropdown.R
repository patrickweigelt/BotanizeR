library(dplyr)
library(ggplot2)
library(shiny)


server <- function(input, output) {
  
  
  raw <- diamonds
  
  
  
  output$cutlist <- renderUI({
    
    cutlist <- sort(unique(as.vector(raw$cut)), decreasing = FALSE)
    cutlist <- append(cutlist, "All", after =  0)
    selectizeInput("cutchoose", "Cut:", cutlist)
    
  })
  
  
  output$colorlist <- renderUI({
    
    colorlist <- sort(unique(as.vector(raw$color)), decreasing = FALSE)
    colorlist <- append(colorlist, "All", 0)
    selectizeInput("colorchoose", "color:", colorlist)
    
  }) 
  
  
  data <- reactive({
    
    req(input$colorchoose)
    req(input$cutchoose)
    
    
    if(input$colorchoose == "All") {
      
      filt1 <- quote(color != "@?><")
      
      
    } else {
      
      filt1 <- quote(color == input$colorchoose) 
      
    }
    
    
    if (input$cutchoose == "All") {
      
      filt2 <- quote(cut != "@?><")
      
      
    } else {
      
      filt2 <- quote(cut == input$cutchoose)
      
    }
    
    
    
    raw %>%
      filter_(filt1) %>%
      filter_(filt2)
    
  })
  
  
  output$table <- renderDataTable({
    
    
    data()
    
  })
  
}

ui <- fluidPage(
    
    # Application title
    titlePanel("Dynamic Filter Test App"),
    
    
    sidebarLayout(
      sidebarPanel(
        uiOutput("cutlist"),
        uiOutput("colorlist")
      ),
      
      
      mainPanel(
        dataTableOutput("table")
      )
    )
  )

shinyApp(ui, server)

