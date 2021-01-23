library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("nrow", label = h3("number of rows"), 
                  choices = list(5, 10, 20), 
                  selected = 5),
      uiOutput(outputId = "show_vars")
    ),
    mainPanel(
      DT::dataTableOutput("mytable1")
    )
  )
)


server <- function(input, output) {
  
  # choose rows to display
  df <- reactive({
    diamonds[sample(nrow(diamonds), input$nrow), ]
  })
  
  output$show_vars <- renderUI({
    checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                       names(df()), selected = names(df()))
  })
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds[, input$show_vars, drop = FALSE])
  })
  
}

shinyApp(ui, server)
