runApp(shinyApp(
  ui=(fluidPage(
    titlePanel("amend data frame"),
    
    mainPanel(
      fileInput("file", "Upload file"),
      
      numericInput("Delete", "Delete row:", 1, step = 1),
      actionButton("Go", "Delete!"),
      
      tableOutput("df_data_out")
    )
  )),
  server = (function(input, output) {
    values <- reactiveValues(df_data = NULL)
    
    observeEvent(input$file, {
      values$df_data <- read.csv(input$file$datapath)
    })
    
    observeEvent(input$Go, {
      temp <- values$df_data[-input$Delete, ]
      values$df_data <- temp
      
    })
    
    output$df_data_out <- renderTable(values$df_data)
  })))