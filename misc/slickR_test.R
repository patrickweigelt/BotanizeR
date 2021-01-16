library(shiny)
library(slickR)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ####
    ),
    
    mainPanel(
      slickROutput("slickr", width="500px")
    )
  )
)

server <- function(input, output) {
  
  output$slickr <- renderSlickR({
    # imgs <- list.files("inst/www/pictures_Clemens/", pattern="Acer platanoides", recursive=TRUE, full.names = TRUE)
    imgs <- list.files("inst/www/pictures_Clemens/", pattern="\\.png", recursive=TRUE, full.names = TRUE)
    # imgs <- slick_list(slick_div(imgs, css = htmltools::css(width = "500px"),type = "img",links = NULL))
    slickR(imgs)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
