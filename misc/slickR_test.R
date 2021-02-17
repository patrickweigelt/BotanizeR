library(shiny)
library(slickR)

#.slick-slide {
#  width: 300px
#}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
"
.slick-slide img {
  max-width: 100%;
  height: auto;
  min-width: 200px;
  min-height: 150px;
}

@media(max-width:800px) {
  .slick-slide img {
    width: 100%;
  }
}
"
    ))),
  sidebarLayout(
    sidebarPanel(
      ####
    ),
    
    mainPanel(
      slickROutput("slickr", width="90%")#, height = "700px")
    )
  )
)



server <- function(input, output) {
  
  output$slickr <- renderSlickR({
  # imgs <- list.files("inst/www/pictures_Clemens/", pattern="Acer platanoides", recursive=TRUE, full.names = TRUE)
  imgs <- list.files("inst/www/test/", pattern="\\.jpg", recursive=TRUE, full.names = TRUE)
  imgs <- slick_list(slick_div(imgs, css = htmltools::css(margin.left="auto", margin.right="auto"),type = "img",links = NULL))
  slickR(imgs, slideId = "something") + settings(adaptiveHeight = TRUE)#, variableWidth = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
