# Activate the course renv project (important in your setup)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::activate("/data/junior/boland_course")
}

library(shiny)

ui <- fluidPage(
  titlePanel("Hello, Shiny!"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "num",
        label   = "Choose a number:",
        min     = 1,
        max     = 100,
        value   = 50
      )
    ),
    mainPanel(
      h3("You selected:"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {
  output$result <- renderText({
    input$num
  })
}

shinyApp(ui = ui, server = server)