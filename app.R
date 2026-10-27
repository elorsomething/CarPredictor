library(shiny)

ui <- fluidPage(
  titlePanel("Car MPG Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Documentation: Use this application to predict a car's Miles Per Gallon (MPG) based on its weight and horsepower. Simply adjust the sliders below and view the predicted MPG update instantly!"),
      hr(),
      sliderInput("wt", "Weight (1000 lbs):", min = 1.5, max = 5.5, value = 3.0, step = 0.1),
      sliderInput("hp", "Gross Horsepower:", min = 50, max = 350, value = 120, step = 5),
      selectInput("cyl", "Number of Cylinders:", choices = c(4, 6, 8), selected = 4)
    ),
    
    mainPanel(
      h3("Prediction Results"),
      textOutput("predictionText"),
      hr(),
      h4("Summary of Input Parameters:"),
      verbatimTextOutput("inputValue")
    )
  )
)

server <- function(input, output) {
  model <- lm(mpg ~ wt + hp + cyl, data = mtcars)
  
  reactivePrediction <- reactive({
    wtInput <- input$wt
    hpInput <- input$hp
    cylInput <- as.numeric(input$cyl)
    predict(model, newdata = data.frame(wt = wtInput, hp = hpInput, cyl = cylInput))
  })
  
  output$inputValue <- renderPrint({
    cat("Weight:", input$wt, "thousand lbs\n")
    cat("Horsepower:", input$hp, "hp\n")
    cat("Cylinders:", input$cyl)
  })
  
  output$predictionText <- renderText({
    paste("Estimated Fuel Efficiency (MPG):", round(reactivePrediction(), 2), "Miles/Gallon")
  })
}

shinyApp(ui = ui, server = server)
