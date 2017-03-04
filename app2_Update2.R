library(shiny)

ui <- fluidPage(
  
  titlePanel("Correlation Demo Assignment"),
  
  splitLayout( #change the layout into split layout but the width on the left-hand side has shrinked...wonder how to fix it
    sidebarPanel(
      sliderInput(inputId = "points",
                  label = "Number of points:",
                  min = 10,
                  max = 200,
                  value = 50),
      sliderInput(inputId = "slope",
                  label = "Slope:",
                  min = -10,
                  max = 10,
                  value = 1),
      sliderInput(inputId = "yIntercept",
                  label = "Intercept:",
                  min = -5,
                  max = 5,
                  value = 0),
      sliderInput(inputId = "error",
                  label = "Error:",
                  min = .001,
                  max = 5,
                  value = 0.5)
    ),
    
    splitLayout( #change the layout into split layout. I have tried to add more codes but changing the layout is the only one that works. Sorry.
      tabsetPanel(type = "tabs", 
                  tabPanel("Scatter", plotOutput("scatterPlot")),
                  tabPanel("X hist", plotOutput("xHist")),
                  tabPanel("Y hist", plotOutput("yHist")),
                  tabPanel("Correlation", verbatimTextOutput("corrTest")), 
                  tabPanel("Regression", verbatimTextOutput("regressionResults")),
                  tabPanel("DescriptiveX", verbatimTextOutput("descriptivedataX")),
                  tabPanel("DescriptiveY", verbatimTextOutput("descriptivedataY"))
#                  tabPanel("DescriptiveTable", tableOutput("desTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  dataX <- reactive({
    x <- runif(input$points)
  })
  
  dataY <- reactive({
    y <- rep(input$yIntercept, input$points) +
      rep(input$slope) * as.vector(dataX()) +
      rnorm(input$points, sd = input$error)
  })
  
  output$scatterPlot <- renderPlot({
    scatter.smooth(dataX(), dataY(), xlab = "x", ylab = "y")
  })
  
  output$xHist <- renderPlot({
    hist(dataX())
  })
  
  output$yHist <- renderPlot({
    hist(dataY())
  })
  
  output$corrTest <- renderPrint({
    cor.test(dataX(), dataY())
  })
  
  output$regressionResults <- renderPrint({
    summary(lm(formula = dataY() ~ dataX()))   
  })
  output$descriptivedataX <- renderPrint({
    describe(dataX())
  })
  output$descriptivedataY <- renderPrint({
    describe(dataY())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

