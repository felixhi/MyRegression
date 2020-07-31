library(shiny)
library(caret)
library(ggplot2)

ui <- fluidPage(
  titlePanel("MyRegression"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("data", "Wähle Daten",
                accept = ".csv"
                ),
     selectInput("x", "Wähle Prädiktor",
               names(data)), 
     selectInput("y", "Wähle Outcome",
               names(data)),
     actionButton("plotte", "Daten plotten"),
#     br(),
#     br(),
#     br(),
#     numericInput("call", "Wähle vorherzusagenden Wert des Prädiktors",
#                value = 0),
#     actionButton("predict", "Vorhersage berechnen")
    ),
    mainPanel(
      h3("Vorschau der Daten"),
      h5("Hier werden die ersten Zeilen deiner Daten dargestellt."),
      tableOutput("Head_Data"),
      br(),
      br(),
      h3("Plot deiner Daten"),
      h5("Zum plotten der Daten wähle einen Prädiktor und ein Outcome und klicke anschließend auf 'Daten plotten'."),
#      br(),
#      br(),
      plotOutput("MyRegression"),
#      br(),
#      br(),
#      h3("Vorhersage deines Wertes"),
#      textOutput("Prediction")
    )
  )
)

server <- function(input, output, session) {

  Vorschau <- reactive({
    if (is.null(input$data))
      return(NULL)                
    df <- input$data
    dat <- read.csv(df$datapath)
    updateSelectizeInput(session, 'x',
                         choices = names(dat),
                         server = TRUE
    )
    
    updateSelectizeInput(session, 'y',
                         choices = names(dat),
                         server = TRUE
    )
    head <- head(dat)
    head
    
  })
  output$Head_Data <- renderTable({
    Vorschau()
  })
  
  Graph <- eventReactive(input$plotte, {
    
    data <- read.csv(input$data$datapath)
    columns_of_interest <- c(input$x, input$y)
    dat <- data[ ,columns_of_interest]
    qplot(dat[,1], dat[,2], data = dat) +
      geom_smooth(method = "lm", se = FALSE, color = I("red"))
  })
  
  output$MyRegression <- renderPlot({
    Graph()
    })

  }


shinyApp(ui, server)





















#  Vorhersage <- eventReactive(input$predict, {
#    data <- read.csv(input$data$datapath)
#    columns_of_interest <- c(input$x, input$y)
#    dat <- data[ ,columns_of_interest]
#    model <- lm(dat[,1] ~ dat[,2], data = dat)
#    print(predict(model, input$call))
#  })
#  
#  output$Prediction <- renderText({
#    Vorhersage()
#    })
