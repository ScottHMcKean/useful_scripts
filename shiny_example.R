# Pre Work
library(ggfortify)
dsnames <- names(iris[1:4])
cb_options <- list()
cb_options[dsnames] <- dsnames

# UI
shinyUI <- fluidPage(
  titlePanel("Interactive K-Means Clustering Using Iris Dataset"),
  sidebarPanel(
    numericInput("numCenter", label=h4("Centers for k-means:"), min=2, max=30, value=3),
    selectInput("xaxisGrp","X-Axis:", c("1"="1","2"="2"), choices=cb_options),
    selectInput("yaxisGrp","Y-Axis:", c("1"="1","2"="2"), choices=cb_options),
    tableOutput("info")
  ),
  mainPanel(
    plotOutput("plot1", click="plot_click", dblclick="plot_dbl")
  )
)

# Server
shinyServer <- function(input, output, session) {
  
  iris_k <- reactive({
    kmeans(iris[1:4], centers=input$numCenter)
  })
  
  output$plot1 <- renderPlot({
    ggplot() + geom_point(data=iris, aes_string(input$xaxisGrp, input$yaxisGrp), colour=iris_k()$cluster)
  })
  
  output$info <- renderTable({
    nearPoints(iris, input$plot_click, xvar=input$xaxisGrp, yvar=input$yaxisGrp)[,c(input$xaxisGrp, input$yaxisGrp)]
  })
  
  v <- reactiveValues(
    selectedData = NULL
  )
  
  observeEvent(input$plot_dbl, {
    X1 <- nearPoints(iris, input$plot_dbl)
    if (is.null(v$selectedData)) {
      v$selectedData <- X1
    } else {
      if (nrow(merge(X1, v$selectedData)) > 0) {
        ind <- anyDuplicated(rbind(v$selectedData, X1), fromLast=TRUE)
        v$selectedData <- v$selectedData[-ind,]
      } else {
        v$selectedData <- rbind(v$selectedData, X1)
      }
    }
  })
  
  return(v)
}

# ShinyApp function call
shinyApp(ui=shinyUI, server=shinyServer)