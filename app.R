library(shiny)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(readr)

source("method_utils.R")

ui <- fluidPage(
  titlePanel("NIH Grant Method Frequency (2023-2024)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("analyze", "Analyze Split Files")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

server <- function(input, output) {
  plot_obj <- eventReactive(input$analyze, {
    plot_top_methods()
  })

  output$barPlot <- renderPlot({
    req(plot_obj())
    plot_obj()
  })
}

shinyApp(ui, server)
