# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)

source("method_utils.R")

# === Precompute tf-idf list ===
ic_data <- tryCatch(load_joined_data(), error = function(e) tibble())
tfidf_list <- if (nrow(ic_data) > 0) precompute_ic_tfidf_list(ic_data, n = 10) else list()

# === Plot tf-idf bigrams for one IC ===
plot_ic <- function(df, ic) {
  plot_ic_tfidf(df, ic)
}

# === UI ===
ui <- fluidPage(
  titlePanel("NIH Grant Methods by Administering IC"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ic_select", "Administering IC", choices = names(tfidf_list))
    ),
    mainPanel(
      plotOutput("tfidfPlot")
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  output$tfidfPlot <- renderPlot({
    req(input$ic_select)
    df <- tfidf_list[[input$ic_select]]
    req(nrow(df) > 0)
    plot_ic(df, input$ic_select)
  })
}

# === Run App ===
shinyApp(ui, server)
