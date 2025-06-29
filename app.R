# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)

source("method_utils.R")

# === Function to load data joined with Administering IC ===
load_ic_data <- function() {
  tryCatch(
    load_joined_data(),
    error = function(e) tibble()
  )
}

# === Plot tf-idf bigrams for one IC ===
plot_ic <- function(df, ic) {
  plot_ic_tfidf(df, ic)
}

# === UI ===
ui <- fluidPage(
  titlePanel("NIH Grant Methods by Administering IC"),
  sidebarLayout(
    sidebarPanel(
      actionButton("analyze", "Analyze Split Files")
    ),
    mainPanel(
      uiOutput("ic_tabs")
    )
  )
)

# === Server ===
server <- function(input, output) {
  joined_data <- eventReactive(input$analyze, {
    withProgress(message = "Loading split files...", {
      load_ic_data()
    })
  })

  tfidf_data <- reactive({
    req(joined_data())
    withProgress(message = "Computing tf-idf...", {
      get_ic_bigram_tfidf(joined_data())
    })
  })

  output$ic_tabs <- renderUI({
    df <- tfidf_data()
    req(nrow(df) > 0)
    split_df <- split(df, df$ADMINISTERING_IC)
    tabs <- lapply(names(split_df), function(ic) {
      plotname <- paste0("plot_", ic)
      local_df <- split_df[[ic]]
      output[[plotname]] <- renderPlot({
        withProgress(message = paste("Rendering", ic, "plot..."), {
          plot_ic(local_df, ic)
        })
      })
      tabPanel(ic, plotOutput(plotname))
    })
    do.call(tabsetPanel, tabs)
  })
}

# === Run App ===
shinyApp(ui, server)
