library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)

library(readr)
library(dplyr)

# === Settings ===
split_folder <- "split_files"               # Folder with split CSVs
file_pattern <- "reporter_split_.*\\.csv"   # Regex pattern to match files

# === List Files ===
files <- list.files(split_folder, pattern = file_pattern, full.names = TRUE)

# === Loop through each split file ===
for (file in files) {
  cat("Processing:", file, "\n")
  
  # Read the file
  chunk_df <- read_csv(file, show_col_types = FALSE)
  
  # Example operation: print summary or count abstracts
  cat("Rows:", nrow(chunk_df), " | Columns:", ncol(chunk_df), "\n")
  
  # You could also:
  # - Push each to Git one at a time
  # - Clean/standardize
  # - Extract keywords or save a summary

  # For example: count how many abstracts are not empty
  if ("abstract_text" %in% names(chunk_df)) {
    valid_abstracts <- sum(!is.na(chunk_df$abstract_text) & chunk_df$abstract_text != "")
    cat("Valid abstracts:", valid_abstracts, "\n")
  }
  
  cat("-----\n")
}

# Prepare word frequency table
get_top_terms <- function(df, n = 10) {
  df %>%
    unnest_tokens(word, abstract) %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!word %in% stop_words$word, str_detect(word, "^[a-z]")) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = n)
}

ui <- fluidPage(
  titlePanel("NIH Grant Method Frequency (2023-2024)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("fetch", "Fetch Abstracts")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

server <- function(input, output) {
  abstracts <- eventReactive(input$fetch, {
    fetch_abstracts()
  })

  top_terms <- reactive({
    req(abstracts())
    get_top_terms(abstracts())
  })

  output$barPlot <- renderPlot({
    req(top_terms())
    ggplot(top_terms(), aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Method", y = "Frequency")
  })
}

shinyApp(ui, server)
