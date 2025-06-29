# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)

source("method_utils.R")

# === Function to load all split abstract chunks ===
load_split_abstracts <- function(folder = "split_files", pattern = "^reporter_split_\\d+\\.csv$") {
  tryCatch(
    read_split_files(folder, pattern),
    error = function(e) tibble(id = integer(), abstract = character())
  )
}

# === Plot 1: Top methods ===
plot_top_methods <- function(df) {
  top_words <- get_top_methods(df, n = 10)
  ggplot(top_words, aes(x = reorder(word, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top 10 Most Frequent Words",
      x = "Word",
      y = "Count"
    )
}

# === Plot 2: Keyword-matched common methods ===
plot_common_methods <- function(df) {
  common_keywords <- c(
    "Mass Spectrometry" = "mass\\s*spec|mass spectrometry",
    "Transcriptomics" = "transcriptomics|rna[- ]?seq|gene expression",
    "Flow Cytometry" = "flow cytometry|facs",
    "Western Blot" = "western blot",
    "Microscopy" = "microscopy|confocal|fluorescence",
    "Cryo-EM" = "cryo[- ]?em",
    "PCR/qPCR" = "\\b(pcr|qpcr|real[- ]?time pcr)\\b"
  )
  
  results <- lapply(names(common_keywords), function(label) {
    pattern <- common_keywords[[label]]
    count <- df %>%
      filter(str_detect(tolower(abstract), pattern)) %>%
      nrow()
    tibble(method = label, count = count)
  }) %>%
    bind_rows()
  
  ggplot(results, aes(x = reorder(method, count), y = count)) +
    geom_col(fill = "darkorange") +
    coord_flip() +
    labs(
      title = "Keyword-Matched Common Methods",
      x = "Method",
      y = "Count"
    )
}

# === UI ===
ui <- fluidPage(
  titlePanel("NIH Grant Method Frequency (2023–2024)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("analyze", "Analyze Split Files")
    ),
    mainPanel(
      plotOutput("barPlot1"),
      plotOutput("barPlot2")
    )
  )
)

# === Server ===
server <- function(input, output) {
  method_data <- eventReactive(input$analyze, {
    load_split_abstracts()
  })
  
  output$barPlot1 <- renderPlot({
    req(method_data())
    plot_top_methods(method_data())
  })
  
  output$barPlot2 <- renderPlot({
    req(method_data())
    plot_common_methods(method_data())
  })
}

# === Run App ===
shinyApp(ui, server)
