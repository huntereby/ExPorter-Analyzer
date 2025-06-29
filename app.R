# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

# === Function to load all split abstract chunks ===
load_split_abstracts <- function(folder = "split_files", pattern = "^reporter_split_\\d+\\.csv$") {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(tibble(method = character()))
  
  df_list <- lapply(files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    if ("method" %in% names(df)) {
      df %>%
        filter(!is.na(method), method != "") %>%
        select(method)
    } else {
      tibble(method = character())
    }
  })
  
  bind_rows(df_list)
}

# === Plot 1: Top methods ===
plot_top_methods <- function(df) {
  df %>%
    count(method = tolower(str_trim(method))) %>%
    filter(!is.na(method), method != "") %>%
    slice_max(n, n = 10) %>%
    ggplot(aes(x = reorder(method, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top 10 Most Frequent Methods",
      x = "Method",
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
      filter(str_detect(tolower(method), pattern)) %>%
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
