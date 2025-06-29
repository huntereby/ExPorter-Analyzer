# Load required libraries
library(shiny)
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)

# Load stop words
data("stop_words", package = "tidytext")

# === Function to load all split abstract chunks ===
load_abstract_chunks <- function(folder = "split_files", pattern = "^reporter_split_\\d+\\.csv$") {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    return(tibble(abstract = character()))
  }
  
  data_list <- lapply(files, function(f) {
    df <- read_csv(f, show_col_types = FALSE)
    if ("abstract_text" %in% names(df)) {
      df %>%
        filter(!is.na(abstract_text), abstract_text != "") %>%
        select(abstract = abstract_text)
    } else {
      tibble(abstract = character())
    }
  })
  
  bind_rows(data_list)
}

# === Function to compute top terms ===
get_top_terms <- function(df, n = 10) {
  df %>%
    unnest_tokens(word, abstract) %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "^[a-z]+$")) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = n)
}

# === UI ===
ui <- fluidPage(
  titlePanel("NIH Abstract Word Frequency (Local CSVs)"),
  sidebarLayout(
    sidebarPanel(
      actionButton("load", "Load Abstracts from split_files/"),
      textOutput("countText")
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  abstracts <- reactiveVal()
  
  observeEvent(input$load, {
    showNotification("Loading abstracts from local files...", type = "message")
    abs_df <- load_abstract_chunks()
    abstracts(abs_df)
    
    if (nrow(abs_df) == 0) {
      showNotification("No valid abstracts found in split_files/.", type = "error")
    } else {
      showNotification(paste("Loaded", nrow(abs_df), "abstracts."), type = "message")
    }
  })
  
  output$countText <- renderText({
    req(abstracts())
    paste("Total abstracts loaded:", nrow(abstracts()))
  })
  
  output$barPlot <- renderPlot({
    req(abstracts())
    top_terms <- get_top_terms(abstracts(), n = 10)
    
    ggplot(top_terms, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top 10 Most Frequent Words in NIH Abstracts",
        x = "Word",
        y = "Frequency"
      )
  })
}

# === Run the app ===
shinyApp(ui, server)
