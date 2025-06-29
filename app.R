library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)

# Fetch abstracts for the provided fiscal years
fetch_abstracts <- function(fy = c(2023, 2024), limit = 500) {
  url <- "https://api.reporter.nih.gov/v2/projects/search"
  body <- list(
    criteria = list(
      fiscal_years = fy
    ),
    include_fields = list("abstract_text"),
    limit = limit
  )
  res <- POST(url, body = body, encode = "json")
  stop_for_status(res)
  data <- content(res, as = "parsed")
  if (!is.null(data$results)) {
    tibble(abstract = vapply(data$results, function(x) x$abstract_text, character(1)))
  } else {
    tibble(abstract = character())
  }
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
