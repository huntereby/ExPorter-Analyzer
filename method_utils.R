# Utility functions for analyzing NIH abstracts in split files

library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)

#' Read all CSVs from a folder and combine into a single data frame
#'
#' @param folder Folder containing CSVs
#' @param pattern Regex pattern to match file names
#' @return Data frame with columns `id` and `abstract`
read_split_files <- function(folder = "split_files",
                             pattern = "reporter_split_.*\\.csv") {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No files found in folder")
  }
  df_list <- lapply(files, function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      rename(id = 1, abstract = 2)
  })
  bind_rows(df_list)
}

#' Compute most common words (methods) in abstracts
#'
#' @param df Data frame with column `abstract`
#' @param n Number of words to return
#' @return Tibble with columns `word` and `n`
get_top_methods <- function(df, n = 10) {
  df %>%
    unnest_tokens(word, abstract) %>%
    mutate(word = str_to_lower(word)) %>%
    filter(!word %in% stop_words$word, str_detect(word, "^[a-z]")) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = n)
}

#' Generate a bar plot of the most common methods
#'
#' @param folder Folder containing CSVs
#' @param pattern Regex pattern for file names
#' @param n Number of words to display
#' @return ggplot object
plot_top_methods <- function(folder = "split_files",
                             pattern = "reporter_split_.*\\.csv",
                             n = 10) {
  df <- read_split_files(folder, pattern)
  top_words <- get_top_methods(df, n)
  ggplot(top_words, aes(x = reorder(word, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(x = "Method", y = "Frequency",
         title = "Most Common Methods")
}
