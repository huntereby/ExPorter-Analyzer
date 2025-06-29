# Utility functions for analyzing NIH abstracts in split files

library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)

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

#' Read project data files containing Administering IC information
#'
#' @param folder Folder with project CSVs
#' @param pattern Regex pattern for file names
#' @return Data frame with columns APPLICATION_ID and ADMINISTERING_IC
read_project_files <- function(folder = "split_files_ProjectData",
                               pattern = "reporter_split_.*\\.csv") {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No project files found in folder")
  }
  df_list <- lapply(files, function(f) {
    read_csv(
      f,
      show_col_types = FALSE,
      col_select = c(APPLICATION_ID, ADMINISTERING_IC)
    )
  })
  bind_rows(df_list)
}

#' Load abstracts and join with Administering IC information
#'
#' @param abstract_folder Folder with abstract CSVs
#' @param project_folder Folder with project CSVs
#' @param pattern Regex pattern for file names
#' @return Data frame with APPLICATION_ID, ADMINISTERING_IC and abstract
load_joined_data <- function(abstract_folder = "split_files",
                             project_folder = "split_files_ProjectData",
                             pattern = "reporter_split_.*\\.csv") {
  abstracts <- read_split_files(abstract_folder, pattern) %>%
    rename(APPLICATION_ID = id, abstract = abstract)
  projects <- read_project_files(project_folder, pattern)
  inner_join(abstracts, projects, by = "APPLICATION_ID")
}

#' Compute TF-IDF for bigrams grouped by Administering IC
#'
#' @param df Data frame returned by `load_joined_data()`
#' @param n Number of bigrams to keep for each IC
#' @return Data frame with columns ADMINISTERING_IC, bigram and tf_idf
#'   (bigrams containing common stop words are removed)
get_ic_bigram_tfidf <- function(df, n = 10) {
  df %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(ADMINISTERING_IC, bigram, sort = TRUE) %>%
    bind_tf_idf(bigram, ADMINISTERING_IC, n) %>%
    arrange(ADMINISTERING_IC, desc(tf_idf)) %>%
    group_by(ADMINISTERING_IC) %>%
    slice_head(n = n) %>%
    ungroup()
}

#' Plot TF-IDF bigrams for a specific Administering IC
#'
#' @param df Data frame of bigram tf-idf values
#' @param ic Character name of the Administering IC
#' @return ggplot object
plot_ic_tfidf <- function(df, ic) {
  ggplot(df, aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Top Methods -", ic), x = "Bigram", y = "tf-idf")
}

#' Precompute a named list of tf-idf results for each Administering IC
#'
#' @param df Data frame returned by `load_joined_data()`
#' @param n Number of bigrams to keep for each IC
#' @return Named list where each element is a tibble of the top `n` bigrams
precompute_ic_tfidf_list <- function(df, n = 10) {
  tfidf <- get_ic_bigram_tfidf(df, n)
  split(tfidf, tfidf$ADMINISTERING_IC)
}
