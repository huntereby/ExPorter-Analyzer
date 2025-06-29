# ExPorter-Analyzer

This repository contains a simple R Shiny application that uses the NIH `repoRter` API to retrieve abstracts from 2023–2024 funded projects and displays the most common terms found in those abstracts.

## Running the application

1. Install the required packages:

```r
install.packages(c("shiny", "httr", "jsonlite", "dplyr", "tidytext", "stringr", "ggplot2"))
```

2. Launch the app from R:

```r
shiny::runApp("app.R")
```

Press **Fetch Abstracts** in the sidebar to download the abstracts and generate the bar chart of the most frequent terms.
