# ExPorter-Analyzer

This repository contains a simple R Shiny application that scans grant abstracts
stored in the `split_files` folder and displays the most common terms found in
those texts. The app automatically reads all `reporter_split_*.csv` files and
extracts word frequencies from the `ABSTRACT_TEXT` column.

## Running the application

1. Install the required packages:

```r
install.packages(c("shiny", "dplyr", "tidytext", "stringr", "ggplot2", "readr"))
```

2. Launch the app from R:

```r
shiny::runApp("app.R")
```

Press **Analyze Split Files** in the sidebar to read the CSV files and generate
a bar chart of the most frequent words (methods) mentioned.
