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

When the app starts it automatically loads the CSV files, computes the top
TF‑IDF bigrams for each Administering IC and caches the results. Use the
dropdown menu to select an IC and view a bar plot of its top methods.
