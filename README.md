# ExPorter-Analyzer

This repository contains a simple R Shiny application that scans grant abstracts
stored in the `split_files` folder and displays the most common methods found in
those texts.

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
a bar chart of the most frequent methods.
