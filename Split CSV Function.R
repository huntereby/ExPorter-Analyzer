library(readr)
library(dplyr)

# === Settings ===
input_file <- "RePORTER_PRJABS_C_FY2024.csv"   # path to your CSV file
rows_per_chunk <- 1000                         # how many rows per file
output_prefix <- "reporter_split_"             # name prefix for chunks
output_dir <- "split_files"                    # output folder name

# === Read CSV ===
df <- read_csv(input_file)

# === Create Output Directory ===
if (!dir.exists(output_dir)) dir.create(output_dir)

# === Split and Write Chunks ===
n_chunks <- ceiling(nrow(df) / rows_per_chunk)

for (i in 1:n_chunks) {
  start_row <- (i - 1) * rows_per_chunk + 1
  end_row <- min(i * rows_per_chunk, nrow(df))
  
  chunk_df <- df[start_row:end_row, ]
  
  out_file <- file.path(output_dir, paste0(output_prefix, i, ".csv"))
  write_csv(chunk_df, out_file)
  
  cat("Wrote", out_file, "with rows", start_row, "to", end_row, "\n")
}
