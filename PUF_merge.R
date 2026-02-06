#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

input_dir <- "final_data"
output_dir <- "final_data_merged"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# Merge filtering_summary.csv files (sum by step)
# -----------------------------
summary_files <- list.files(
  input_dir,
  pattern = "filtering_summary\\.csv$",
  full.names = TRUE
)

if (length(summary_files) == 0) {
  message("No *filtering_summary.csv files found in ", input_dir)
} else {
  summaries <- lapply(summary_files, function(f) {
    read_csv(f, show_col_types = FALSE)
  })

  step_order <- summaries[[1]]$step

  summary_all <- bind_rows(summaries)

  if (!("step" %in% names(summary_all))) {
    stop("Merged filtering summaries do not contain a 'step' column.")
  }

  numeric_cols <- names(summary_all)[vapply(summary_all, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, "step")

  if (length(numeric_cols) == 0) {
    stop("No numeric columns found to sum in filtering summaries (expected e.g. 'n').")
  }

  summary_merged <- summary_all %>%
    group_by(step) %>%
    summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  summary_merged <- summary_merged %>%
  mutate(step = factor(step, levels = step_order)) %>%
  arrange(step) %>%
  mutate(step = as.character(step))

  out_summary <- file.path(output_dir, "filtering_summary.csv")
  write_csv(summary_merged, out_summary)
  message("Wrote merged filtering summary to: ", out_summary)
}

# -----------------------------
# Merge cleaned.csv files (row-bind)
# -----------------------------
cleaned_files <- list.files(
  input_dir,
  pattern = "cleaned\\.csv$",
  full.names = TRUE
)

if (length(cleaned_files) == 0) {
  message("No *cleaned.csv files found in ", input_dir)
} else {
  # Read first file to define canonical column names and order
  first_df <- read_csv(cleaned_files[1], show_col_types = FALSE)
  canonical_names <- names(first_df)

  dfs <- vector("list", length(cleaned_files))
  dfs[[1]] <- first_df

  if (length(cleaned_files) > 1) {
    for (i in 2:length(cleaned_files)) {
      df_i <- read_csv(cleaned_files[i], show_col_types = FALSE)

      if (!identical(names(df_i), canonical_names)) {
        stop(
          "Column names/order mismatch in file: ", cleaned_files[i], "\n",
          "Expected: ", paste(canonical_names, collapse = ", "), "\n",
          "Got:      ", paste(names(df_i), collapse = ", ")
        )
      }

      dfs[[i]] <- df_i
    }
  }

  cleaned_merged <- bind_rows(dfs)

  out_cleaned <- file.path(output_dir, "cleaned.csv")
  write_csv(cleaned_merged, out_cleaned)
  message("Wrote merged cleaned data to: ", out_cleaned)
}
