#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  extract_latencies.R
# Purpose:      Read DOT raw files and compute mean latencies with filtering
#               and simple outlier handling.
#
# Author(s):    Jan-Philipp Bach, Dilara Bingöl, Andreas Mäckel,
#		Franziska Maier, Josefine Waldthaler, David Pedrosa
#
# Notes:
# - Project:    ISAfluct (2018–2026)
# - Repository: https://github.com/dpedrosac/ISAfluct/
#
# Usage:       	source("scripts/extract_latencies.R")
# =============================================================================

# ---- Latency extraction -------------------------------------------------------

read_latency <- function(
  file_path,
  min_latency = 150,
  sd_mult = 5,
  drop_first_last = TRUE
) {
  if (!is.character(file_path) || length(file_path) != 1L) {
    stop("file_path must be a single character string.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }

  data <- tryCatch(
    utils::read.table(
      file_path,
      header = TRUE,
      sep = "\t",
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      stop("Error reading file '", file_path, "': ", e$message, call. = FALSE)
    }
  )

  # Harmonize/validate required columns
  # Keep strict defaults, but allow minor naming variants.
  names_lower <- tolower(names(data))

  latency_col <- names(data)[match("latency", names_lower)]
  prev_col <- names(data)[match("values.previousoutcome", names_lower)]

  # Optional: tolerate common alternatives for prev_col
  if (is.na(prev_col)) {
    prev_col <- names(data)[match("values_previousoutcome", names_lower)]
  }
  if (is.na(prev_col)) {
    prev_col <- names(data)[match("previousoutcome", names_lower)]
  }

  if (is.na(latency_col) || is.na(prev_col)) {
    stop(
      "Missing required columns in '", file_path, "'. Required: 'latency' and ",
      "'values.previousoutcome' (or supported aliases). Found: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  # Drop first/last row (only if there are enough rows)
  if (isTRUE(drop_first_last) && nrow(data) >= 3) {
    data <- data[-c(1, nrow(data)), , drop = FALSE]
  }

  # Coerce latency to numeric and drop NA
  data[[latency_col]] <- suppressWarnings(as.numeric(data[[latency_col]]))
  data <- data[!is.na(data[[latency_col]]), , drop = FALSE]

  if (nrow(data) == 0) {
    return(list(mean_valid = NA_real_, mean_prev = NA_real_, mean_other = NA_real_))
  }

  # Filter latencies: >= min_latency and <= mean + sd_mult * sd
  mu <- mean(data[[latency_col]])
  s  <- stats::sd(data[[latency_col]])

  # If sd is NA (e.g., only one observation), skip the upper-bound filter
  if (is.na(s) || s == 0) {
    keep <- data[[latency_col]] >= min_latency
  } else {
    keep <- data[[latency_col]] >= min_latency & data[[latency_col]] <= (mu + sd_mult * s)
  }

  data <- data[keep, , drop = FALSE]

  if (nrow(data) == 0) {
    return(list(mean_valid = NA_real_, mean_prev = NA_real_, mean_other = NA_real_))
  }

  prev_vals <- data[[prev_col]]

  list(
    mean_valid = mean(data[[latency_col]], na.rm = TRUE),
    mean_prev  = mean(data[[latency_col]][prev_vals == 2], na.rm = TRUE),
    mean_other = mean(data[[latency_col]][prev_vals == 1], na.rm = TRUE)
  )
}

