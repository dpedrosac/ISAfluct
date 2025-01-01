# This is code aims toi compute and retuurn mean latencies with outlier handling and filtering
# Code developed by David Pedrosa

# Version 1.2 # 2024-29-12 # removed major bug, as valid values were not removed.

read_latency <- function(file_path) {
  if (!file.exists(file_path)) stop("File not found.")
  
  # Read the file
  data <- tryCatch(
    read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE),
    error = function(e) stop("Error reading file: ", e$message)
  )
  
  # Check required columns
  if (!all(c("latency", "values.previousoutcome") %in% colnames(data))) 
    stop("Missing required columns: 'latency' or 'values.previousoutcome'")
  
  # Exclude first and last rows, and remove NA
  data <- data[-c(1, nrow(data)), ]
  data <- data[!is.na(data$latency), ]
  
  # Filter latencies (values >= 200 and within 3 * SD of the mean)
  mean_latency <- mean(data$latency)
  sd_latency <- sd(data$latency)
  data <- data[data$latency >= 150 & data$latency <= (mean_latency + 5 * sd_latency), ]
  
  # If no valid rows remain, return NA for all values
  if (nrow(data) == 0) return(list(mean_valid = NA, mean_prev = NA, mean_other = NA))
  
  # Calculate means based on filtered data
  mean_valid <- mean(data$latency, na.rm = TRUE)
  mean_prev <- mean(data$latency[data$values.previousoutcome == 2], na.rm = TRUE)
  mean_other <- mean(data$latency[data$values.previousoutcome == 1], na.rm = TRUE)
  
  # Return means
  list(
    mean_valid = mean_valid,
    mean_prev = mean_prev,
    mean_other = mean_other
  )
}
