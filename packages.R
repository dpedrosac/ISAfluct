packages <- c(
  "afex",
  "dplyr", 
  "haven",
  "ggcorrplot",
  "ggplot2",
  "ggpubr",
  "patchwork",
  "purrr",
  "rlang",
  "RColorBrewer",
  "tableone", 
  "tidyverse"
)

## Now load or install & load all iff necessary
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
