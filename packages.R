#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  packages.R
# Purpose:      Install (if missing) and load required R packages.
#
# Author(s):    David Pedrosa
#
# Notes:
# - Project:     ISAfluct (2018–2026)
# - Repository:  https://github.com/dpedrosac/ISAfluct/
#
# Usage:
# - Source from other scripts: source("scripts/packages.R")
# =============================================================================

required_packages <- c(
  "afex",
  "dplyr",
  "haven",
  "ggcorrplot",
  "ggplot2",
  "ggpubr",
  "patchwork",
  "purrr",
  "RColorBrewer",
  "rlang",
  "tableone",
  "tidyverse"
)

install_if_missing <- function(pkgs, dependencies = TRUE) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = dependencies)
  }

  invisible(missing_pkgs)
}

load_packages <- function(pkgs) {
  invisible(
    lapply(pkgs, function(pkg) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
    })
  )
}

install_if_missing(required_packages)
load_packages(required_packages)

