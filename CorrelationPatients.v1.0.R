#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  CorrelationPatients.v1.0.R
# Purpose:      Compute Correlations (patients only) across clinical,
#               behavioural, and DOT measures; adjust p-values (BH), print
#               significant correlations, and export a clustered correlation plot
#
# Author(s):    Jan-Philipp Bach, Dilara Bingöl, Andreas Mäckel,
#		Franziska Maier, Josefine Waldthaler, David Pedrosa
#
# - Project:    ISAfluct (2018–2026)
# - Repository: https://github.com/dpedrosac/ISAfluct/
#
# Usage: 	source("scripts/CorrelationPatients.v1.0.R")
# =============================================================================

# ---- Configuration ------------------------------------------------------------

vars_raw <- c(
  "Alter",
  "Krankheitsdauer_Jahre",
  "BDI",
  "BIS_motor",
  "BIS_attention",
  "BIS_non.planning",
  "BIS_gesamt",
  "PANDA_gesamt",
  "PD_NMS",
  "MSS",
  "AES",
  "MDS_UPDRS_III_Mittelwert",
  "MDS_UPDRS_I",
  "MDS_UPDRS_II",
  "MDS_UPDRS_IV",
  # "ISAm_total",
  "ISAm_LID_total",
  "ISAm_Hypokin_total",
  "Tagesäqiuvalenzdosis_L_DOPA_mg",
  "subj_ID",
  "DOT_Klicks",
  "DAA",
  "DOT_latency"
)

vars_labels <- c(
  "Age",
  "Disease duration [years]",
  "BDI-II",
  "BIS11 (motor)",
  "BIS11 (attention)",
  "BIS11 (non.planning)",
  "BIS (total)",
  "PANDA",
  "Non-motor symptoms",
  "MSS",
  "AES",
  "MDS-UPDRS part III",
  "MDS-UPDRS part I",
  "MDS-UPDRS part II",
  "MDS-UPDRS part IV",
  # "ISAm Score",
  "ISAm Score (LID)",
  "ISAm Score (Hypokinesia)",
  "LEDD",
  "subj_ID",
  "DOT, Klicks",
  "DAA",
  "DOT, Latency"
)

if (length(vars_raw) != length(vars_labels)) {
  stop("vars_raw and vars_labels must have the same length.", call. = FALSE)
}

# ---- Prepare patient-only numeric data ---------------------------------------

df_patients_corr <- df_total %>%
  dplyr::filter(group == "patient") %>%
  dplyr::select(dplyr::all_of(vars_raw))

colnames(df_patients_corr) <- vars_labels

df_numeric <- df_patients_corr %>%
  dplyr::select(where(is.numeric))

# ---- Helper: Kendall correlation matrix + p-values ---------------------------

compute_kendall_cor_p <- function(df) {
  n <- ncol(df)

  corr_mat <- matrix(NA_real_, nrow = n, ncol = n)
  pval_mat <- matrix(NA_real_, nrow = n, ncol = n)

  for (i in seq_len(n)) {
    for (j in i:n) {
      if (i == j) {
        corr_mat[i, j] <- 1
        pval_mat[i, j] <- NA_real_
      } else {
        test <- stats::cor.test(
          df[[i]],
          df[[j]],
          use    = "complete.obs",
          method = "kendall",
          exact  = FALSE
        )

        corr_mat[i, j] <- unname(test$estimate)
        corr_mat[j, i] <- unname(test$estimate)
        pval_mat[i, j] <- test$p.value
        pval_mat[j, i] <- test$p.value
      }
    }
  }

  colnames(corr_mat) <- colnames(df)
  rownames(corr_mat) <- colnames(df)
  colnames(pval_mat) <- colnames(df)
  rownames(pval_mat) <- colnames(df)

  list(
    correlations = corr_mat,
    pvalues      = pval_mat
  )
}

cor_results <- compute_kendall_cor_p(df_numeric)

correlations <- cor_results$correlations
pvalues      <- cor_results$pvalues

# ---- Long format + p-adjustment (BH) -----------------------------------------

corr_long <- as.data.frame(as.table(correlations))
pval_long <- as.data.frame(as.table(pvalues))

cor_pval_long <- dplyr::left_join(
  corr_long,
  pval_long,
  by = c("Var1", "Var2")
)

colnames(cor_pval_long) <- c("Var1", "Var2", "Correlation", "PValue")

cor_pval_long <- cor_pval_long %>%
  dplyr::mutate(AdjPValue = stats::p.adjust(PValue, method = "BH"))

significant_corr <- cor_pval_long %>%
  dplyr::filter(AdjPValue < 0.05) %>%
  dplyr::arrange(AdjPValue)

print(significant_corr)

# ---- Cluster plot (set nonsignificant to 0 for clustering) -------------------

adjusted_cor_matrix <- correlations
adjusted_cor_matrix[pvalues > 0.05] <- 0

plot4 <- ggcorrplot::ggcorrplot(
  adjusted_cor_matrix,
  tl.cex         = 18,
  type           = "lower",
  lab            = FALSE,
  hc.order       = TRUE,
  colors         = c("blue", "white", "red"),
  outline.color  = NA,
  p.mat          = pvalues,
  sig.level      = 0.05,
  insig          = "blank"
)

# ---- Custom labels with significance asterisks -------------------------------

plot_data <- plot4[["data"]] %>%
  dplyr::mutate(
    label = dplyr::case_when(
      is.na(pvalue)   ~ "",
      pvalue < 0.01   ~ paste0(value, "**"),
      pvalue < 0.05   ~ paste0(value, "*"),
      TRUE            ~ as.character(value)
    )
  ) %>%
  dplyr::filter(signif == 1) %>%
  dplyr::select(-signif)

plot4 <- plot4 +
  ggplot2::geom_text(
    data    = plot_data,
    ggplot2::aes(x = Var1, y = Var2, label = label),
    color   = "black",
    nudge_y = 0.01,
    size    = 7
  ) +
  ggplot2::theme(
    axis.text.x       = ggplot2::element_text(size = 20, angle = 45, hjust = 1),
    axis.text.y       = ggplot2::element_text(size = 20),
    legend.title      = ggplot2::element_text(size = 20, face = "bold"),
    legend.text       = ggplot2::element_text(size = 20),
    legend.key.size   = grid::unit(1.5, "cm"),
    legend.key.width  = grid::unit(1, "cm")
  )

# ---- Export ------------------------------------------------------------------

output_path <- file.path(wdir, "results", "figure4.correlations.v1.0.png")

ggplot2::ggsave(
  filename = output_path,
  plot     = plot4,
  device   = "png",
  width    = 22,
  height   = 34,
  dpi      = 300
)

