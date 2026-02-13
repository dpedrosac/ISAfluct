#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  RMpatientData.v1.1.R
# Purpose:      Analyses of within-day repeated measures (T1–T3) and plots.
#
# Author(s):    Jan-Philipp Bach; Dilara Bingöl; Andreas Mäckel;
#               Franziska Maier; Josefine Waldthaler; David Pedrosa
#
# Notes:
# - Project:     ISAfluct (2018–2026)
# - Repository:  https://github.com/dpedrosac/ISAfluct/
# - Usage:       source("scripts/RMpatientData.v1.1.R")
# =============================================================================

# ---- Repeated measures throughout the day ------------------------------------

variables_rm <- c(
  "MDS_UPDRS_III",
  "ISA_m_LID",
  "ISA_m_Hypokin",
  # "BIS",
  "DOT_Klicks",
  "DOT_latency",
  "Schmerz_VAS_Rang"
)

time_labels <- c(T1 = "morning", T2 = "noon", T3 = "evening")

rename_map <- c(
  MDS_UPDRS_III    = "MDS-UPDRS part III",
  ISA_m_LID        = "ISA motor score (LID related)",
  ISA_m_Hypokin    = "ISA motor score (OFF related)",
  BIS              = "Barrett Impulsivity Scale-11",
  DOT_Klicks       = "Door Opening Task, Clicks",
  DOT_latency      = "Door Opening Task, Latency",
  Schmerz_VAS_Rang = "Visual Analog Scale for Discomfort"
)

custom_order <- c(
  "MDS-UPDRS part III",
  "Barrett Impulsivity Scale-11",
  "ISA motor score (LID related)",
  "ISA motor score (OFF related)",
  "Door Opening Task, Clicks",
  "Door Opening Task, Latency",
  "Visual Analog Scale for Discomfort"
)

# ---- Wide -> long ------------------------------------------------------------

df_long_rm <- df_total %>%
  tidyr::pivot_longer(
    cols = dplyr::matches(
      paste0("^(", paste(variables_rm, collapse = "|"), ")_T[1-3]$")
    ),
    names_to = c("variable", "time"),
    names_pattern = "([a-zA-Z0-9_]+)_T([1-3])",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    time = factor(paste0("T", time), levels = c("T1", "T2", "T3")),
    group = haven::as_factor(group)
  )

# Drop haven labels to avoid downstream type issues (afex/ggplot)
df_long_rm <- df_long_rm %>%
  dplyr::mutate(
    dplyr::across(where(haven::is.labelled), ~ as.numeric(.x))
  )

# ---- Summary: mean ± SD ------------------------------------------------------

summary_stats_rm <- df_long_rm %>%
  dplyr::group_by(variable, time) %>%
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = stats::sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(mean_sd = paste0(round(mean, 2), " \u00B1 ", round(sd, 2)))

# ---- Repeated measures ANOVA (patients only; within = time) ------------------

anova_by_variable <- df_long_rm %>%
  dplyr::filter(group == "patient") %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    afex_model = list(
      afex::aov_car(value ~ time + Error(subj_ID / time), data = dplyr::cur_data())
    ),
    .groups = "drop"
  )

anova_summaries <- anova_by_variable %>%
  dplyr::mutate(
    f_value = purrr::map_dbl(afex_model, ~ .x$anova_table$F[1]),
    p_value = purrr::map_dbl(afex_model, ~ .x$anova_table$Pr[1]),
    sig = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  dplyr::select(variable, f_value, p_value, sig)

summary_table_rm <- summary_stats_rm %>%
  dplyr::select(variable, time, mean_sd) %>%
  tidyr::pivot_wider(
    names_from = time,
    values_from = mean_sd
  ) %>%
  dplyr::left_join(anova_summaries, by = "variable") %>%
  dplyr::mutate(
    item = dplyr::recode(variable, !!!rename_map),
    item = factor(item, levels = custom_order)
  ) %>%
  dplyr::arrange(item) %>%
  dplyr::select(item, T1, T2, T3, f_value, p_value, sig) %>%
  dplyr::rename(
    morning = T1,
    noon = T2,
    evening = T3,
    `F-value` = f_value,
    p = p_value,
    `sig.` = sig
  )

print(summary_table_rm)

utils::write.csv(
  summary_table_rm,
  file = file.path(wdir, "results", "table2.repeated_measures.v1.0.csv"),
  row.names = FALSE
)

# ---- Plot helper -------------------------------------------------------------
# Creates one subplot per variable:
# - boxplot + jitter by time
# - within-subject ANOVA (patients only), annotated with F/p

create_subplot <- function(data, variable_name, title = NULL) {
  pattern <- paste0("^", variable_name, "_T[1-3]$")

  df_long <- data %>%
    dplyr::select(subj_ID, group, dplyr::matches(pattern)) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches(pattern),
      names_to = "time",
      names_pattern = ".*_(T[1-3])$",
      values_to = "score"
    ) %>%
    dplyr::mutate(
      time  = factor(time, levels = c("T1", "T2", "T3")),
      group = haven::as_factor(group)
    ) %>%
    dplyr::filter(!is.na(subj_ID))

  df_long <- df_long %>%
    dplyr::mutate(dplyr::across(where(haven::is.labelled), ~ as.numeric(.x)))

  # Within-subject ANOVA (patients only), consistent with the table section
  afex_model <- afex::aov_car(
    score ~ time + Error(subj_ID / time),
    data = df_long %>% dplyr::filter(group == "patient")
  )

  f_val <- afex_model$anova_table$F[1]
  p_val <- afex_model$anova_table$Pr[1]

  annot <- sprintf("F = %.2f\np = %s", f_val, format.pval(p_val, digits = 2, eps = 0.001))
  y_annot <- max(df_long$score, na.rm = TRUE) * 1.1

  ggplot2::ggplot(df_long, ggplot2::aes(x = time, y = score, fill = group)) +
    ggplot2::geom_boxplot(
      width = 0.5,
      outlier.shape = NA,
      alpha = 0.5
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(x = time, y = score),
      color = "black",
      position = ggplot2::position_jitter(width = 0.15),
      size = 2,
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = 2,
      y = y_annot,
      label = annot,
      size = 5
    ) +
    ggplot2::scale_x_discrete(labels = time_labels) +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::labs(
      title = if (is.null(title)) paste(variable_name, "scores") else title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.position = "none"
    )
}

# ---- Assemble repeated measures plots ----------------------------------------

df_plot_rm <- df_total %>%
  dplyr::select(
    subj_ID, group,
    ISA_m_Hypokin_T1, ISA_m_Hypokin_T2, ISA_m_Hypokin_T3,
    ISA_m_LID_T1, ISA_m_LID_T2, ISA_m_LID_T3,
    #BIS_T1, BIS_T2, BIS_T3,
    MDS_UPDRS_III_T1, MDS_UPDRS_III_T2, MDS_UPDRS_III_T3
  )

plot_vars <- c("ISA_m_Hypokin", "ISA_m_LID", "MDS_UPDRS_III") # BIS removed
plot_titles <- c(
  MDS_UPDRS_III = "MDS-UPDRS, part III",
  ISA_m_Hypokin = "ISAm (Hypokinesia subscore)",
  ISA_m_LID = "ISAm (LID subscore)"
  #BIS = "Barrett Impulsiveness Scale-11",
)

subplots <- purrr::map(
  plot_vars,
  ~ create_subplot(df_plot_rm, variable_name = .x, title = plot_titles[[.x]])
)

combined_plot <- subplots[[1]] /
  subplots[[2]] /
  subplots[[3]] +
  patchwork::plot_layout(axis_titles = "collect") +
  patchwork::plot_annotation(tag_levels = "A") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 16))

print(combined_plot)

ggplot2::ggsave(
  filename = file.path(wdir, "results", "suppl.figure1.rmResults.v1.0.png"),
  plot     = combined_plot,
  device   = "png",
  width    = 10,
  height   = 18,
  dpi      = 300
)

