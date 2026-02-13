#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  DOTanalyses.v1.0.R
# Purpose:      Analyses of the Door-Opening-Task (DOT) outcomes.
#
#               The non-commercial DOT implementation is available at:
#               https://www.millisecond.com/download/library/dooropeningtask
#
#               Reference:
#               Matthys, W., van Goozen, S. H. M., de Vries, H.,
#               Cohen-Kettenis, P. T., & van Engeland, H. (1998).
#               The Dominance of Behavioural Activation over Behavioural
#               Inhibition in Conduct Disordered Boys with or without Attention
#               Deficit Hyperactivity Disorder. Journal of Child Psychology and
#               Psychiatry, 5, 643–651.
#
# Author(s):    Jan-Philipp Bach; Dilara Bingöl; Andreas Mäckel;
#               Franziska Maier; Josefine Waldthaler; David Pedrosa
#
# Notes:
# - Project:     ISAfluct (2018–2026)
# - Repository:  https://github.com/dpedrosac/ISAfluct/
# - Usage:       source("scripts/DOTanalyses.v1.0.R")
# =============================================================================

time_labels <- c(
  "T1" = "morning",
  "T2" = "noon",
  "T3" = "evening"
)

feedback_labels <- c(
  "DOT_npE_latency" = "After positive\n answer",
  "DOT_pE_latency"  = "After negative\n answer"
)

# ---- Reshape DOT clicks (T1–T3) to long --------------------------------------

df_longISAb <- df_total %>%
  tidyr::pivot_longer(
    cols          = dplyr::starts_with("DOT_Klicks"),
    names_to      = c("Score_Type", "Time"),
    names_pattern = "(DOT_Klicks)_(T\\d+)",
    values_to     = "DOT_Klicks"
  )

# ---- Plot 2A: DOT clicks across time of day and groups -----------------------

plot2A <- ggplot2::ggplot(
  df_longISAb %>% dplyr::filter(!is.na(Time)),
  ggplot2::aes(x = Time, y = DOT_Klicks, fill = group)
) +
  ggplot2::geom_boxplot(
    outlier.shape = NA,
    alpha         = 0.5,
    position      = ggplot2::position_dodge(1.1),
    lwd           = 0.6,
    fatten        = 0
  ) +
  ggplot2::stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = ggplot2::position_dodge(1.1),
    width    = 0.8,
    size     = 1
  ) +
  ggplot2::geom_jitter(
    ggplot2::aes(group = group),
    position = ggplot2::position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 1.1
    ),
    size  = 2,
    alpha = 0.5
  ) +
  ggplot2::labs(
    title = "DOT results over the day",
    x     = NULL,
    y     = "Opened doors in \nDoor-Opening-Task (DOT)*"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::scale_x_discrete(
    labels = time_labels,
    expand = ggplot2::expansion(mult = c(0.5, 0.5))
  ) +
  ggplot2::theme(
    plot.title   = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text    = ggplot2::element_text(size = 14),
    axis.title.x = ggplot2::element_text(size = 16),
    axis.title.y = ggplot2::element_text(size = 16),
    legend.title = ggplot2::element_blank()
  )

# ---- Plot 2B: latency difference between groups ------------------------------

t_test_result <- stats::t.test(DOT_latency ~ group, data = df_total)

t_stat  <- round(t_test_result$statistic, 2)
p_value <- t_test_result$p.value
p_value_text <- ifelse(
  p_value < 0.001,
  "p < 0.001",
  sprintf("p = %.3f", p_value)
)

lat_max <- max(df_total$DOT_latency, na.rm = TRUE)

plot2B <- ggplot2::ggplot(
  df_total,
  ggplot2::aes(x = group, y = DOT_latency, fill = group)
) +
  ggplot2::geom_boxplot(
    outlier.shape = NA,
    alpha         = 0.5,
    lwd           = 0.6,
    fatten        = 0
  ) +
  ggplot2::stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = ggplot2::position_dodge(1.1),
    width    = 0.8,
    size     = 1
  ) +
  ggplot2::geom_jitter(
    position = ggplot2::position_jitter(width = 0.2),
    size     = 2,
    alpha    = 0.5
  ) +
  ggplot2::annotate(
    "segment",
    x    = 1,
    xend = 2,
    y    = lat_max * 1.05,
    yend = lat_max * 1.05,
    size = 0.8
  ) +
  ggplot2::annotate(
    "text",
    x     = 1.5,
    y     = lat_max * 1.1,
    label = sprintf("t = %.2f, %s", t_stat, p_value_text),
    size  = 5
  ) +
  ggplot2::labs(
    title = "Latency for DOT responses",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text       = ggplot2::element_text(size = 14),
    axis.title.y    = ggplot2::element_text(size = 16),
    axis.title.x    = ggplot2::element_blank(),
    legend.position = "none"
  )

# ---- Latency: effect of feedback (positive/negative answer) ------------------

df_long <- df_total %>%
  dplyr::select(group, DOT_npE_latency, DOT_pE_latency) %>%
  tidyr::pivot_longer(
    cols      = dplyr::starts_with("DOT_"),
    names_to  = "Latency_Type",
    values_to = "Latency"
  ) %>%
  dplyr::mutate(
    Latency_Type = factor(
      Latency_Type,
      levels = c("DOT_npE_latency", "DOT_pE_latency")
    )
  )

anova_result <- stats::aov(Latency ~ Latency_Type * group, data = df_long)
summary(anova_result)

# t-tests per Latency_Type (group differences)
t_test_results <- df_long %>%
  dplyr::group_by(Latency_Type) %>%
  dplyr::summarise(
    t_stat  = round(stats::t.test(Latency ~ group)$statistic, 2),
    p_value = stats::t.test(Latency ~ group)$p.value,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    p_value_text = ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.3f", p_value))
  )

# Interaction term for ANOVA (same model; reuse summary)
anova_summary <- summary(anova_result)
interaction_f <- round(anova_summary[[1]]$`F value`[3], 2)
interaction_p <- anova_summary[[1]]$`Pr(>F)`[3]
interaction_txt <- ifelse(
  interaction_p < 0.001,
  "p < 0.001",
  sprintf("p = %.3f", interaction_p)
)

overall_y_min <- min(df_long$Latency, na.rm = TRUE)
latency_max   <- max(df_long$Latency, na.rm = TRUE)

# OLD version: boxplot with multiple annotations (kept for reference)
plot2CboxplotOLD <- ggplot2::ggplot(
  df_long,
  ggplot2::aes(x = Latency_Type, y = Latency, fill = group)
) +
  ggplot2::geom_boxplot(
    outlier.shape = NA,
    alpha         = 0.5,
    position      = ggplot2::position_dodge(1.1),
    lwd           = 0.6,
    fatten        = 0
  ) +
  ggplot2::stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = ggplot2::position_dodge(1.1),
    width    = 0.8,
    size     = 1
  ) +
  ggplot2::geom_jitter(
    ggplot2::aes(group = interaction(Latency_Type, group)),
    position = ggplot2::position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 0.8
    ),
    size  = 2,
    alpha = 0.5
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x    = 0.75,
      xend = 1.25,
      y    = latency_max * 1.05,
      yend = latency_max * 1.05
    ),
    size  = 0.8,
    color = "black"
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x    = 1.75,
      xend = 2.25,
      y    = latency_max * 1.05,
      yend = latency_max * 1.05
    ),
    size  = 0.8,
    color = "black"
  ) +
  ggplot2::geom_text(
    data = t_test_results,
    ggplot2::aes(
      x     = Latency_Type,
      y     = latency_max * 1.1,
      label = paste0("t = ", t_stat, "\n", p_value_text)
    ),
    inherit.aes = FALSE,
    size        = 4,
    position    = ggplot2::position_dodge(width = 0.8)
  ) +
  ggplot2::annotate(
    "text",
    x     = 1.5,
    y     = overall_y_min * 0.9,
    label = paste0("Interaction: F = ", interaction_f, ", ", interaction_txt),
    size  = 5,
    hjust = 0
  ) +
  ggplot2::labs(
    title = "Latency Differences",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::scale_x_discrete(
    labels = c(
      "DOT_npE_latency" = "After positive answer",
      "DOT_pE_latency"  = "After negative answer"
    ),
    expand = ggplot2::expansion(mult = c(0.5, 0.5))
  ) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text       = ggplot2::element_text(size = 16),
    axis.title.y    = ggplot2::element_text(size = 16),
    axis.title.x    = ggplot2::element_blank(),
    legend.position = "none"
  )

# Newer visualization: line plot of means + SEM
df_summary <- df_long %>%
  dplyr::group_by(group, Latency_Type) %>%
  dplyr::summarise(
    mean_latency = mean(Latency, na.rm = TRUE),
    sem_latency  = stats::sd(Latency, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups      = "drop"
  )

plot2C <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data  = df_summary,
    ggplot2::aes(x = Latency_Type, y = mean_latency, group = group, color = group),
    size  = 1,
    alpha = 0.5
  ) +
  ggplot2::geom_point(
    data  = df_summary,
    ggplot2::aes(x = Latency_Type, y = mean_latency, color = group),
    size  = 3,
    alpha = 0.5
  ) +
  ggplot2::geom_errorbar(
    data  = df_summary,
    ggplot2::aes(
      x    = Latency_Type,
      ymin = mean_latency - sem_latency,
      ymax = mean_latency + sem_latency
    ),
    width = 0.1,
    size  = 0.5,
    alpha = 0.5
  ) +
  ggplot2::labs(
    title = "Latency Differences",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_brewer(palette = "Set1") +
  ggplot2::scale_x_discrete(
    labels = feedback_labels,
    expand = ggplot2::expansion(mult = c(0.1, 0.1))
  ) +
  ggplot2::ylim(0, 6300) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text        = ggplot2::element_text(size = 14),
    axis.title.y     = ggplot2::element_blank(),
    axis.title.x     = ggplot2::element_blank(),
    legend.position  = "none",
    legend.title     = ggplot2::element_blank()
  )

# Alternative: boxplot + jitter for feedback types
plot2C_box <- ggplot2::ggplot(
  data = df_long,
  ggplot2::aes(x = Latency_Type, y = Latency, fill = group)
) +
  ggplot2::geom_jitter(
    ggplot2::aes(size = 2, alpha = 0.5),
    position = ggplot2::position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 0.7
    ),
    size  = 1.8,
    alpha = 0.5
  ) +
  ggplot2::geom_boxplot(
    width         = 0.6,
    position      = ggplot2::position_dodge(width = 0.7),
    outlier.shape = NA,
    alpha         = 0.5
  ) +
  ggplot2::stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = ggplot2::position_dodge(0.7),
    width    = 0.8,
    size     = 1
  ) +
  ggplot2::labs(
    title = "Latency Differences",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::scale_color_brewer(palette = "Set1") +
  ggplot2::scale_x_discrete(
    labels = feedback_labels,
    expand = ggplot2::expansion(mult = c(0.1, 0.1))
  ) +
  ggplot2::ylim(0, 6300) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text        = ggplot2::element_text(size = 14),
    axis.title.y     = ggplot2::element_blank(),
    axis.title.x     = ggplot2::element_blank(),
    legend.position  = "none",
    legend.title     = ggplot2::element_blank()
  )

# ---- DOT latency over the day in PD patients (RM plot) -----------------------

df_plotRM <- df_total %>%
  dplyr::select(Probanden_ID, group, DOT_latency_T1, DOT_latency_T2, DOT_latency_T3)

variables <- c("DOT_latency")

plot2D <- lapply(
  variables,
  function(var) create_subplot(variable_name = var, df = df_plotRM)
)

plot2D[[1]]$labels$title <- "DOT results for PD-patients"
plot2D[[1]]$labels$y     <- "Latency [ms]"

# Add extra right margin to plot2B to make room for annotation in the combined figure
plot2B <- plot2B + ggplot2::theme(plot.margin = ggplot2::margin(r = 120))

# ---- Combine plots into a multi-panel figure ---------------------------------

combined_plot2 <- (
  plot2A /
    ((plot2B + plot2C) + patchwork::plot_layout(widths = c(2, 1))) /
    plot2D
) +
  patchwork::plot_layout(heights = c(1, 0.7, 1)) +
  patchwork::plot_annotation(
    title      = "Combined Results for DOT Analysis",
    tag_levels = "A"
  ) &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 16))

ggplot2::ggsave(
  file.path(wdir, "results", "figure3.DOTresults.v1.0.png"),
  combined_plot2,
  width  = 12,
  height = 18,
  dpi    = 300
)

