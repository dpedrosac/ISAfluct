#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  NormativeData.v1.0.R
# Purpose:      Generate normative (control-only) reference distributions for
#               DOT latency and click metrics. Exports Figures and in the 
#		manuscript.
#
# Author(s):    David Pedrosa
#
# Notes:
# - Outputs: figures and tables written to <results_dir>/ (see paths below)
# - Run from repo root: ./NormativeData.v1.0.R
# =============================================================================

# ---- Age quantiles (equal-sized age bins) ------------------------------------

qs <- quantile(
  df_total$Alter,
  probs = seq(0, 1, length.out = 5),
  na.rm = TRUE
)

qs_round <- round(qs)

# ---- Prepare data: age bins + gender -----------------------------------------

df_total_prepped <- df_total %>%
  mutate(
    age_bin = cut(
      Alter,
      breaks = qs_round,
      include.lowest = TRUE,
      right = FALSE
    ),
    gender = dplyr::recode(
      Geschlecht,
      `0` = "female",
      `1` = "male"
    )
  ) %>%
  filter(!is.na(age_bin))

# Nicer labels for age-bin factors (e.g., "[49,65)" -> "49–65")
df_total_prepped <- df_total_prepped %>%
  mutate(
    age_bin = forcats::fct_relabel(age_bin, function(x) {
      x <- gsub("\\[|\\(|\\)", "", x)
      x <- gsub(",", "–", x)
      x
    })
  )

# TODO: Replace this manual relabeling with labels derived from qs_round
levels(df_total_prepped$age_bin) <- c("49–65", "65–71", "71–79", "79–94")

# ---- Plot helper: normative boxplots -----------------------------------------
# data: data frame with columns age_bin, gender, and the outcome variable
# var:  bare variable name (e.g., DOT_latency)

plot_norm_box <- function(data, var) {
  v <- rlang::ensym(var)
  ylab <- rlang::as_name(v)

  # Global stats (±2 SD reference lines across the full sample)
  stats <- data %>%
    summarise(
      mu = mean(!!v, na.rm = TRUE),
      sd = sd(!!v, na.rm = TRUE)
    )

  # n per age bin (annotation above boxes)
  n_labels <- data %>%
    group_by(age_bin) %>%
    summarise(
      n = n(),
      y = max(!!v, na.rm = TRUE),
      .groups = "drop"
    )

  label_y <- max(data[[ylab]], na.rm = TRUE) * 1.05

  ggplot(data, aes(x = age_bin, y = !!v, fill = gender)) +
    geom_boxplot(
      outlier.shape = 16,
      width = 0.7,
      position = position_dodge(width = 0.8),
      alpha = 0.5
    ) +
    geom_hline(
      yintercept = stats$mu + 2 * stats$sd,
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = stats$mu - 2 * stats$sd,
      linetype = "dashed"
    ) +
    geom_text(
      data = n_labels,
      inherit.aes = FALSE,
      aes(x = age_bin, y = label_y, label = paste0("n = ", n)),
      vjust = -0.5,
      size = 5
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      x = "Age bin",
      y = ylab,
      fill = "Gender"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom"
      # panel.grid.minor = element_blank()
    )
}

# ---- Controls only: create the two plots -------------------------------------

controls <- df_total_prepped %>%
  filter(group == "control")

p_latency <- plot_norm_box(controls, DOT_latency) +
  labs(
    title = "DOT latency",
    y = "Latency (ms)"
  )

p_score <- plot_norm_box(controls, DOT_Klicks) +
  labs(
    title = "Opened doors before quitting",
    y = "Count (n)"
  )

# ---- Combine with patchwork ---------------------------------------------------

combined_plot <- (p_latency | p_score) +
  plot_layout(
    guides = "collect",
    axis_titles = "collect"
  ) +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 16)
  )

print(combined_plot)

# ---- Export ------------------------------------------------------------------

output_path <- file.path(wdir, "results", "figure2.normativeData.v1.0.png")

ggsave(
  filename = output_path,
  plot = combined_plot,
  device = "png",
  width = 18,
  height = 10,
  dpi = 300
)

