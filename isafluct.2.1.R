#!/usr/bin/env Rscript

# =============================================================================
# ISAfluct Analysis Script
# =============================================================================
# This is code to analyse the ISAfluct data which was collected from 2020–2023
# Code developed by David Pedrosa
#
# Version 2.1  |  2026-02-11
# Added the subscores for the ISAm into the text.
# =============================================================================


#############################################################
## Preamble: setup working directory, load packages and helpers
#############################################################

username <- Sys.info()[["login"]]

if (username == "dpedr") {
  wdir <- "D:/ISAfluct/"
} else if (username == "david") {
  wdir <- "/media/storage/ISAfluct/"
}

setwd(wdir)

# Load packages (see packages.R for details)
source("packages.R")

# Load raw data and combine to single dataframe

# Read raw data from SPSS files and add group indicator
df_patients <- read_sav(file.path(wdir, "raw_data", "rohdaten_patientinnen_isafluct.sav")) %>%
  mutate(group = factor("patient"))

df_contr.subj <- read_sav(file.path(wdir, "raw_data", "rohdaten_kontrollprobanden.sav")) %>%
  mutate(group = factor("control"))

# Combined data set (patients + controls)
df_total <- bind_rows(df_patients, df_contr.subj)

# Run Helper scripts
source("extract_latencies.R")   # Functions to extract latencies
source("housekeeping.R")        # General helper functions / recoding

#############################################################
## Part 1: Table 1 – general characteristics (TableOne)
#############################################################
## - Prepare variables for TableOne
## - Recode factors and labels
## - Export Table 1 as CSV

source("TableOne.v1.0.R")

#############################################################
## Part 2: Normative data – controls only (Figure 1 / 2)
#############################################################
## - Create age bins based on quantiles
## - Plot normative distributions for DOT latency and clicks
## - Export Figure 2 and summary stats
source("NormativeDataDOT.v1.0.R")

# Descriptive normative summary for manuscript text
normative_data <- df_total_prepped %>%
  group_by(group) %>%
  summarise(
    mean_latency = mean(DOT_latency, na.rm = TRUE),
    sd_latency   = sd(DOT_latency, na.rm = TRUE),
    mean_score   = mean(DOT_Score, na.rm = TRUE),
    sd_score     = sd(DOT_Score, na.rm = TRUE),
    min_score    = min(DOT_Score, na.rm = TRUE),
    max_score    = max(DOT_Score, na.rm = TRUE),
    min_latency  = min(DOT_latency, na.rm = TRUE),
    max_latency  = max(DOT_latency, na.rm = TRUE),
    n            = n()
  )

# Paired t-test: pE_latency vs. npE_latency in controls
df_control <- df_total_prepped %>%
  filter(group == "control")

t_res <- t.test(
  df_control$DOT_pE_latency,
  df_control$DOT_npE_latency,
  paired = TRUE
)

print(t_res)

#############################################################
## Part 3: Repeated measures – Table 2 and Suppl. Figure 1
#############################################################
## - Long format for T1/T2/T3 variables
## - Repeated-measures ANOVA (afex)
## - Summary table and multi-panel figure

source("RMpatientData.v1.1.R")

#############################################################
## Part 4: DOT analyses – clicks and latencies (Figure 3)
#############################################################
## - DOT clicks over the day (patients vs controls)
## - Overall latency difference between groups
## - Feedback-dependent latencies and time-of-day effects

source("DOTanalyses.v1.0.R")


#############################################################
## Part 5: Correlation analyses – clinical and behavioural
#############################################################
## - Kendall tau correlations among clinical & behavioural measures
## - Benjamini–Hochberg correction
## - Correlation heatmap with ggcorrplot (Figure 4)

source("CorrelationPatients.v1.0.R")


#############################################################
## Part 6: Stacked plots – predictors vs. latency (Figure 5)
#############################################################
## - Scatter plots (predictor vs DOT latency) for punished vs rewarded
## - Overlaid regression lines
## - Panel-wise r / significance annotations

variables <- c("PANDA_gesamt", "ISAm_LID_total", "LEDD", "DAA")

variable_labels <- c(
  "PANDA_gesamt" = "PANDA total score",
  #"BIS_gesamt"   = "Barrett Impulsivity Scale-11",
  "LEDD"         = "LEDD [mg]",
  "DAA"          = "Dopamine agonist dose (DAA)",
  "ISAm_LID_total" = "ISAm Scores related to LID"
)

df_long <- df_total %>%
  filter(group == "patient") %>%
  pivot_longer(
    cols      = c(DOT_pE_latency, DOT_npE_latency),
    names_to  = "Latency_Type",
    values_to = "Latency"
  ) %>%
  mutate(
    Latency_Type = recode(
      Latency_Type,
      DOT_pE_latency  = "punished",
      DOT_npE_latency = "rewarded"
    )
  )

# Helper: create small-multiple scatter plots (Fig. 5 panels)
generate_combined_plots <- function(df_long, variables) {

  # Manuscript-consistent blue palette:
  blues <- c(
    "punished" = "#1f78b4",  # dark blue
    "rewarded" = "#a6cee3"   # light blue
  )

  plots <- list()

  for (i in seq_along(variables)) {
    var <- variables[i]

    # Prepare complete cases
    df_filtered <- df_long %>%
      select(all_of(c(var, "Latency", "Latency_Type"))) %>%
      filter(!is.na(.data[[var]]), !is.na(Latency))

    # Correlations by latency type
    cor_results <- df_filtered %>%
      group_by(Latency_Type) %>%
      summarise(
        cor_test = list(cor.test(.data[[var]], Latency)),
        .groups  = "drop"
      ) %>%
      mutate(
        r_value = map_dbl(cor_test, ~ round(.x$estimate, 2)),
        p_value = map_dbl(cor_test, ~ .x$p.value),
        significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01  ~ "**",
          p_value < 0.05  ~ "*",
          TRUE            ~ "ns"
        ),
        label = paste0(Latency_Type, ": r=", r_value, " ", significance)
      )

    # Only last panel shows legend
    is_last_panel <- var == tail(variables, 1)
    legend_theme  <- if (is_last_panel) {
      theme(
        legend.position      = c(1.13, 0.97),
        legend.justification = c(1, 1),
        legend.background    = element_rect(fill = alpha("white", 0.8)),
        legend.title         = element_blank(),
        legend.text          = element_text(size = 24)
      )
    } else {
      theme(legend.position = "none")
    }

    # Right column drops y-axis label
    is_right_column    <- i %% 2 == 0
    y_axis_title_theme <- if (is_right_column) theme(axis.title.y = element_blank()) else theme()

    # Annotation vertical placement
    y_mid <- max(df_filtered$Latency, na.rm = TRUE) * 0.55

    # Plot build
    p <- ggplot(
      df_filtered,
      aes_string(
        x     = var,
        y     = "Latency",
        color = "Latency_Type",
        shape = "Latency_Type"
      )
    ) +
      # Points
      geom_point(alpha = 0.4, size = 6) +
      # Regression lines
      geom_smooth(
        aes(linetype = Latency_Type),
        method = "lm", se = FALSE, size = 1.3
      ) +
      # Blues palette
      scale_color_manual(values = blues) +
      scale_shape_manual(values = c("punished" = 19, "rewarded" = 17)) +
      scale_linetype_manual(values = c("punished" = "solid", "rewarded" = "dashed")) +
      labs(
        title = variable_labels[[var]],
        x     = variable_labels[[var]],
        y     = "Latency [ms]"
      ) +
      theme_minimal(base_size = 18) +
      theme(
        plot.title   = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title   = element_text(size = 20),
        axis.text    = element_text(size = 18),
        plot.margin  = margin(15, 25, 15, 25)
      ) +
      legend_theme +
      y_axis_title_theme +
      # Annotation box with bigger text (r, significance)
      annotate(
        "label",
        x         = Inf,
        y         = y_mid,
        label     = paste(cor_results$label, collapse = "\n"),
        hjust     = 1.1,
        vjust     = 0.5,
        size      = 9,
        label.size = 0.4,
        label.r   = unit(0.2, "lines"),
        fill      = alpha("white", 0.9),
        color     = "black"
      )

    plots[[var]] <- p
  }

  plots
}

combined_plots <- generate_combined_plots(df_long, variables)

plot4_combined <- wrap_plots(combined_plots, ncol = 2) +
  plot_layout(
    widths  = c(1, 1),
    heights = c(1, 1)
  ) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag    = element_text(face = "bold", size = 14),
    plot.margin = margin(40, 40, 40, 40),
    plot.spacing = unit(2, "lines")
  )

print(plot4_combined)

output_path <- file.path(wdir, "results", "figure5.latencies_correlated.v1.0.png")

ggsave(
  filename = output_path,
  plot     = plot4_combined,
  device   = "png",
  width    = 20,
  height   = 15,
  dpi      = 300
)

