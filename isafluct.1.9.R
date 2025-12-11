#!/usr/bin/env Rscript

# =============================================================================
# ISAfluct Analysis Script
# =============================================================================
# This is code to analyse the ISAfluct data which was collected from 2020–2023
# Code developed by David Pedrosa
#
# Version 1.9  |  2025-12-11
# Corrected some bugs, changed the way Fig. 5 is displayed/rendered.
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

# Read raw data from SPSS files and add group indicator
df_patients <- read_sav(file.path(wdir, "raw_data", "rohdaten_patientinnen_isafluct.sav")) %>%
  mutate(group = factor("patient"))

df_contr.subj <- read_sav(file.path(wdir, "raw_data", "rohdaten_kontrollprobanden.sav")) %>%
  mutate(group = factor("control"))

# Combined data set (patients + controls)
df_total <- bind_rows(df_patients, df_contr.subj)

# Helper scripts
source("extract_latencies.R")   # Functions to extract latencies
source("housekeeping.R")        # General helper functions / recoding


#############################################################
## Part 1: Table 1 – general characteristics (TableOne)
#############################################################
## - Prepare variables for TableOne
## - Recode factors and labels
## - Export Table 1 as CSV

# Work on a copy to keep original df_total intact
df_tableone <- df_total

# Convert selected columns to factors
vars.factors <- c("Geschlecht", "Schulabschluss")
df_tableone[vars.factors] <- lapply(
  df_tableone[vars.factors],
  function(x) factor(as.character(x))
)

# Select relevant columns for analysis / Table 1
vars.proportional <- c(
  "Geschlecht", "Schulabschluss", "Alter", "Krankheitsdauer_Jahre", "BDI",
  "AES", "MSS", "MDS_UPDRS_I", "MDS_UPDRS_II", "MDS_UPDRS_III_Mittelwert",
  "MDS_UPDRS_IV", "Tagesäqiuvalenzdosis_L_DOPA_mg", "PANDA_gesamt", "MMST",
  "MCI", "group"
)

df_tableone <- df_tableone %>%
  dplyr::select(all_of(vars.proportional))

# Rename columns (English labels + abbreviations)
colnames_Vars <- c(
  "Gender", "Education", "Age", "Disease duration", "BDI-II*", "AES†",
  "MSS‡", "MDS-UPDRS part I", "MDS-UPDRS part II", "MDS-UPDRS part III",
  "MDS-UPDRS part IV", "LEDD§ [in mg]", "PANDA¶", "MMSE**", "MCI", "group"
)

colnames(df_tableone) <- colnames_Vars

# Recode group and categorical variables
df_tableone <- df_tableone %>%
  mutate(
    group = recode(
      group,
      "patient" = "PD patients",
      "control" = "Control subjects"
    ),
    Gender = recode(
      Gender,
      "0" = "Female",
      "1" = "Male"
    ),
    Education = recode(
      Education,
      "1" = "primary school",
      "2" = "lower secondary school degree",
      "3" = "intermediate secondary school",
      "4" = "High school diploma or university entrance qualification"
    )
  )

# Variables to include in TableOne object
# (Education removed from the final table)
vars.proportional <- c(
  "Gender", "Age", "Disease duration", "BDI-II*", "AES†",  # "Education" removed
  "MSS‡", "MDS-UPDRS part I", "MDS-UPDRS part II", "MDS-UPDRS part III",
  "MDS-UPDRS part IV", "LEDD§ [in mg]", "PANDA¶", "MMSE**", "MCI"
)

factVars <- c("Gender", "MCI")  # ,"Education" could be added here if used

# Create the TableOne object
tableOne <- CreateTableOne(
  vars       = vars.proportional,
  strata     = "group",
  factorVars = factVars,
  data       = df_tableone
)

# Convert TableOne to data frame and keep row names as a column
tableOne_df <- as.data.frame(
  print(
    tableOne,
    nonnormal     = c("MCI"),   # e.g. binary variable
    showAllLevels = FALSE       # single category for Gender
  )
)

tableOne_df <- tibble::rownames_to_column(tableOne_df, var = "Variable")

# Row with footnote-style abbreviation explanations
blank_row <- data.frame(
  "Variable"         = "Abbreviations: * Beck's Depression Inventory-II, † Apathy evaluation scale, ‡ ??, § Levodopa-equivalent dosage, ¶ Parkinson Neuropsychometric Dementia Assessment, ** Mini-Mental State Examination",
  "PD patients"      = NA,
  "Control subjects" = NA,
  "p"                = NA,
  "test"             = NA
)

colnames(blank_row) <- colnames(tableOne_df)

# Append the annotations and write to csv
tableOne_df <- rbind(tableOne_df, blank_row)

write.csv(
  tableOne_df,
  file      = file.path(wdir, "results", "table1.general_results.v1.1.csv"),
  row.names = FALSE
)


#############################################################
## Part 2: Normative data – controls only (Figure 1 / 2)
#############################################################
## - Create age bins based on quantiles
## - Plot normative distributions for DOT latency and clicks
## - Export Figure 2 and summary stats

# Quantiles on age to define equally sized age bins
qs <- quantile(
  df_total$Alter,
  probs = seq(0, 1, length.out = 5),
  na.rm = TRUE
)

qs_round <- round(qs)

# Assign ages to age bins and recode gender
df_total_prepped <- df_total %>%
  mutate(
    age_bin = cut(
      Alter,
      breaks         = qs_round,
      include.lowest = TRUE,
      right          = FALSE
    ),
    Gender = dplyr::recode(
      Geschlecht,
      `0` = "female",
      `1` = "male"
    )
  ) %>%
  filter(!is.na(age_bin))

# Rename factor levels of age bins for nicer labels in plots
df_total_prepped <- df_total_prepped %>%
  mutate(
    age_bin = forcats::fct_relabel(age_bin, function(x) {
      x <- gsub("\\[|\\(|\\)", "", x)
      x <- gsub(",", "–", x)
      x
    })
  )

# TODO: consider replacing this “dirty hack” with labels based on qs_round
levels(df_total_prepped$age_bin) <- c("49–65", "65–71", "71–79", "79–94")


# Reusable plotting function for normative boxplots
# data: data frame with columns age_bin, Gender and the outcome
# var:  bare variable name (e.g., DOT_latency)
plot_norm_box <- function(data, var) {

  v    <- rlang::ensym(var)
  ylab <- rlang::as_name(v)

  # Global stats (for ±2 SD reference lines across whole sample)
  stats <- data %>%
    summarise(
      mu = mean(!!v, na.rm = TRUE),
      sd = sd(!!v, na.rm = TRUE),
      .groups = "drop"
    )

  # n per age bin (for annotation above boxes)
  n_labels <- data %>%
    group_by(age_bin) %>%
    summarise(
      n = n(),
      y = max(!!v, na.rm = TRUE),
      .groups = "drop"
    )

  label_y <- max(data[[ylab]], na.rm = TRUE) * 1.05

  ggplot(data, aes(x = age_bin, y = !!v, fill = Gender)) +
    geom_boxplot(
      outlier.shape = 16,
      width         = 0.7,
      position      = position_dodge(width = 0.8),
      alpha         = 0.5
    ) +
    geom_hline(yintercept = stats$mu + 2 * stats$sd, linetype = "dashed") +
    geom_hline(yintercept = stats$mu - 2 * stats$sd, linetype = "dashed") +
    geom_text(
      data        = n_labels,
      inherit.aes = FALSE,
      aes(x = age_bin, y = label_y, label = paste0("n = ", n)),
      vjust       = -0.5,
      size        = 5
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
      x    = "Age bin",
      y    = ylab,
      fill = "Gender"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom"
      # panel.grid.minor = element_blank()
    )
}

# Apply normative plotting function to controls only
p_latency <- plot_norm_box(
  df_total_prepped %>% filter(group == "control"),
  DOT_latency
)

p_score <- plot_norm_box(
  df_total_prepped %>% filter(group == "control"),
  DOT_Klicks
)

# Refine labels and titles for manuscript-ready figs
p_latency <- p_latency +
  labs(
    y     = "Latency (ms)",
    title = "DOT latency"
  )

p_score <- p_score +
  labs(
    y     = "Count (n)",
    title = "Opened doors before quitting"
  )

# Combine both normative plots with patchwork
combined_plot <- (p_latency + p_score) +
  plot_layout(
    guides      = "collect",   # one shared legend
    axis_titles = "collect"    # share axis titles where possible
  ) +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag        = element_text(face = "bold", size = 16)
  )

print(combined_plot)

output_path <- file.path(wdir, "results", "figure2.normativeData.v1.0.png")

ggsave(
  filename = output_path,
  plot     = combined_plot,
  device   = "png",
  width    = 18,
  height   = 10,
  dpi      = 300
)

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

variables_repeated <- c(
  "MDS_UPDRS_III", "ISA_m", "BIS",
  "DOT_Klicks", "DOT_latency", "Schmerz_VAS_Rang"
)

# Convert wide to long: Variable_T1/T2/T3 -> Variable + Time
df_longRM <- df_total %>%
  pivot_longer(
    cols         = matches(paste0("^(", paste(variables_repeated, collapse = "|"), ")_T[1-3]$")),
    names_to     = c("Variable", "Time"),
    names_pattern = "([a-zA-Z0-9_]+)_T([1-3])",
    values_to    = "Value"
  ) %>%
  mutate(Time = as.factor(Time))

# Drop haven labels to avoid type issues in afex
df_longRM <- df_longRM %>%
  mutate(across(where(is.labelled), as.numeric))

df_longRM <- df_longRM %>%
  mutate(
    group = as_factor(group),
    Time  = as_factor(Time)
  )

# Summary: mean ± SD per variable and time point
summary_statsRM <- df_longRM %>%
  group_by(Variable, Time) %>%
  summarise(
    Mean   = mean(Value, na.rm = TRUE),
    SD     = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_SD = paste0(round(Mean, 2), " ± ", round(SD, 2))
  )

# Repeated measures ANOVA using afex (patients only)
anova_results_afex <- df_longRM %>%
  filter(group == "patient") %>%
  group_by(Variable) %>%
  summarise(
    anova   = list(aov_car(Value ~ Time + Error(subj_ID / Time), data = cur_data())),
    .groups = "drop"
  )

# Extract F and p from afex object
anova_summaries <- anova_results_afex %>%
  mutate(
    F_value = map_dbl(anova, ~ .x$anova_table$F[1]),
    P_value = map_dbl(anova, ~ .x$anova_table$Pr[1]),
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(Variable, F_value, P_value, Significance)

# Reshape summary into wide-format table
summary_tableRM <- summary_statsRM %>%
  select(Variable, Time, Mean_SD) %>%
  pivot_wider(
    names_from   = Time,
    values_from  = Mean_SD,
    names_prefix = "T"
  ) %>%
  left_join(anova_summaries, by = "Variable")

# Rename variables for human-readable table labels
rename_map <- c(
  "MDS_UPDRS_III"    = "MDS-UPDRS part III",
  "ISA_m"            = "ISA motor score",
  "BIS"              = "Barrett Impulsivity Scale-11",
  "DOT_Klicks"       = "Door Opening Task, Clicks",
  "DOT_latency"      = "Door Opening Task, Latency",
  "Schmerz_VAS_Rang" = "Visual Analog Scale for Discomfort"
)

summary_tableRM <- summary_tableRM %>%
  mutate(Variable = recode(Variable, !!!rename_map))

colnames(summary_tableRM) <- c(
  "Item", "morning", "noon", "evening",
  "F-value", "p", "sig."
)

print(summary_tableRM)

# Custom ordering of rows in table
custom_order <- c(
  "MDS-UPDRS part III",
  "Barrett Impulsivity Scale-11",
  "ISA motor score",
  "Door Opening Task, Clicks",
  "Door Opening Task, Latency",
  "Visual Analog Scale for Discomfort"
)

summary_tableRM <- summary_tableRM %>%
  mutate(Item = factor(Item, levels = custom_order)) %>%
  arrange(Item)

# Save repeated measures table as CSV
write.csv(
  summary_tableRM,
  file      = file.path(wdir, "results", "table2.repeated_measures.v1.0.csv"),
  row.names = FALSE
)


# Repeated measures plotting function
# create_subplot():
# - Reshapes one variable from wide to long
# - Runs within-subject ANOVA
# - Plots boxplots + jitter per time point
create_subplot <- function(variable_name,
                           df,
                           title        = NULL,
                           group_colors = c("Patient" = "blue", "Control" = "orange")) {

  # Long format with ID, group, and T1/T2/T3 columns for the variable
  df_long <- df %>%
    select(Probanden_ID, group, starts_with(variable_name)) %>%
    pivot_longer(
      cols      = starts_with(paste0(variable_name, "_T")),
      names_to  = "Time",
      values_to = "Score"
    ) %>%
    mutate(
      Time = case_when(
        .data$Time == paste0(variable_name, "_T1") ~ "T1",
        .data$Time == paste0(variable_name, "_T2") ~ "T2",
        .data$Time == paste0(variable_name, "_T3") ~ "T3"
      ),
      group = factor(group),
      Time  = factor(Time, levels = c("T1", "T2", "T3"))
    )

  # Filter out missing IDs
  df_filtered <- df_long %>%
    filter(!is.na(Probanden_ID))

  # Two-way mixed ANOVA (within: Time; between: group)
  anova_results <- aov(
    Score ~ Time + Error(Probanden_ID / Time),
    data = df_filtered
  )

  print(summary(anova_results))  # optional: keep for console diagnostics

  # Summary for plotting: mean ± SEM
  df_summary <- df_long %>%
    group_by(group, Time) %>%
    summarise(
      Mean   = mean(Score, na.rm = TRUE),
      SEM    = sd(Score, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # y-position for annotation
  y_annot <- max(df_long$Score, na.rm = TRUE) * 1.1

  plot <- ggplot() +
    # Boxplot per group and time point
    geom_boxplot(
      data    = df_long,
      aes(x = Time, y = Score, fill = group),
      width   = 0.5,
      outlier.shape = NA,
      alpha   = 0.5
    ) +
    # Jittered individual data
    geom_jitter(
      data = df_long,
      aes(x = Time, y = Score),
      color   = "black",
      position = position_jitterdodge(
        jitter.width = 0.15,
        dodge.width  = 1
      ),
      size = 2,
      alpha = 0.2
    ) +
    # Simple annotation example (currently generic; can be replaced with real F/p)
    annotate(
      "text",
      x     = 1.5,
      y     = y_annot,
      label = sprintf("F = %.2f\n%s", anova_results[[1]]$`F value`[1], "p < 0.05"),
      size  = 5
    ) +
    labs(
      title = ifelse(is.null(title), paste(variable_name, "Scores"), title),
      x     = "Time Point",
      y     = "Score"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1") +
    scale_x_discrete(
      labels = c(
        "T1" = "morning",
        "T2" = "noon",
        "T3" = "evening"
      ),
      expand = expansion(mult = c(0.5, 0.5))
    ) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 18, face = "bold"),
      axis.text    = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_blank(),
      legend.position = "none"
    )

  return(plot)
}

# Assemble repeated measures plots for ISA_m, BIS, MDS_UPDRS_III
df_plotRM <- df_total %>%
  select(
    Probanden_ID, group,
    ISA_m_T1, ISA_m_T2, ISA_m_T3,
    BIS_T1, BIS_T2, BIS_T3,
    MDS_UPDRS_III_T1, MDS_UPDRS_III_T2, MDS_UPDRS_III_T3
  )

variables <- c("ISA_m", "BIS", "MDS_UPDRS_III")

subplots <- lapply(
  variables,
  function(var) create_subplot(variable_name = var, df = df_plotRM)
)

# Adjust titles for clarity
subplots[[1]]$labels$title <- "Motor ISA"
subplots[[2]]$labels$title <- "Barrett Impulsiveness Scale-11"
subplots[[3]]$labels$title <- "MDS-UPDRS, part III"

combined_plot <- subplots[[1]] /
  subplots[[2]] /
  subplots[[3]] +
  plot_layout(axis_titles = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 16))

print(combined_plot)

output_path <- file.path(wdir, "results", "suppl.figure1.rmResults.v1.0.png")

ggsave(
  filename = output_path,
  plot     = combined_plot,
  device   = "png",
  width    = 10,
  height   = 18,
  dpi      = 300
)


#############################################################
## Part 4: DOT analyses – clicks and latencies (Figure 3)
#############################################################
## - DOT clicks over the day (patients vs controls)
## - Overall latency difference between groups
## - Feedback-dependent latencies and time-of-day effects

# Reshape DOT_Klicks_T1..T3 to long
df_longISAb <- df_total %>%
  pivot_longer(
    cols         = starts_with("DOT_Klicks"),
    names_to     = c("Score_Type", "Time"),
    names_pattern = "(DOT_Klicks)_(T\\d+)",
    values_to    = "DOT_Klicks"
  )

# Plot 2A: DOT clicks across time of day and groups
plot2A <- ggplot(
  df_longISAb %>% filter(!is.na(Time)),
  aes(x = Time, y = DOT_Klicks, fill = group)
) +
  geom_boxplot(
    outlier.shape = NA,
    alpha         = 0.5,
    position      = position_dodge(1.1),
    lwd           = 0.6,
    fatten        = 0
  ) +
  stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = position_dodge(1.1),
    width    = 0.8,
    size     = 1
  ) +
  geom_jitter(
    aes(group = group),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 1.1
    ),
    size  = 2,
    alpha = 0.5
  ) +
  labs(
    title = "DOT results over the day",
    x     = NULL,
    y     = "Opened doors in \nDoor-Opening-Task (DOT)*"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(
    labels = c(
      "T1" = "morning",
      "T2" = "noon",
      "T3" = "evening"
    ),
    expand = expansion(mult = c(0.5, 0.5))
  ) +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text    = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_blank()
  )

# Plot 2B: Latency difference between groups
t_test_result <- t.test(DOT_latency ~ group, data = df_total)

t_stat       <- round(t_test_result$statistic, 2)
p_value      <- t_test_result$p.value
p_value_text <- ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.3f", p_value))

plot2B <- ggplot(
  df_total,
  aes(x = group, y = DOT_latency, fill = group)
) +
  geom_boxplot(
    outlier.shape = NA,
    alpha         = 0.5,
    lwd           = 0.6,
    fatten        = 0
  ) +
  stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = position_dodge(1.1),
    width    = 0.8,
    size     = 1
  ) +
  geom_jitter(
    position = position_jitter(width = 0.2),
    size     = 2,
    alpha    = 0.5
  ) +
  annotate(
    "segment",
    x    = 1,
    xend = 2,
    y    = max(df_total$DOT_latency, na.rm = TRUE) * 1.05,
    yend = max(df_total$DOT_latency, na.rm = TRUE) * 1.05,
    size = 0.8
  ) +
  annotate(
    "text",
    x     = 1.5,
    y     = max(df_total$DOT_latency, na.rm = TRUE) * 1.1,
    label = sprintf("t = %.2f, %s", t_stat, p_value_text),
    size  = 5
  ) +
  labs(
    title = "Latency for DOT responses",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text    = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Latency: effect of feedback (positive/negative answer)
df_long <- df_total %>%
  select(group, DOT_npE_latency, DOT_pE_latency) %>%
  pivot_longer(
    cols      = starts_with("DOT_"),
    names_to  = "Latency_Type",
    values_to = "Latency"
  ) %>%
  mutate(
    Latency_Type = factor(
      Latency_Type,
      levels = c("DOT_npE_latency", "DOT_pE_latency")
    )
  )

anova_result <- aov(
  Latency ~ Latency_Type * group,
  data = df_long
)

summary(anova_result)

# t-tests per Latency_Type (group differences)
t_test_results <- df_long %>%
  group_by(Latency_Type) %>%
  summarise(
    t_stat  = round(t.test(Latency ~ group)$statistic, 2),
    p_value = t.test(Latency ~ group)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_value_text = ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.3f", p_value))
  )

# Interaction term for ANOVA
anova_result    <- aov(Latency ~ as.factor(Latency_Type) * group, data = df_long)
anova_summary   <- summary(anova_result)
interaction_f   <- round(anova_summary[[1]]$`F value`[3], 2)
interaction_p   <- anova_summary[[1]]$`Pr(>F)`[3]
interaction_txt <- ifelse(interaction_p < 0.001, "p < 0.001", sprintf("p = %.3f", interaction_p))

overall_y_min <- min(df_long$Latency, na.rm = TRUE)

# OLD version: boxplot with multiple annotations (kept for reference)
plot2CboxplotOLD <- ggplot(
  df_long,
  aes(x = Latency_Type, y = Latency, fill = group)
) +
  geom_boxplot(
    outlier.shape = NA,
    alpha         = 0.5,
    position      = position_dodge(1.1),
    lwd           = 0.6,
    fatten        = 0
  ) +
  stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = position_dodge(1.1),
    width    = 0.8,
    size     = 1
  ) +
  geom_jitter(
    aes(group = interaction(Latency_Type, group)),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 0.8
    ),
    size  = 2,
    alpha = 0.5
  ) +
  geom_segment(
    aes(
      x    = 0.75,
      xend = 1.25,
      y    = max(df_long$Latency, na.rm = TRUE) * 1.05,
      yend = max(df_long$Latency, na.rm = TRUE) * 1.05
    ),
    size  = 0.8,
    color = "black"
  ) +
  geom_segment(
    aes(
      x    = 1.75,
      xend = 2.25,
      y    = max(df_long$Latency, na.rm = TRUE) * 1.05,
      yend = max(df_long$Latency, na.rm = TRUE) * 1.05
    ),
    size  = 0.8,
    color = "black"
  ) +
  geom_text(
    data = t_test_results,
    aes(
      x     = Latency_Type,
      y     = max(df_long$Latency, na.rm = TRUE) * 1.1,
      label = paste0("t = ", t_stat, "\n", p_value_text)
    ),
    inherit.aes = FALSE,
    size        = 4,
    position    = position_dodge(width = 0.8)
  ) +
  annotate(
    "text",
    x     = 1.5,
    y     = overall_y_min * 0.9,
    label = paste0("Interaction: F = ", interaction_f, ", ", interaction_txt),
    size  = 5,
    hjust = 0
  ) +
  labs(
    title = "Latency Differences",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(
    labels = c(
      "DOT_npE_latency" = "After positive answer",
      "DOT_pE_latency"  = "After negative answer"
    ),
    expand = expansion(mult = c(0.5, 0.5))
  ) +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text    = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

# Newer visualization: line plot of means + SEM
df_summary <- df_long %>%
  group_by(group, Latency_Type) %>%
  summarise(
    mean_latency = mean(Latency, na.rm = TRUE),
    sem_latency  = sd(Latency, na.rm = TRUE) / sqrt(n()),
    .groups      = "drop"
  )

plot2C <- ggplot() +
  geom_line(
    data  = df_summary,
    aes(x = Latency_Type, y = mean_latency, group = group, color = group),
    size  = 1,
    alpha = 0.5
  ) +
  geom_point(
    data  = df_summary,
    aes(x = Latency_Type, y = mean_latency, color = group),
    size  = 3,
    alpha = 0.5
  ) +
  geom_errorbar(
    data  = df_summary,
    aes(
      x    = Latency_Type,
      ymin = mean_latency - sem_latency,
      ymax = mean_latency + sem_latency
    ),
    width = 0.1,
    size  = 0.5,
    alpha = 0.5
  ) +
  labs(
    title = "Latency Differences",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(
    labels = c(
      "DOT_npE_latency" = "After positive\n answer",
      "DOT_pE_latency"  = "After negative\n answer"
    ),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  ylim(0, 6300) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text     = element_text(size = 14),
    axis.title.y  = element_blank(),
    axis.title.x  = element_blank(),
    legend.position = "none",
    legend.title    = element_blank()
  )

# Alternative: boxplot + jitter for feedback types
plot2C_box <- ggplot(
  data = df_long,
  aes(x = Latency_Type, y = Latency, fill = group)
) +
  geom_jitter(
    aes(size = 2, alpha = 0.5),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 0.7
    ),
    size  = 1.8,
    alpha = 0.5
  ) +
  geom_boxplot(
    width    = 0.6,
    position = position_dodge(width = 0.7),
    outlier.shape = NA,
    alpha   = 0.5
  ) +
  stat_summary(
    fun      = median,
    geom     = "crossbar",
    position = position_dodge(0.7),
    width    = 0.8,
    size     = 1
  ) +
  labs(
    title = "Latency Differences",
    x     = NULL,
    y     = "Latency [ms]"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(
    labels = c(
      "DOT_npE_latency" = "After positive\n answer",
      "DOT_pE_latency"  = "After negative\n answer"
    ),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  ylim(0, 6300) +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text    = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title    = element_blank()
  )

# DOT latency over the day in PD patients (RM plot)
df_plotRM <- df_total %>%
  select(Probanden_ID, group, DOT_latency_T1, DOT_latency_T2, DOT_latency_T3)

variables <- c("DOT_latency")

plot2D <- lapply(
  variables,
  function(var) create_subplot(variable_name = var, df = df_plotRM)
)

plot2D[[1]]$labels$title <- "DOT results for PD-patients"
plot2D[[1]]$labels$y     <- "Latency [ms]"

# Add extra right margin to plot2B to make room for annotation in the combined figure
plot2B <- plot2B +
  theme(plot.margin = margin(r = 120))

# Combine 2A, (2B + 2C), and 2D into a multi-panel figure
combined_plot2 <- (
  plot2A /
    ((plot2B + plot2C) + plot_layout(widths = c(2, 1))) /
    plot2D
) +
  plot_layout(heights = c(1, 0.7, 1)) +
  plot_annotation(
    title      = "Combined Results for DOT Analysis",
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold", size = 16)
  )

ggsave(
  file.path(wdir, "results", "figure3.DOTresults.v1.0.png"),
  combined_plot2,
  width  = 12,
  height = 18,
  dpi    = 300
)


#############################################################
## Part 5: Correlation analyses – clinical and behavioural
#############################################################
## - Kendall tau correlations among clinical & behavioural measures
## - Benjamini–Hochberg correction
## - Correlation heatmap with ggcorrplot (Figure 4)

variables_for_corr <- c(
  "Alter",
  "Krankheitsdauer_Jahre",
  "BDI",
  "BIS_gesamt",
  "PANDA_gesamt",
  "PD_NMS",
  "MSS",
  "AES",
  "MDS_UPDRS_III_Mittelwert",
  "MDS_UPDRS_I",
  "MDS_UPDRS_II",
  "MDS_UPDRS_IV",
  "ISAm_total",
  "Tagesäqiuvalenzdosis_L_DOPA_mg",
  "subj_ID",
  "DOT_Klicks",
  "DAA",
  "DOT_latency"
)

df_longISAb2 <- df_total %>%
  filter(group == "patient") %>%
  dplyr::select(all_of(variables_for_corr))

variable_vector <- c(
  "Age",
  "Disease duration [years]",
  "BDI-II",
  "Barrett Impulsivity Scale",
  "PANDA",
  "Non-motor symptoms",
  "MSS",
  "AES",
  "MDS-UPDRS part III",
  "MDS-UPDRS part I",
  "MDS-UPDRS part II",
  "MDS-UPDRS part IV",
  "ISAm Score",
  "LEDD",
  "subj_ID",
  "DOT, Klicks",
  "DAA",
  "DOT, Latency"
)

colnames(df_longISAb2) <- variable_vector

# Keep only numeric columns for correlations
df_numeric <- df_longISAb2 %>%
  select(where(is.numeric))

# Helper: compute Kendall correlations + p-values
compute_correlation_with_pvalues <- function(df) {
  n           <- ncol(df)
  corr_matrix <- matrix(NA, n, n)
  pval_matrix <- matrix(NA, n, n)

  for (i in seq_len(n)) {
    for (j in i:n) {
      if (i == j) {
        corr_matrix[i, j] <- 1
        pval_matrix[i, j] <- NA
      } else {
        test <- cor.test(
          df[[i]],
          df[[j]],
          use    = "complete.obs",
          method = "kendall",
          exact  = FALSE
        )
        corr_matrix[i, j] <- test$estimate
        corr_matrix[j, i] <- test$estimate
        pval_matrix[i, j] <- test$p.value
        pval_matrix[j, i] <- test$p.value
      }
    }
  }

  colnames(corr_matrix) <- colnames(df)
  rownames(corr_matrix) <- colnames(df)
  colnames(pval_matrix) <- colnames(df)
  rownames(pval_matrix) <- colnames(df)

  list(
    correlations = corr_matrix,
    pvalues      = pval_matrix
  )
}

cor_results <- compute_correlation_with_pvalues(df_numeric)

correlations <- cor_results$correlations
pvalues      <- cor_results$pvalues

# Flatten matrices into long format
corr_long <- as.data.frame(as.table(correlations))
pval_long <- as.data.frame(as.table(pvalues))

cor_pval_long <- left_join(
  corr_long,
  pval_long,
  by = c("Var1", "Var2")
)

colnames(cor_pval_long) <- c("Var1", "Var2", "Correlation", "PValue")

# Adjust p-values (Benjamini–Hochberg)
cor_pval_long <- cor_pval_long %>%
  mutate(AdjPValue = p.adjust(PValue, method = "BH"))

# Filter significant correlations
significant_corr <- cor_pval_long %>%
  filter(AdjPValue < 0.05) %>%
  arrange(AdjPValue)

print(significant_corr)

# Set non-significant correlations to 0 for clustering
adjusted_cor_matrix <- correlations
adjusted_cor_matrix[pvalues > 0.05] <- 0

# Create ggcorrplot object
plot4 <- ggcorrplot(
  adjusted_cor_matrix,
  tl.cex   = 18,
  type     = "lower",
  lab      = FALSE,
  hc.order = TRUE,
  colors   = c("blue", "white", "red"),
  outline.color = NA,
  p.mat    = pvalues,
  sig.level = 0.05,
  insig    = "blank"
)

# Extract data for custom labels with significance asterisks
plot_data <- plot4[["data"]] %>%
  mutate(
    label = ifelse(
      is.na(pvalue),
      "",
      ifelse(
        pvalue < 0.01,
        paste0(value, "**"),
        ifelse(pvalue < 0.05, paste0(value, "*"), value)
      )
    )
  )

plot_data <- plot_data %>%
  filter(signif == 1) %>%
  select(-signif)

plot4 <- plot4 +
  geom_text(
    data   = plot_data,
    aes(x = Var1, y = Var2, label = label),
    color  = "black",
    nudge_y = 0.01,
    size   = 7
  ) +
  theme(
    axis.text.x  = element_text(size = 20, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.size  = unit(1.5, "cm"),
    legend.key.width = unit(1, "cm")
  )

output_path <- file.path(wdir, "results", "figure4.correlations.v1.0.png")

ggsave(
  filename = output_path,
  plot     = plot4,
  device   = "png",
  width    = 22,
  height   = 34,
  dpi      = 300
)


#############################################################
## Part 6: Stacked plots – predictors vs. latency (Figure 5)
#############################################################
## - Scatter plots (predictor vs DOT latency) for punished vs rewarded
## - Overlaid regression lines
## - Panel-wise r / significance annotations

variables <- c("PANDA_gesamt", "BIS_gesamt", "LEDD", "DAA")

variable_labels <- c(
  "PANDA_gesamt" = "PANDA total score",
  "BIS_gesamt"   = "Barrett Impulsivity Scale-11",
  "LEDD"         = "LEDD [mg]",
  "DAA"          = "Dopamine agonist dose (DAA)"
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

