# This is code to analyse the ISAfluct data which was collected from 2020 - 2023
# Code developed by David Pedrosa

# Version 1.8 # 2025-13-11 # Final comments in the MS included and some bugs corrected.

# Preamble
# ==================================================================================================
## In case of multiple people working on one project, this helps to create an automatic script

username = Sys.info()["login"]
if (username == "dpedr") {
    wdir = "D:/ISAfluct/"
} else if (username == "david") {
    wdir = "/media/storage/ISAfluct/"
}
setwd(wdir)
source("packages.R")

df_patients    <- read_sav(file.path(wdir, "raw_data", "rohdaten_patientinnen_isafluct.sav")) %>% mutate(group=factor("patient"))
df_contr.subj  <- read_sav(file.path(wdir, "raw_data", "rohdaten_kontrollprobanden.sav")) %>% mutate(group=factor("control"))
df_total    <- bind_rows(df_patients, df_contr.subj)

source("extract_latencies.R") # Function needed to extract latencies
source("housekeeping.R")

# Part 1: Create TableOne using [tableone] package for general results (Table 1)
# ==================================================================================================
# Create a copy of df_total for analysis
df_tableone <- df_total

# Convert specified 	columns to factors
vars.factors <- c("Geschlecht", "Schulabschluss")
df_tableone[vars.factors] <- lapply(df_tableone[vars.factors], function(x) factor(as.character(x)))

# Select relevant columns for analysis
vars.proportional <- c(
  "Geschlecht", "Schulabschluss", "Alter", "Krankheitsdauer_Jahre", "BDI", 
  "AES", "MSS", "MDS_UPDRS_I", "MDS_UPDRS_II", "MDS_UPDRS_III_Mittelwert", 
  "MDS_UPDRS_IV", "Tagesäqiuvalenzdosis_L_DOPA_mg", "PANDA_gesamt", "MMST", "MCI", "group"
)
df_tableone <- df_tableone %>% 
  dplyr::select(all_of(vars.proportional))

# Rename columns for clarity
colnames_Vars <- c(
  "Gender", "Education", "Age", "Disease duration", "BDI-II*", "AES†", 
  "MSS‡", "MDS-UPDRS part I", "MDS-UPDRS part II", "MDS-UPDRS part III", 
  "MDS-UPDRS part IV", "LEDD§ [in mg]", "PANDA¶", "MMSE**", "MCI", "group"
)
colnames(df_tableone) <- colnames_Vars

# Update group names and recode categorical variables
df_tableone <- df_tableone %>%
  mutate(
    group = recode(group, "patient" = "PD patients", "control" = "Control subjects"),
    Gender = recode(Gender, "0" = "Female", "1" = "Male"),
    Education = recode(
      Education, 
      "1" = "primary school", 
      "2" = "lower secondary school degree", 
      "3" = "intermediate secondary school", 
      "4" = "High school diploma or university entrance qualification"
    )
  )

# Define variables for the TableOne object
vars.proportional <- c(
  "Gender", "Age", "Disease duration", "BDI-II*", "AES†", # "Education" was removed 
  "MSS‡", "MDS-UPDRS part I", "MDS-UPDRS part II", "MDS-UPDRS part III", 
  "MDS-UPDRS part IV", "LEDD§ [in mg]", "PANDA¶", "MMSE**", "MCI"
)
factVars <- c("Gender", "MCI") #, "Education")

# Create the TableOne object
tableOne <- CreateTableOne(
  vars = vars.proportional,
  strata = "group",
  factorVars = factVars,
  data = df_tableone
)

# Convert TableOne to a data frame with row names as a column
tableOne_df <- as.data.frame(
  print(
    tableOne, 
    nonnormal = c("MCI"), # c("Education"), 
    showAllLevels = FALSE # Show single category for Gender
  )
)
tableOne_df <- tibble::rownames_to_column(tableOne_df, var = "Variable")

blank_row <- data.frame(
  "Variable" = "Abbreviations: * Beck's Depression Inventory-II, † Apathy evaluation scale, ‡ ??, § Levodopa-equivalent dosage, ¶ Parkinson Neuropsychometric Dementia Assessment, ** Mini-Mental State Examination",
  "PD patients" = NA,
  "Control subjects" = NA,
  "p" = NA,
  "test" = NA
)

colnames(blank_row) <- colnames(tableOne_df)

# Append the annotations and write to csv
tableOne_df <- rbind(tableOne_df, blank_row)

write.csv(
  tableOne_df, 
  file = file.path(wdir, "results", "table1.general_results.v1.1.csv"), 
  row.names = FALSE
)

# Part 2: Create normative data (Figure 1)
# ==================================================================================================

# 1) Prep: make age bins and tidy factors

# df_total_prepped <- df_total %>%
#  mutate(
#    age_bin = cut(
#      Alter,
#      breaks = c(55, 64, 73, 89, Inf),
#      labels = c("55–63", "63–72", "75–84", "85+"),
#      right = FALSE
#    ),
#    Gender = recode(Geschlecht,
#                        `0` = "female",
#                        `1` = "male")
#  ) %>%
#  filter(!is.na(age_bin))

qs <- quantile(df_total$Alter,
               probs = seq(0, 1, length.out = 5),
               na.rm = TRUE)

qs_round <- round(qs)

# Assign the ages to the equally distributed age-bins
df_total_prepped <- df_total %>%
  mutate(
    age_bin = cut(
      Alter,
      breaks = qs_round,
      include.lowest = TRUE,
      right = FALSE
    ),
    Gender = dplyr::recode(Geschlecht,
                           `0` = "female",
                           `1` = "male")
  ) %>%
  filter(!is.na(age_bin))
  
# Renamne the factors/age-bins
df_total_prepped <- df_total_prepped %>%
  mutate(
    age_bin = forcats::fct_relabel(age_bin, function(x) {
      x <- gsub("\\[|\\(|\\)", "", x)
      x <- gsub(",", "–", x)	
      x
    })
  )
  
levels(df_total_prepped$age_bin) = c("49–65", "65–71", "71–79", "79–94") # dirty hack!!

# 2) Reusable plotting function
plot_norm_box <- function(data, var) {
  v <- rlang::ensym(var)
  ylab <- rlang::as_name(v)

  # Global stats
  stats <- data %>%
    summarise(
      mu = mean(!!v, na.rm = TRUE),
      sd = sd(!!v, na.rm = TRUE),
      .groups = "drop"
    )

  # n per age bin
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
      width = 0.7,
      position = position_dodge(width = 0.8),
      alpha = 0.5
    ) +
    geom_hline(yintercept = stats$mu + 2 * stats$sd, linetype = "dashed") +
    geom_hline(yintercept = stats$mu - 2 * stats$sd, linetype = "dashed") +

    geom_text(
      data = n_labels,
      inherit.aes = FALSE,
      aes(x = age_bin, y = label_y, label = paste0("n = ", n)),
      vjust = -0.5,
      size = 5
    ) +

    scale_fill_brewer(palette = "Set1") +
    labs(
      x   = "Age bin",
      y   = ylab,
      fill = "Gender"
    ) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "bottom")
}

# 3) Use it for both variables (no changes needed)
p_latency <- plot_norm_box(df_total_prepped %>% filter(group=="control"), DOT_latency)
p_score   <- plot_norm_box(df_total_prepped %>% filter(group=="control"), DOT_Klicks)

p_latency <- p_latency + labs(y = "Latency (in ms)", title = "DOT latency")
p_score   <- p_score + labs(y = "Count (n)", title = "Opened doors before quitting")

combined_plot <- (
  p_latency + p_score
) +
  plot_layout(
    guides = "collect",      # one shared legend
    axis_titles = "collect"  # share axis titles where possible
  ) +
  plot_annotation(tag_levels = "A") &   # adds A, B tags
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 16)
  )

print(combined_plot)

output_path <- file.path(wdir, "results", "figure2.normativeData.v1.0.png")
ggsave(
  filename = output_path,
  plot = combined_plot,
  device = "png",
  width = 18,   # Width of the output image in inches
  height = 10,  # Height of the output image in inches
  dpi = 300     # Resolution of the output image
)

# Data for description in the manuscript

normative_data <- df_total_prepped %>%
  group_by(group) %>%
  summarise(
    mean_latency = mean(DOT_latency, na.rm = TRUE),
    sd_latency   = sd(DOT_latency, na.rm = TRUE),
    mean_score   = mean(DOT_Score, na.rm = TRUE),
    sd_score     = sd(DOT_Score, na.rm = TRUE),
    min_score 	 = min(DOT_Score, na.rm = TRUE), 
    max_score 	 = max(DOT_Score, na.rm = TRUE), 
    min_latency  = min(DOT_latency, na.rm = TRUE), 
    max_latency  = max(DOT_latency, na.rm = TRUE),     
    n            = n()
  )
  
# Filter to controls
df_control <- df_total_prepped %>%
  filter(group == "control")

# Paired t-test: pE_latency vs. npE_latency
t_res <- t.test(df_control$DOT_pE_latency,
                df_control$DOT_npE_latency,
                paired = TRUE)

print(t_res)  

# Part 3: Create repeated measures table and figure (suppl. Figure 1)
# ==================================================================================================
variables_repeated <- c("MDS_UPDRS_III", "ISA_m", "BIS", "DOT_Klicks", "DOT_latency", "Schmerz_VAS_Rang")

df_longRM <- df_total %>% # Convert data to long format
  pivot_longer(
    cols = matches(paste0("^(", paste(variables_repeated, collapse = "|"), ")_T[1-3]$")), # Match _T1, _T2, _T3
    names_to = c("Variable", "Time"),
    names_pattern = "([a-zA-Z0-9_]+)_T([1-3])",
    values_to = "Value"
  ) %>%
  mutate(Time = as.factor(Time)) # Convert Time to numeric for ANOVA

df_longRM <- df_longRM %>%
  mutate(across(where(is.labelled), as.numeric))

df_longRM <- df_longRM %>%
  mutate(
    group = as_factor(group),
    Time = as_factor(Time)
  )

# Summarize data: mean and SD for each variable and time point
summary_statsRM <- df_longRM %>%
  group_by(Variable, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Mean_SD = paste0(round(Mean, 2), " ± ", round(SD, 2)))

# Repeated measures ANOVA using afex-package
anova_results_afex <- df_longRM %>%
  filter(group == "patient") %>%
  group_by(Variable) %>%
  summarise(
    anova = list(
      aov_car(Value ~ Time + Error(subj_ID / Time), data = cur_data())
    ),
    .groups = "drop"
  )

# Extract summaries
anova_summaries <- anova_results_afex %>%
  mutate(
    F_value = map_dbl(anova, ~ .x$anova_table$F[1]),
    P_value = map_dbl(anova, ~ .x$anova_table$Pr[1]),
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  select(Variable, F_value, P_value, Significance)

# Reshape summary_stats for table format
summary_tableRM <- summary_statsRM %>%
  select(Variable, Time, Mean_SD) %>%
  pivot_wider(names_from = Time, values_from = Mean_SD, names_prefix = "T") %>%
  left_join(anova_summaries, by = "Variable")

rename_map <- c(
  "MDS_UPDRS_III" = "MDS-UPDRS part III",
  "ISA_m" = "ISA motor score",
  "BIS" = "Barrett Impulsivity Scale-11",
  "DOT_Klicks" = "Door Opening Task, Clicks",
  "DOT_latency" = "Door Opening Task, Latency",
  "Schmerz_VAS_Rang" = "Visual Analog Scale for Discomfort"
)

# Apply the mapping to the Variable column
summary_tableRM <- summary_tableRM %>%
  mutate(Variable = recode(Variable, !!!rename_map))
colnames(summary_tableRM) <- c("Item", "morning", "noon", "evening", "F-value", "p", "sig.")
print(summary_tableRM)

custom_order <- c("MDS-UPDRS part III", "Barrett Impulsivity Scale-11", "ISA motor score", 
			"Door Opening Task, Clicks", "Door Opening Task, Latency", 
			"Visual Analog Scale for Discomfort")

summary_tableRM <- summary_tableRM %>% # sort by [custom_order]
  mutate(Item = factor(Item, levels = custom_order)) %>%
  arrange(Item)

# Save as CSV
write.csv(
  summary_tableRM,
  file = file.path(wdir, "results", "table2.repeated_measures.v1.0.csv"),
  row.names = FALSE
)

create_subplot <- function(variable_name, df, title = NULL, group_colors = c("Patient" = "blue", "Control" = "orange")) {
  # Reshape data to long format with an ID column
  df_long <- df %>%
    select(Probanden_ID, group, starts_with(variable_name)) %>%
    pivot_longer(
      cols = starts_with(paste0(variable_name, "_T")),
      names_to = "Time",
      values_to = "Score"
    ) %>%
    mutate(
      Time = case_when(
        .data$Time == paste0(variable_name, "_T1") ~ "T1",
        .data$Time == paste0(variable_name, "_T2") ~ "T2",
        .data$Time == paste0(variable_name, "_T3") ~ "T3"
      ),
      group = factor(group),
      Time = factor(Time, levels = c("T1", "T2", "T3"))
    )

  # Filter out rows with missing IDs
  df_filtered <- df_long %>%
    filter(!is.na(Probanden_ID))
  
  # Run a two-way mixed ANOVA
  anova_results <- aov(Score ~ Time + Error(Probanden_ID / Time), data = df_filtered)
  print(summary(anova_results)) # Print ANOVA results for debugging
  
  # Summarize data for plotting
  df_summary <- df_long %>%
    group_by(group, Time) %>%
    summarise(
      Mean = mean(Score, na.rm = TRUE),
      SEM = sd(Score, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  plot <- ggplot() +
  # Add boxplot for the groups at each time point
  geom_boxplot(
    data = df_long, 
    aes(x = Time, y = Score, fill = group), 
    width = .5,
    outlier.shape = NA, 
    alpha = 0.5
  ) +
  # Add jittered individual points
  geom_jitter(
    data = df_long, 
    aes(x = Time, y = Score, fill="gray"),
    color = "black",            # Set all jitter points to gray
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 1), 
    size = 2, 
    alpha = 0.2
  ) +
  # Add annotation text for statistical results
  annotate(
    "text", 
    x = 1.5, 
    y = max(df_long$Score, na.rm = TRUE) * 1.1, 
    label = sprintf("F = %.2f\n%s", anova_results[[1]]$`F value`[1], "p < 0.05"), 
    size = 5
  ) +
  # Customize plot appearance
  labs(
    title = ifelse(is.null(title), paste(variable_name, "Scores"), title),
    x = "Time Point",
    y = "Score"
  ) +
  # ylim(y_limits) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") + # Use Set1 palette for fill colors
  # scale_color_manual(values = group_colors) + # Customize colors for groups
  scale_x_discrete(
    labels = c("T1" = "morning", "T2" = "noon", "T3" = "evening"), # Update column labels
    expand = expansion(mult = c(0.5, 0.5))  # Add extra spacing between timepoints
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.position = "none"
  )
  
  return(plot)
}

df_plotRM <- df_total %>% 
	select(Probanden_ID, group, ISA_m_T1, ISA_m_T2, ISA_m_T3, 
					BIS_T1, BIS_T2, BIS_T3, 
					MDS_UPDRS_III_T1, MDS_UPDRS_III_T2, MDS_UPDRS_III_T3)

variables <- c("ISA_m", "BIS", "MDS_UPDRS_III")
subplots <- lapply(variables, function(var) create_subplot(variable_name = var, df = df_plotRM))

subplots[[1]]$labels$title <- "Motor ISA"
subplots[[2]]$labels$title <- "Barrett Impulsiveness Scale-11"
subplots[[3]]$labels$title <- "MDS-UPDRS, part III"

# Combine subplots into a single figure using patchwork (optional)
combined_plot <- subplots[[1]] / subplots[[2]] / subplots[[3]] + plot_layout(axis_titles='collect') + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size=16))
print(combined_plot)

output_path <- file.path(wdir, "results", "suppl.figure1.rmResults.v1.0.png")
ggsave(
  filename = output_path,
  plot = combined_plot,
  device = "png",
  width = 10,   # Width of the output image in inches
  height = 18,  # Height of the output image in inches
  dpi = 300     # Resolution of the output image
)


# Part 4: Start with the DOT analyses (see Figure 2) 
# ==================================================================================================

df_longISAb <- df_total %>% # Reshape to long format
  pivot_longer(
    cols = starts_with("DOT_Klicks"),                # Columns to reshape
    names_to = c("Score_Type", "Time"),             # Split into Score_Type and Time
    names_pattern = "(DOT_Klicks)_(T\\d+)",          # Regex to capture both Score_Type and Time
    values_to = "DOT_Klicks"                         # Name for reshaped values
  )
  
plot2A <- ggplot(df_longISAb %>% filter(!is.na(Time)), aes(x = Time, y = DOT_Klicks, fill = group)) +
  # Add boxplots without the default median line
  geom_boxplot(
    outlier.shape = NA, 
    alpha = 0.5, 
    position = position_dodge(1.1),        # Consistent dodge width
    lwd = 0.6,                             # Box border line width
    fatten = 0                             # Remove default median line
  ) + 
  # Add custom median lines
  stat_summary(
    fun = median, 
    geom = "crossbar", 
    position = position_dodge(1.1),        # Match dodge width for alignment
    width = 0.8,                          # Wider than the box
    size = 1                              # Adjusted thickness
  ) +
  # Add jittered points with transparency
  geom_jitter(aes(group = group), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 1.1), # Match dodge width
              size = 2, alpha = 0.5) +    # Reduced alpha for jitter
  labs(
    title = "DOT results over the day",
    x = NULL,                            # Updated x-axis title
    y = "Opened doors in \nDoor-Opening-Task (DOT)*"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +                # Use Set1 palette for fill colors
  scale_x_discrete(
    labels = c("T1" = "morning", "T2" = "noon", "T3" = "evening"), # Update column labels
    expand = expansion(mult = c(0.5, 0.5))  # Add extra spacing between timepoints
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Larger title font
    axis.text = element_text(size = 14),                              # Larger axis text
    axis.title.x = element_text(size = 16),                           # Larger x-axis title
    axis.title.y = element_text(size = 16),                           # Y-axis title font size
    legend.title = element_blank()                                    # Remove legend title
  )

# Perform t-test
t_test_result <- t.test(DOT_latency ~ group, data = df_total)

# Extract t-test results
t_stat <- round(t_test_result$statistic, 2)
p_value <- t_test_result$p.value
p_value_text <- ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.3f", p_value))

# Plot the boxplot
plot2B <- ggplot(df_total, aes(x = group, y = DOT_latency, fill = group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5, lwd = .6, fatten = 0) +        # Add boxplots
    # Add custom median s
  stat_summary(
    fun = median, 
    geom = "crossbar", 
    position = position_dodge(1.1),        # Match dodge width for alignment
    width = 0.8,                          # Wider than the box
    size = 1                              # Adjusted thickness
  ) +
  geom_jitter(position = position_jitter(width = 0.2), size = 2, alpha = 0.5) + # Add jitter
  # Add a line connecting groups
  annotate("segment", x = 1, xend = 2, y = max(df_total$DOT_latency, na.rm = TRUE) * 1.05, 
           yend = max(df_total$DOT_latency, na.rm = TRUE) * 1.05, size = 0.8) +
  # Add text annotation
  annotate("text", x = 1.5, y = max(df_total$DOT_latency, na.rm = TRUE) * 1.1, 
           label = sprintf("t = %.2f, %s", t_stat, p_value_text), size = 5) +
  labs(
    title = "Latency for DOT responses",
    x = NULL,
    y = "Latency [in msec.]"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +                 # Use Set1 palette for fill colors
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered, bold title
    axis.text = element_text(size = 14),                              # Larger axis text
    axis.title.y = element_text(size = 16),                           # Larger y-axis title
    axis.title.x = element_blank(),                                   # Blank x-axis title
    legend.position = "none"                                          # Remove legend
  )

# In a next step, the possibility of different effects of feedback on the latency should
# be tested
df_long <- df_total %>%
  select(group, DOT_npE_latency, DOT_pE_latency) %>% # Select relevant columns
  pivot_longer(
    cols = starts_with("DOT_"),              # Combine DOT_npE_latency and DOT_pE_latency
    names_to = "Latency_Type",               # New column for latency type
    values_to = "Latency"                    # New column for latency values
  ) %>%
  mutate(Latency_Type = factor(Latency_Type, levels = c("DOT_npE_latency", "DOT_pE_latency")))

anova_result <- aov(Latency ~ Latency_Type * group, data = df_long)

# Print ANOVA summary
summary(anova_result)

# Perform t-tests for each Latency_Type
t_test_results <- df_long %>%
  group_by(Latency_Type) %>%
  summarise(
    t_stat = round(t.test(Latency ~ group)$statistic, 2),
    p_value = t.test(Latency ~ group)$p.value
  ) %>%
  mutate(
    p_value_text = ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.3f", p_value))
  )

# Perform ANOVA for interaction
anova_result <- aov(Latency ~ as.factor(Latency_Type) * group, data = df_long)
anova_summary <- summary(anova_result)
interaction_f_stat <- round(anova_summary[[1]]$`F value`[3], 2)
interaction_p_value <- anova_summary[[1]]$`Pr(>F)`[3]
interaction_p_text <- ifelse(interaction_p_value < 0.001, "p < 0.001", sprintf("p = %.3f", interaction_p_value))

overall_y_min <- min(df_long$Latency, na.rm = TRUE)

# Plot boxplots with group differences and interaction
plot2CboxplotOLD <- ggplot(df_long, aes(x = Latency_Type, y = Latency, fill = group)) +
  # Add boxplots
  geom_boxplot(outlier.shape = NA, alpha = 0.5, position = position_dodge(1.1), lwd = .6, fatten = 0) +        # Add boxplots
    # Add custom median lines
  stat_summary(
    fun = median, 
    geom = "crossbar", 
    position = position_dodge(1.1),        # Match dodge width for alignment
    width = 0.8,                          # Wider than the box
    size = 1                              # Adjusted thickness
  ) +
  # Add jittered points
  geom_jitter(aes(group = interaction(Latency_Type, group)), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
              size = 2, alpha = 0.5) +
  # Annotate t-test results
  geom_segment(
    aes(x = .75, xend = 1.25, y = max(df_long$Latency, na.rm = TRUE) * 1.05, 
        yend = max(df_long$Latency, na.rm = TRUE) * 1.05), 
    size = 0.8, color = "black"
  ) +
  geom_segment(
    aes(x = 1.75, xend = 2.25, 
        y = max(df_long$Latency, na.rm = TRUE) * 1.05, 
        yend = max(df_long$Latency, na.rm = TRUE) * 1.05), 
    size = 0.8, color = "black"
  ) +
  # Annotate t-test results
  geom_text(data = t_test_results, aes(
    x = Latency_Type, y = max(df_long$Latency, na.rm = TRUE) * 1.1,
    label = paste0("t = ", t_stat, "\n", p_value_text)
  ), inherit.aes = FALSE, size = 4, position = position_dodge(width = 0.8)) +
  # Annotate interaction at the bottom
  annotate("text", x = 1.5, y = min(df_long$Latency, na.rm = TRUE) * 0.9, 
           label = paste0("Interaction: F = ", interaction_f_stat, ", ", interaction_p_text),
           size = 5, hjust = 0) +
  labs(
    title = "Latency Differences",
    x = NULL,
    y = "Latency [in msec.]"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +                # Use Set1 palette for fill colors
    scale_x_discrete(
    labels = c("DOT_npE_latency" = "After positive answer", "DOT_pE_latency" = "After negative answer"), # Update column labels
    expand = expansion(mult = c(0.5, 0.5))  # Add extra spacing between timepoints
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered, bold title
    axis.text = element_text(size = 16),                              # Larger axis text
    axis.title.y = element_text(size = 16),                           # Larger y-axis title
    axis.title.x = element_blank(),                                   # Blank x-axis title
    legend.position = "none"                                          # Remove legend
  )

# 1. Summarize the data: Means + SEM
df_summary <- df_long %>%
  group_by(group, Latency_Type) %>%
  summarise(
    mean_latency = mean(Latency, na.rm = TRUE),
    sem_latency = sd(Latency, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# 2. Plot means with SEM as line plot + jittered raw points
plot2C <- ggplot() +
  #geom_jitter(
  #  data = df_long,
  #  aes(x = Latency_Type, y = Latency, color = group),
  #  alpha = 0.4, size = 2,
  #  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4)
  #  ) +
  geom_line(
    data = df_summary,
    aes(x = Latency_Type, y = mean_latency, group = group, color = group),
    size = 1,
    alpha = 0.5	
  ) +
  geom_point(
    data = df_summary,
    aes(x = Latency_Type, y = mean_latency, color = group),
    size = 3,
    alpha = 0.5
  ) +
  geom_errorbar(
    data = df_summary,
    aes(x = Latency_Type, ymin = mean_latency - sem_latency, ymax = mean_latency + sem_latency),
    width = 0.1, size =.5, alpha = .5
  ) +
  labs(
    title = "Latency Differences",
    x = NULL,
    y = "Latency [in msec.]"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_x_discrete(
    labels = c(
      "DOT_npE_latency" = "After positive\n answer", 
      "DOT_pE_latency" = "After negative\n answer"
    ),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  ylim(0,6300) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )


plot2C_box <- ggplot(
  data = df_long,
  aes(x = Latency_Type, y = Latency, fill = group)
) +
  
  # --- Jittered raw data ---
  geom_jitter(
    aes(size = 2, alpha = 0.5),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 0.7
    ),
    size = 1.8,
    alpha = 0.5
  ) +
  
  # --- Boxplots by group ---
  geom_boxplot(
    width = 0.6,
    position = position_dodge(width = 0.7),
    outlier.shape = NA,      # hide outliers because jitter already shows raw points
    alpha = 0.5) +              # slight transparency so points are visible behind
    
    stat_summary(
    fun = median, 
    geom = "crossbar", 
    position = position_dodge(.7),        # Match dodge width for alignment
    width = 0.8,                          # Wider than the box
    size = 1                              # Adjusted thickness
  ) +

  
  labs(
    title = "Latency Differences",
    x = NULL,
    y = "Latency [in msec.]"
  ) +
  
  theme_minimal() +
  
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +   # jitter uses color, box uses fill
  
  scale_x_discrete(
    labels = c(
      "DOT_npE_latency" = "After positive\n answer", 
      "DOT_pE_latency"  = "After negativ\n answer"
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


df_plotRM <- df_total %>% 
	select(Probanden_ID, group, DOT_latency_T1, DOT_latency_T2, DOT_latency_T3)

variables <- c("DOT_latency")
plot2D <- lapply(variables, function(var) create_subplot(variable_name = var, df = df_plotRM))
plot2D[[1]]$labels$title <- "DOT results for PD-patients"
plot2D[[1]]$labels$y <- "Latency [in msec.]"

#combined_plot2 <- plot2A / (plot2B - plot2C) / (plot2D) + 
#  plot_annotation(title = "Combined Results for DOT Analysis", tag_levels = "A") & theme(plot.tag = element_text(face = 'bold', size=16))
#	ggsave(file.path(wdir, "results", "figure3.DOTresults.v1.0.png"), combined_plot2, width = 12, height = 18, dpi = 300)

# Add margin to plot2B
plot2B <- plot2B + theme(plot.margin = margin(r = 120))

# Then combine normally
combined_plot2 <- (plot2A / 
  ((plot2B + plot2C) + plot_layout(widths = c(2, 1))) / 
  plot2D) +
  plot_layout(heights = c(1, 0.7, 1)) +
  plot_annotation(
    title = "Combined Results for DOT Analysis",
    tag_levels = "A"
  ) & 
  theme(
    plot.tag = element_text(face = 'bold', size = 16)
  )
  
# Save the figure
ggsave(
  file.path(wdir, "results", "figure3.DOTresults.v1.0.png"),
  combined_plot2,
  width = 12,
  height = 18,
  dpi = 300
)

# Part 4: Correlation analyses (behaviour ISA)
# ==================================================================================================

## Next level
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

# Create the new data frame
df_longISAb2 <- df_total %>%
  filter(group=="patient") %>%
  dplyr::select(all_of(variables_for_corr)) 

variable_vector <- c(
  "Age", "Disease duration [in years]", "BDI-II", "Barrett Impulsivity Scale",
  "PANDA", "Non-motor symptoms", "MSS", "AES",
  "MDS-UPDRS, part III", "MDS-UPDRS, part I", "MDS-UPDRS, part II", "MDS-UPDRS, part IV",
  "ISAm Score", "LEDD", "subj_ID", 
  "DOT, Klicks", "DAA", "DOT, Latency"
)
colnames(df_longISAb2) <- variable_vector

# Filter out non-numeric columns for correlation analysis
df_numeric <- df_longISAb2 %>%
  select(where(is.numeric))  # Select only numeric columns

# Compute correlation matrix and p-values
compute_correlation_with_pvalues <- function(df) {
  n <- ncol(df)
  corr_matrix <- matrix(NA, n, n)
  pval_matrix <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        corr_matrix[i, j] <- 1
        pval_matrix[i, j] <- NA
      } else {
        test <- cor.test(df[[i]], df[[j]], use = "complete.obs", method="kendall", exact=FALSE)
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
  
  list(correlations = corr_matrix, pvalues = pval_matrix)
}

# Apply the function to compute correlations and p-values
cor_results <- compute_correlation_with_pvalues(df_numeric)

# Extract correlation and p-value matrices
correlations <- cor_results$correlations
pvalues <- cor_results$pvalues

# Flatten the matrices into long format
corr_long <- as.data.frame(as.table(correlations))
pval_long <- as.data.frame(as.table(pvalues))

# Merge correlation and p-value data
cor_pval_long <- left_join(corr_long, pval_long, by = c("Var1", "Var2"))
colnames(cor_pval_long) <- c("Var1", "Var2", "Correlation", "PValue")

# Adjust p-values using Benjamini-Hochberg correction
cor_pval_long <- cor_pval_long %>%
  mutate(AdjPValue = p.adjust(PValue, method = "BH"))

# Filter significant correlations
significant_corr <- cor_pval_long %>%
  filter(AdjPValue < 0.05) %>%
  arrange(AdjPValue)

# View the significant correlations
print(significant_corr)

# Replace non-significant correlations with 0 for clustering purposes
adjusted_cor_matrix <- correlations
adjusted_cor_matrix[pvalues > 0.05] <- 0

# Create a mask for significant correlations (TRUE if significant, FALSE otherwise)
significance_mask <- pvalues <= 0.05

# Plot the correlogram with significant correlations annotated
plot4 <- ggcorrplot(
  adjusted_cor_matrix, 
  tl.cex=18,
  type = "lower",
  lab = FALSE,                      # Annotate values
  # lab_col = "black",               # Annotation color
  # lab_size = 7,                    # Annotation size
  hc.order = TRUE,                 # Hierarchical clustering order
  colors = c("blue", "white", "red"), # Negative, neutral, positive
  outline.color = NA,
  p.mat = pvalues,                 # Use p-values for masking
  sig.level = 0.05,                # Significance level for annotation
  insig = "blank"                  # Remove labels for non-significant correlations
)

# Extract the data from the plot
plot_data <- plot4[["data"]]

# Add significance asterisks to the labels
plot_data <- plot_data %>%
  mutate(
    label = ifelse(
      is.na(pvalue), "",                            # No label if p-value is NA
      ifelse(pvalue < 0.01, paste0(value, "**"),     # Two asterisks for p < 0.01
             ifelse(pvalue < 0.05, paste0(value, "*"), value)) # One asterisk for p < 0.05
    )
  )

plot_data = select(filter(plot_data, signif == 1), -signif)

# Re-plot with geom_text() for custom annotations
plot4 <- plot4 + 
  geom_text(
    data = plot_data,
    aes(x = Var1, y = Var2, label = label),
    color = "black", nudge_y = 0.01, 
            size = 7
  ) + 
  theme(
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),  # X-axis tick labels bigger
    axis.text.y = element_text(size = 20),                         # Y-axis tick labels bigger
    # axis.title.x = element_text(size = 18, face = "bold"),         # X-axis title bigger and bold
    # axis.title.y = element_text(size = 18, face = "bold"),         # Y-axis title bigger and bold
    legend.title = element_text(size = 20, face = "bold"),   # Legend title size
    legend.text = element_text(size = 20),                  # Legend text size
    legend.key.size = unit(1.5, "cm"),                      # Legend key size (box size)
    legend.key.width = unit(1, "cm")                        # Legend key width
  )

output_path <- file.path(wdir, "results", "figure4.correlations.v1.0.png")
ggsave(
  filename = output_path,
  plot = plot4,
  device = "png",
  width = 22,   # Width of the output image in inches
  height = 34,  # Height of the output image in inches
  dpi = 300     # Resolution of the output image
)


## Stacked plots:
variables <- c("PANDA_gesamt", "BIS_gesamt", "LEDD", "DAA")
color_palette <- c("punished" = "black", "rewarded" = "black")

# Prepare data in long format for plotting
df_long <- df_total %>%
  filter(group == "patient") %>%
  pivot_longer(
    cols = c(DOT_pE_latency, DOT_npE_latency),
    names_to = "Latency_Type",
    values_to = "Latency"
  ) %>%
  mutate(
    Latency_Type = recode(Latency_Type, DOT_pE_latency = "punished", DOT_npE_latency = "rewarded")
  )

# Function to generate combined plots
generate_combined_plots <- function(df_long, variables, color_palette) {
  plots <- list()
  
for (i in seq_along(variables)) {
  var <- variables[i]
    # Filter and compute correlation for alpha transparency
    df_filtered <- df_long %>%
      select(all_of(c(var, "Latency", "Latency_Type"))) %>%
      filter(!is.na(cur_data()[[var]]) & !is.na(Latency))
    
    cor_results <- df_filtered %>%
      group_by(Latency_Type) %>%
      summarise(
        cor_test = list(cor.test(.data[[var]], Latency)),
        .groups = "drop"
      ) %>%
      mutate(
        r_value = map_dbl(cor_test, ~ round(.x$estimate, 2)),
        p_value = map_dbl(cor_test, ~ .x$p.value),
        significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ "ns"
        )
      )
    
    legend_pos <- if (var == tail(variables, 1)) c(1,0) else "none"
	is_right_column <- i %% 2 == 0

y_axis_title_theme <- if (is_right_column) theme(axis.title.y = element_blank()) else theme()
y_mid <- max(df_filtered$Latency, na.rm = TRUE) * 0.5

    # Create scatter plot with lm lines for both latency types
plot <- ggplot(df_filtered, aes_string(x = var, y = "Latency", color = "Latency_Type", shape = "Latency_Type")) +
  geom_point(alpha = 0.7, size = 5) +  # Different shapes based on Latency_Type
  geom_smooth(aes(linetype = Latency_Type), method = "lm", formula = y ~ x, se = FALSE, size = 1) +
  scale_color_manual(values = color_palette) +
  scale_shape_manual(values = c("punished" = 16, "rewarded" = 17)) +  # Different shapes for pE and npE
  scale_linetype_manual(values = c("punished" = "solid", "rewarded" = "dashed")) +
  labs(
    title = paste(var, "vs Latency"),
    # x = var,
    y = "Latency [ms]"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = legend_pos,
    plot.margin = margin(15, 25, 15, 25)  # top, right, bottom, left
  )
  
    # Annotate correlation values
    cor_labels <- cor_results %>%
      mutate(label = paste0(Latency_Type, ": r = ", r_value, " ", significance))
    
 #   plot <- plot +
 # 	annotate(
 #   "text",
 #   x = Inf, y = y_mid,
 #   label = paste(cor_labels$label, collapse = "\n"),
 #   hjust = 1.1, vjust = 1.1,
 #   size = 6,
 #   color = "black"
 # 	)
 #       

plot <- plot +
  annotate(
    "label",
    x = Inf,
    y = y_mid,
    label = paste(cor_labels$label, collapse = "\n"),
    hjust = 1.1,
    vjust = 0.5,
    size = 6,
    label.size = 0.5,        # thickness of the black border
    label.r = unit(0.1, "lines"),  # corner rounding; set to 0 for sharp corners
    color = "black",         # text color
    fill = "white"           # box fill color
  )

        plot <- plot + y_axis_title_theme



    # Append plot to list
    plots[[var]] <- plot
  }
  
  return(plots)
}

# Generate combined plots
combined_plots <- generate_combined_plots(df_long, variables, color_palette)

# Combine into one column
plot4_combined <- wrap_plots(combined_plots, ncol = 2) +
  plot_layout(
    #axis_titles = 'collect',
    guides = 'collect',
    widths = c(1, 1),
    heights = c(1, 1)
  ) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(face = 'bold', size = 12),
    plot.margin = margin(40, 40, 40, 40),  # outer space
    plot.spacing = unit(2, "lines")  # controls inner spacing between subplots
  )


# wrap_plots(combined_plots, ncol = 2) + plot_layout(axis_titles='collect') + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = 'bold', size=14))
print(plot4_combined)

output_path <- file.path(wdir, "results", "figure5.latencies_correlated.v1.0.png")
ggsave(
  filename = output_path,
  plot = plot4_combined,
  device = "png",
  width = 15,   # Width of the output image in inches
  height = 25,  # Height of the output image in inches
  dpi = 300     # Resolution of the output image
)

