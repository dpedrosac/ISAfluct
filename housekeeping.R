#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  housekeeping.R
# Purpose:      Some manual changes of variables to clean up the nomenclature
# 		and to make it easier to follow.
#
# Author(s):    Jan-Philipp Bach, Dilara Bingöl, Andreas Mäckel,
#		Franziska Maier, Josefine Waldthaler, David Pedrosa
#		
#
# Notes:
# - Project:    ISAfluct (2018–2026)
# - Repository: https://github.com/dpedrosac/ISAfluct/
#
# Usage: 	source("scripts/housekeeping.R")
# =============================================================================

df_patients_save <- df_patients # saves information

# Helpers ----------------------------------------------------------------------

to_num_comma <- function(x) {
  # Convert strings like "1,23" to numeric 1.23
  as.numeric(stringr::str_replace(as.character(x), ",", "."))
}

# ---- 1) Standardize subject IDs and rename BIS columns -----------------------

rename_if_present <- function(data, old, new) {
  if (old %in% names(data)) {
    dplyr::rename(data, !!rlang::sym(new) := !!rlang::sym(old))
  } else {
    data
  }
}

# --- 1. Rename IDs ---
df_patients <- df_patients %>%
  mutate(subj_ID = Probanden_ID)

# --- 2. Rename Columns ---	
df_patients$BIS_T1=df_patients$BIS_1
df_patients$BIS_T2=df_patients$BIS_2
df_patients$BIS_T3=df_patients$BIS_3

df_patients$BIS_attention=df_patients$BIS_1
df_patients$BIS_motor=df_patients$BIS_2
df_patients$BIS_non.planning=df_patients$BIS_3


df_contr.subj <- df_contr.subj %>%
  mutate(subj_ID = Kontrollprobanden_ID_ISA_FLUCT)

# --- 3. Recode Time-Resolved Variables ---
df_contr.subj <- df_contr.subj %>%
  mutate(
    DOT_Score_T1 = ifelse(MZP == 1, DOT_Score, NA),
    DOT_Score_T2 = ifelse(MZP == 2, DOT_Score, NA),
    DOT_Score_T3 = ifelse(MZP == 3, DOT_Score, NA),
    DOT_Klicks_T1 = ifelse(MZP == 1, DOT_Klicks, NA),
    DOT_Klicks_T2 = ifelse(MZP == 2, DOT_Klicks, NA),
    DOT_Klicks_T3 = ifelse(MZP == 3, DOT_Klicks, NA)
  )

# --- 4. Combine Patients and Controls ---
df_total <- bind_rows(df_patients, df_contr.subj)

# --- 5. Convert Columns to Numeric ---
df_total <- df_total %>%
  mutate(
    Medi_Park_Dopagonist_Pramipexol_mg_Tagesdosis = as.numeric(str_replace(Medi_Park_Dopagonist_Pramipexol_mg_Tagesdosis, ",", "."))
  )

# --- 6. Calculate Mean Scores ---
df_total <- df_total %>%
  rowwise() %>%
  mutate(
    DOT_Score = if_else(
      is.na(DOT_Score),
      mean(c_across(c(DOT_Score_T1, DOT_Score_T2, DOT_Score_T3)), na.rm = TRUE),
      DOT_Score
    ),
    DOT_Klicks = if_else(
      is.na(DOT_Klicks),
      mean(c_across(c(DOT_Klicks_T1, DOT_Klicks_T2, DOT_Klicks_T3)), na.rm = TRUE),
      DOT_Klicks
    ),
    ISAm_total = mean(c_across(c(ISA_m_T1, ISA_m_T2, ISA_m_T3)), na.rm = TRUE),
    ISAm_LID_total = mean(c_across(c(ISA_m_T1_LID, ISA_m_T2_LID, ISA_m_T3_LID)), na.rm = TRUE),
    ISAm_Hypokin_total = mean(c_across(c(ISA_m_T1_Hypokin, ISA_m_T2_Hypokin, ISA_m_T3_Hypokin)), na.rm = TRUE)
  ) %>%
  ungroup()

# --- 7. Calculate LEDD ---
df_total <- df_total %>%
  mutate(
    LEDD = Tagesäqiuvalenzdosis_L_DOPA_mg
  )

# --- 8. Transform Columns to Double and Calculate DAA ---
df_total <- df_total %>%
  mutate(
    Medi_Park_Dopagonist_Rotigotin_Pflaster_mg_Tagesdosis = replace_na(as.numeric(str_replace(Medi_Park_Dopagonist_Rotigotin_Pflaster_mg_Tagesdosis, ",", ".")), 0),
    Medi_Park_Dopagonist_Ropinirol_mg_Tagesdosis = replace_na(as.numeric(str_replace(Medi_Park_Dopagonist_Ropinirol_mg_Tagesdosis, ",", ".")), 0),
    Medi_Park_Dopagonist_Pramipexol_mg_Tagesdosis = replace_na(as.numeric(str_replace(Medi_Park_Dopagonist_Pramipexol_mg_Tagesdosis, ",", ".")), 0),
    Medi_Park_Dopagonist_Piribedil_mg_Tagesdosis = replace_na(as.numeric(str_replace(Medi_Park_Dopagonist_Piribedil_mg_Tagesdosis, ",", ".")), 0)
  ) %>%
  mutate(
    DAA = (Medi_Park_Dopagonist_Rotigotin_Pflaster_mg_Tagesdosis * 30.3) +
          (Medi_Park_Dopagonist_Ropinirol_mg_Tagesdosis * 20) +
          (Medi_Park_Dopagonist_Pramipexol_mg_Tagesdosis * 100) +
          (Medi_Park_Dopagonist_Piribedil_mg_Tagesdosis * 1)       
  )
  
# --- 9. Add columns for cognitive screening and for UPDRS-I ---  

df_total <- df_total %>%
  mutate(MDS_UPDRS_I = MDS_UPDRS_Ia + MDS_UPDRS_Ib)

df_total <- df_total %>%
  mutate(Cognitive_Screening = coalesce(PANDA_gesamt, MMST))

df_total <- df_total %>%
 mutate(
    # Derive MCI per group:
    # - controls: MMSE in [24, 27]
    # - patients: PANDA in [15, 17]
    MCI = case_when(
      group == "control" & !is.na(MMST) & between(MMST, 24, 27)   ~ "yes",
      group == "control" & !is.na(MMST)                            ~ "no",
      group == "patient" & !is.na(PANDA_gesamt) & between(PANDA_gesamt, 15, 17)  ~ "yes",
      group == "patient" & !is.na(PANDA_gesamt)                           ~ "no",
      TRUE                                                         ~ NA_character_
    ),
    MCI = factor(MCI, levels = c("no", "yes"))
  )
  
  
# --- 10. Initialize new columns for the DOT latencies ---  
columns_to_add <- c(
  "DOT_latency", "DOT_latency_T1", "DOT_latency_T2", "DOT_latency_T3",
  "DOT_pE_latency", "DOT_pE_latency_T1", "DOT_pE_latency_T2", "DOT_pE_latency_T3",
  "DOT_npE_latency", "DOT_npE_latency_T1", "DOT_npE_latency_T2", "DOT_npE_latency_T3"
)

new_columns <- setNames(rep(list(NA_real_), length(columns_to_add)), columns_to_add)
df_total <- df_total %>%
  mutate(!!!new_columns)
  

timepoints <- c("morgens", "mittags", "abends")  # Define timepoints

# Loop through rows in df_total
for (i in 1:nrow(df_total)) {
  subj_id <- df_total$subj_ID[i]
  
  if (df_total$group[i] == "control") {
    DOT_dir <- file.path(wdir, "raw_data", "DOT-Rohdaten Normprobanden")
    pattern <- sprintf("dooropeningtask_raw_ISAFLUCT%s", subj_id)
    
    # Search for the file
    files <- list.files(
      path = DOT_dir,
      pattern = pattern,
      full.names = TRUE
    )
    
    if (length(files) == 1) {  # If a single file is found
      latency <- read_latency(files[1])
      df_total$DOT_latency[i] <- latency$mean_valid
      df_total$DOT_pE_latency[i] <- latency$mean_prev
      df_total$DOT_npE_latency[i] <- latency$mean_other
    } else {
      cat("No file or multiple files found for subject:", subj_id, "\n")
    }
    
  } else {  # For patients
    DOT_dir <- file.path(wdir, "raw_data", "DOT-Rohdaten Parkinsonpatienten")
    for (k in seq_along(timepoints)) {
      timepoint <- timepoints[k]
      DOR_dir <- file.path(DOT_dir, sprintf("Ergebnisse Patienten %s", timepoint))
      pattern <- sprintf("dooropeningtask_raw_%s", gsub("_", "", subj_id))
      
      # Search for the file
      files <- list.files(
        path = DOR_dir,
        pattern = pattern,
        full.names = TRUE
      )
      
      if (length(files) == 1) {  # If a single file is found
        latency <- read_latency(files[1])
        
        # Assign latency to the correct timepoint column
        if (timepoint == "morgens") {
          df_total$DOT_latency_T1[i] <- latency$mean_valid
          df_total$DOT_pE_latency_T1[i] <- latency$mean_prev
          df_total$DOT_npE_latency_T1[i] <- latency$mean_other          
	} else if (timepoint == "mittags") {
          df_total$DOT_latency_T2[i] <- latency$mean_valid
          df_total$DOT_pE_latency_T2[i] <- latency$mean_prev
          df_total$DOT_npE_latency_T2[i] <- latency$mean_other
        } else if (timepoint == "abends") {
          df_total$DOT_latency_T3[i] <- latency$mean_valid
          df_total$DOT_pE_latency_T3[i] <- latency$mean_prev   
          df_total$DOT_npE_latency_T3[i] <- latency$mean_other
        }
      } else {
        cat("No file or multiple files found for subject:", subj_id, "at timepoint:", timepoint, "\n")
      }
    }
    
    # List of prefixes for latency calculations
    latency_types <- c("DOT_latency", "DOT_pE_latency", "DOT_npE_latency")

	# Loop over each type of latency and calculate overall mean
	for (latency_type in latency_types) {
	  # Extract T1, T2, T3 columns dynamically
	  timepoint_columns <- paste0(latency_type, "_T", 1:3)
	  
	  # Calculate the mean across T1, T2, T3 and assign to the main column
	  df_total[[latency_type]][i] <- mean(as.numeric(df_total[i, timepoint_columns]), na.rm = TRUE)
	}    
  }
}

# --- 10. Add subscores for ISAm/Hypokinesia and ISAm LID and rename them --- 
df_total <- df_total %>%
  rename_with(
    ~ str_replace(.x, "^ISA_m_(T[0-9]+)_Hypokin$", "ISA_m_Hypokin_\\1"),
    matches("^ISA_m_T[0-9]+_Hypokin$")
  )

df_total <- df_total %>%
  rename_with(
    ~ str_replace(.x, "^ISA_m_(T[0-9]+)_LID$", "ISA_m_LID_\\1"),
    matches("^ISA_m_T[0-9]+_LID$")
  )


