#!/usr/bin/env Rscript
# =============================================================================
# Script Name:  TableOne.v1.0.R
# Purpose:      Create Table 1 (general characteristics) stratified by group
#               using tableone; export as CSV for manuscript use.
#
# Author(s):    David Pedrosa
#
# ---------------------------
#
# Notes:
#   - Project: ISAfluct (2018–2026)
#   - GitHub repository: https://github.com/dpedrosac/ISAfluct/
#
# ---------------------------

# ---- Prepare data ------------------------------------------------------------

vars_raw <- c(
  "Geschlecht",
  "Schulabschluss",
  "Alter",
  "Krankheitsdauer_Jahre",
  "BDI",
  "AES",
  "MSS",
  "MDS_UPDRS_I",
  "MDS_UPDRS_II",
  "MDS_UPDRS_III_Mittelwert",
  "MDS_UPDRS_IV",
  "Tagesäqiuvalenzdosis_L_DOPA_mg", # [sic!]
  "PANDA_gesamt",
  "MMST",
  "MCI",
  "BIS_attention", 
  "BIS_motor",
  "BIS_non.planning",
  "group"
)

# Work on a copy to keep df_total intact, then rename to "safe" internal names
df_tableone <- df_total %>%
  dplyr::select(dplyr::all_of(vars_raw)) %>%
  dplyr::rename(
    gender      = Geschlecht,
    education   = Schulabschluss,
    age         = Alter,
    disease_yrs = Krankheitsdauer_Jahre,
    bdi         = BDI,
    aes         = AES,
    mss         = MSS,
    updrs_1     = MDS_UPDRS_I,
    updrs_2     = MDS_UPDRS_II,
    updrs_3     = MDS_UPDRS_III_Mittelwert,
    updrs_4     = MDS_UPDRS_IV,
    ledd_mg     = Tagesäqiuvalenzdosis_L_DOPA_mg,
    panda       = PANDA_gesamt,
    mmse        = MMST,
    mci         = MCI
  ) %>%
  dplyr::mutate(
    gender    = factor(as.character(gender)),
    education = factor(as.character(education)),
    mci       = factor(as.character(mci)),
    group     = dplyr::recode(
      group,
      patient = "PD patients",
      control = "Control subjects"
    ),
    gender = dplyr::recode(
      gender,
      `0` = "Female",
      `1` = "Male"
    ),
    education = dplyr::recode(
      education,
      `1` = "Primary school",
      `2` = "Lower secondary school degree",
      `3` = "Intermediate secondary school",
      `4` = "High school diploma / university entrance qualification"
    )
  )

# Variables to include in Table 1 (Education excluded)
vars_table1 <- c(
  "gender",
  "age",
  "disease_yrs",
  "bdi",
  "aes",
  "mss",
  "updrs_1",
  "updrs_2",
  "updrs_3",
  "updrs_4",
  "ledd_mg",
  "panda",
  "BIS_attention",
  "BIS_motor", 
  "BIS_non.planning",
  "mmse",
  "mci"
)

factor_vars <- c("gender", "mci")

# Pretty labels (display only; internal names remain safe)
var_labels <- c(
  gender      		= "Gender",
  age         		= "Age",
  disease_yrs 		= "Disease duration",
  bdi         		= "BDI-II*",
  aes        		= paste0("AES", "\u2020"),                 # †
  mss         		= paste0("MSS", "\u2021"),                 # ‡
  updrs_1     		= "MDS-UPDRS part I",
  updrs_2     		= "MDS-UPDRS part II",
  updrs_3     		= "MDS-UPDRS part III",
  updrs_4     		= "MDS-UPDRS part IV",
  ledd_mg     		= paste0("LEDD", "\u00A7", " [in mg]"),    # §
  panda       		= paste0("PANDA", "\u00B6"),               # ¶
  BIS_attention		= "BIS-11 (attention factors)",
  BIS_motor   		= "BIS-11 (motor factors)",
  BIS_non.planning	= "BIS-11 (non-planning factors)",
  mmse        		= "MMSE**",
  mci         		= "MCI"
)

# ---- Create TableOne ---------------------------------------------------------

table_one <- tableone::CreateTableOne(
  vars       = vars_table1,
  strata     = "group",
  factorVars = factor_vars,
  data       = df_tableone
)

table_one_df <- as.data.frame(
  print(
    table_one,
    # NOTE: For nonnormal, typically list skewed continuous vars; kept as-is.
    nonnormal     = c("mci"),
    showAllLevels = FALSE
  )
) %>%
  tibble::rownames_to_column(var = "Variable")

# ---- Relabel "Variable" column (post-processing) -------------------------
# TableOne prints row names like "aes", "aes (mean (SD))", "gender = Male (%)", etc.
# We replace ONLY the leading variable token with "pretty" label.

relabel_variable <- function(x, labels) {
  # Replace matches at the start: "^aes\\b" -> "AES†", etc.
  for (nm in names(labels)) {
    x <- stringr::str_replace(x, paste0("^", nm, "\\b"), labels[[nm]])
  }
  x
}

table_one_df <- table_one_df %>%
  dplyr::mutate(Variable = relabel_variable(Variable, var_labels))

# ---- Append abbreviations / footnotes ----------------------------------------

footnote <- tibble::tibble(
  Variable = paste(
    "Abbreviations:",
    "* Beck Depression Inventory-II;",
    "\u2020 Apathy Evaluation Scale;",
    "\u2021 <define MSS here>;",
    "\u00A7 Levodopa equivalent daily dose;",
    "\u00B6 Parkinson Neuropsychometric Dementia Assessment;",
    "** Mini-Mental State Examination"
  )
)

# Match the printed TableOne column structure
for (nm in setdiff(names(table_one_df), names(footnote))) {
  footnote[[nm]] <- NA
}
footnote <- footnote[, names(table_one_df)]

table_one_df <- dplyr::bind_rows(table_one_df, footnote)

# ---- Export to CSV-file ------------------------------------------------------

out_path <- file.path(wdir, "results", "table1.general_results.v1.1.csv")

write.csv(
  table_one_df,
  file      = out_path,
  row.names = FALSE
)

