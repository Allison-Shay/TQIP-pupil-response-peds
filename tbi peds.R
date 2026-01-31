library(stringr)
library(tidyverse)
library(dplyr)
library(readr)


trach_codes <- c("0B110F4", "0B110Z4", "0B113F4", "0B113Z4", "0B114F4", "0B114Z4")
gastro_codes <- c("0DH60UZ", "0DH63UZ", "0DH64UZ", "0DH68UZ")
crani_codes <- c("0N500ZZ","0N503ZZ","0N504ZZ","0N510ZZ","0N513ZZ","0N514ZZ","0N520ZZ","0N523ZZ","0N524ZZ","0N530ZZ","0N533ZZ","0N534ZZ","0N540ZZ","0N543ZZ","0N544ZZ","0N550ZZ","0N553ZZ","0N554ZZ","0N560ZZ","0N563ZZ","0N564ZZ","0N570ZZ","0N573ZZ","0N574ZZ","0N580ZZ","0N583ZZ","0N584ZZ",
                 "009430Z", "00943ZZ", "009440Z", "0N9000Z", "0N900ZZ", "0N9040Z", "0N904ZZ",
                 "0WJ10ZZ", "00J00ZZ", "0N800ZZ", "0N803ZZ", "0N804ZZ", "0NC10ZZ", "0NC13ZZ",
                 "0NC14ZZ", "0NC30ZZ", "0NC33ZZ", "0NC34ZZ", "0NC40ZZ", "0NC43ZZ", "0NC44ZZ",
                 "0NC50ZZ", "0NC53ZZ", "0NC54ZZ", "0NC60ZZ", "0NC63ZZ", "0NC64ZZ", "0N500ZZ",
                 "0N503ZZ", "0N504ZZ", "0NB00ZZ", "0NB03ZZ", "0NB04ZZ", "0NT10ZZ", "0NT30ZZ",
                 "0NT40ZZ", "0NT50ZZ", "0NT60ZZ", "0NT70ZZ", "009100Z", "00910ZZ", "00C10ZZ",
                 "00C13ZZ", "00C14ZZ", "00800ZZ", "00803ZZ", "00804ZZ", "00870ZZ", "00873ZZ",
                 "00874ZZ", "00590ZZ", "00593ZZ", "00594ZZ", "009900Z", "00990ZZ", "009930Z",
                 "00993ZZ", "009940Z", "00B90ZZ", "00B93ZZ", "00B94ZZ", "00C90ZZ", "00C93ZZ",
                 "00C94ZZ", "00N90ZZ", "00N93ZZ", "00N94ZZ", "00Q90ZZ", "00Q93ZZ", "00Q94ZZ",
                 "00580ZZ", "00583ZZ", "00584ZZ", "00880ZZ", "00883ZZ", "00884ZZ", "009800Z",
                 "00980ZZ", "009830Z", "00983ZZ", "009840Z", "00984ZZ", "00B80ZZ", "00B83ZZ",
                 "00B84ZZ", "00C80ZZ", "00C83ZZ", "00C84ZZ", "00510ZZ", "00513ZZ", "00514ZZ",
                 "00B10ZZ",
                 "00B13ZZ", "00B14ZZ", "00D10ZZ", "00D13ZZ", "00D14ZZ", "00T70ZZ", "00T73ZZ",
                 "00T74ZZ", "00B70ZZ", "00B73ZZ", "00B74ZZ", "00500ZZ", "00503ZZ", "00504ZZ",
                 "00B00ZZ", "00B03ZZ", "00B04ZZ", "0NS004Z", "0NS005Z", "0NS00ZZ", "0NS034Z",
                 "0NS035Z", "0NS03ZZ", "0NS044Z", "0NS045Z", "0NS04ZZ", "0NS0XZZ", "00Q20ZZ",
                 "00Q23ZZ", "00Q24ZZ", "00560ZZ", "00563ZZ", "00564ZZ", "00Q00ZZ", "00Q03ZZ",
                 "00Q04ZZ", "0NQ0XZZ",
                 "00963ZZ", "00964ZZ", "009130Z", "00913ZZ", "009140Z", "00914ZZ", "009230Z",
                 "00923ZZ", "009240Z", "00924ZZ", "4A003BD", "4A007BD", "4A103BD", "4A107BD",
                 "4A003RD", "4A007RD", "4A103RD", "4A107RD", "4A003KD", "4A007KD", "4A103KD",
                 "4A107KD", "00J03ZZ", "00J04ZZ", "00K00ZZ", "00K03ZZ", "00K04ZZ", "00K70ZZ",
                 "00K73ZZ", "00K74ZZ", "00K80ZZ", "00K83ZZ", "00K84ZZ", "00K90ZZ", "00K93ZZ",
                 "00K94ZZ", "00KA0ZZ", "00KA3ZZ", "00KA4ZZ", "00KB0ZZ", "00KB3ZZ", "00KB4ZZ",
                 "00KC0ZZ", "00KC3ZZ", "00KC4ZZ", "00KD0ZZ", "00KD3ZZ", "00KD4ZZ", "0WJ13ZZ",
                 "0WJ14ZZ", "0NJ00ZZ", "0NJ03ZZ", "0NJ04ZZ", "0WH103Z", "0WH133Z", "0WH143Z",
                 "0WP103Z", "0WP133Z", "0WP143Z", "0WP1X3Z", "00H033Z", "009600Z", "009630Z",
                 "009640Z")

# Age
agecutoff <- 18


### The directory structure of the data ###

make_path <- function(x, y) {
  sprintf("/Users/hunter/Downloads/Carilion/TQIP 2007-2023/PUF AY %s/CSV/%s", x, y)
}
### Year to analyze ###
args <- commandArgs(trailingOnly = TRUE)
year <- 2018# e.g. 2023

### The output path ###

output_path <- "final_data"

### Filtering criteria ###

# This will store number of encounters (note: not patients, because TQIP only stores encounter IDs) at each filtering step
filter_log <- data.frame(
  step = character(),
  n = integer(),
  stringsAsFactors = FALSE
)
log_step <- function(df, name, log_df) {
  rbind(
    log_df,
    data.frame(step = name, n = nrow(df), stringsAsFactors = FALSE)
  )
}

cols <- names(read.csv(make_path(year, "PUF_TRAUMA.csv"), nrows = 1))
drop <- grepl("_biu$", cols, ignore.case = TRUE)
colClasses <- ifelse(drop, "NULL", NA)
df <- read.csv(make_path(year, "PUF_TRAUMA.csv"), colClasses = colClasses)
df_ais <- read.csv(make_path(year, "PUF_AISDIAGNOSIS.csv"))
df_icdproc <- read.csv(make_path(year, "PUF_ICDPROCEDURE.csv"))
df_icddiag <- read.csv(make_path(year, "PUF_ICDDIAGNOSIS.csv"))
df_ecode <- read.csv(make_path(year, "PUF_ECODE_LOOKUP.csv"))
# lowercase column names
colnames(df) <- tolower(colnames(df))
colnames(df_ais) <- tolower(colnames(df_ais))
colnames(df_icdproc) <- tolower(colnames(df_icdproc))
colnames(df_icddiag) <- tolower(colnames(df_icddiag))
colnames(df_ecode) <- tolower(colnames(df_ecode))
filter_log <- log_step(df, "Initial cohort", filter_log)


### Filter the loaded data ###

df <- df %>% filter(ageyears < agecutoff)
filter_log <- log_step(df, "Age < 18", filter_log)

# crani code pts only
df_icdproc_crani <- df_icdproc %>%
  filter(icdprocedurecode %in% crani_codes)
crani_inc_keys <- df_icdproc_crani %>%
  pull(inc_key) %>%
  unique()

# crani_inc_keys are the inclusion keys
df_crani <- df %>%
  filter(inc_key %in% crani_inc_keys)
filter_log <- log_step(df_crani, "Craniotomy ICD-10-PCS code", filter_log)

# Merge df_crani (patient-level) with df_icdproc (procedure-level)
df_crani <- df_crani %>%
  left_join(df_icdproc, by = "inc_key")

# Collapse by inc_key
df_crani <- df_crani %>%
  group_by(inc_key) %>%
  summarise(
    # keep first for all patient-level columns except icdprocedurecode
    across(-icdprocedurecode, first),
    # collapse all ICD procedure codes into one string
    icd_procedures = paste(icdprocedurecode, collapse = "; ")
  ) %>%
  ungroup()

# Merge df_crani (patient-level) with df_icddiag
df_crani <- df_crani %>%
  left_join(df_icddiag, by = "inc_key")

# Merge df_crani with df_ecode
df_crani <- df_crani %>%
  dplyr::left_join(
    df_ecode %>% dplyr::select(ecode, intent, mechanism),
    by = c("primaryecodeicd10" = "ecode")
  )

# Collapse by inc_key
df_crani <- df_crani %>%
  group_by(inc_key) %>%
  summarise(
    # keep first for all patient-level columns except icdprocedurecode
    across(-icddiagnosiscode, first),
    
    # collapse all ICD procedure codes into one string
    icd_diagnoses = paste(icddiagnosiscode, collapse = "; ")
  ) %>%
  ungroup()

### Make relevant columns ###

#ICH codes
# Define ICH type patterns
df_crani <- df_crani %>%
  mutate(
    ich_edh = ifelse(str_detect(icd_diagnoses, "S06\\.4"), 1, 0),
    ich_sdh = ifelse(str_detect(icd_diagnoses, "S06\\.5"), 1, 0),
    ich_sah = ifelse(str_detect(icd_diagnoses, "S06\\.6"), 1, 0),
    ich_iph = ifelse(str_detect(icd_diagnoses, "S06\\.3"), 1, 0),
    ich_other = ifelse(str_detect(icd_diagnoses, "S06\\.89|S06\\.9"), 1, 0)
  )

# Count number of ICH types per patient
df_crani <- df_crani %>%
  rowwise() %>%
  mutate(
    ich_type_count = sum(c(ich_edh, ich_sdh, ich_sah, ich_iph, ich_other), na.rm = TRUE)
  ) %>%
  ungroup()

# Create a single ICH category column
df_crani <- df_crani %>%
  mutate(
    ich_category = case_when(
      ich_type_count >= 2 ~ ">=2 concomitant ICHs",
      ich_edh == 1 & ich_type_count == 1 ~ "isolated EDH",
      ich_sdh == 1 & ich_type_count == 1 ~ "isolated SDH",
      ich_sah == 1 & ich_type_count == 1 ~ "isolated SAH",
      ich_iph == 1 & ich_type_count == 1 ~ "isolated IPH",
      ich_other == 1 & ich_type_count == 1 ~ "other/unspecified ICH",
      TRUE ~ NA_character_
    )
  )

# Make minority column
race_cols <- c("americanindian", "asian", "black", 
               "pacificislander", "raceother")
df_crani$minority <- ifelse(
  df_crani$white == 1, 
  0,
  ifelse(rowSums(df_crani[race_cols]) > 0, 1, NA)
)
colnames(df_crani) <- tolower(colnames(df_crani))

# Make race column
df_crani$race <- NA_character_
df_crani$race[df_crani$minority == 0] <- "white"
idx_minority <- which(df_crani$minority == 1)
race_mat <- df_crani[idx_minority, race_cols, drop = FALSE]
race_count <- rowSums(race_mat, na.rm = TRUE)
single_idx <- race_count == 1   # Exactly one race
df_crani$race[idx_minority[single_idx]] <- race_cols[max.col(race_mat[single_idx, ], ties.method = "first")]
df_crani$race[idx_minority[race_count > 1]] <- "multiple"   # Multiple races

#Make pupillometry column binary #QUONK this is the only edit I would be concerned about in this file
df_crani$pupils <- NA_character_
df_crani$pupils[df_crani$tbipupillaryresponse == 3] <-"ABPR"
df_crani$pupils[df_crani$tbipupillaryresponse<3]<-"PPR"

### Finalize our column names (renaming columns if necessary since different years have different column names) ###

# Rename certain variables 
if ("iss_05" %in% names(df_crani) && !"iss" %in% names(df_crani)) {
  names(df_crani)[names(df_crani) == "iss_05"] <- "iss"
}
if ("losdays" %in% names(df_crani) && !"finaldischargedays" %in% names(df_crani)) {
  names(df_crani)[names(df_crani) == "losdays"] <- "finaldischargedays"
}
if ("cerebralmonitordays" %in% names(df_crani) && !"tbicerebralmonitordays" %in% names(df_crani)) {
  names(df_crani)[names(df_crani) == "cerebralmonitordays"] <- "tbicerebralmonitordays"
}
if ("proceduredays" %in% names(df_crani) && !"hospitalprocedurestartdays" %in% names(df_crani)) {
  names(df_crani)[names(df_crani) == "proceduredays"] <- "hospitalprocedurestartdays"
}

### Optional: Load and output the ICD code descriptions (this is done by supplying ICD on the command-line) ###

# ---- Optional ICD lookup export ----
if (length(args) >= 2 && args[2] == "ICD") {
  
  # Read lookup tables (skip header, keep first 2 columns only)
  proc_lookup <- read.csv(
    make_path(year, "PUF_ICDPROCEDURE_LOOKUP.csv"),
    skip = 1,
    header = FALSE,
    stringsAsFactors = FALSE
  )[, 1:2]
  
  diag_lookup <- read.csv(
    make_path(year, "PUF_ICDDIAGNOSIS_LOOKUP.csv"),
    skip = 1,
    header = FALSE,
    stringsAsFactors = FALSE
  )[, 1:2]
  
  colnames(proc_lookup) <- colnames(diag_lookup) <- c("code", "description")
  
  proc_lookup$code <- trimws(proc_lookup$code)
  diag_lookup$code <- trimws(diag_lookup$code)
  
  # ---- Procedure sections ----
  crani_rows  <- proc_lookup[proc_lookup$code %in% crani_codes, ]
  gastro_rows <- proc_lookup[proc_lookup$code %in% gastro_codes, ]
  trach_rows  <- proc_lookup[proc_lookup$code %in% trach_codes, ]
  
  # ---- ICH diagnosis sections (ACTUAL ICD CODES) ----
  ich_edh <- diag_lookup[str_detect(diag_lookup$code, "^S06\\.4"), ]
  ich_sdh <- diag_lookup[str_detect(diag_lookup$code, "^S06\\.5"), ]
  ich_sah <- diag_lookup[str_detect(diag_lookup$code, "^S06\\.6"), ]
  ich_iph <- diag_lookup[str_detect(diag_lookup$code, "^S06\\.3"), ]
  ich_other <- diag_lookup[str_detect(diag_lookup$code, "^S06\\.89|^S06\\.9"), ]
  
  out_file <- paste(output_path, "/", "ICD_codes.csv", sep = "")
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  con <- file(out_file, open = "wt")
  
  write_section <- function(title, df) {
    writeLines(title, con)
    writeLines("code,description", con)
    if (nrow(df) > 0) {
      write.table(df, con, sep = ",", row.names = FALSE,
                  col.names = FALSE, quote = TRUE)
    }
    writeLines("", con)
  }
  
  write_section("Cranial procedure codes", crani_rows)
  write_section("Gastrostomy procedure codes", gastro_rows)
  write_section("Tracheostomy procedure codes", trach_rows)
  
  write_section("ICH type: epidural hematoma", ich_edh)
  write_section("ICH type: subdural hematoma", ich_sdh)
  write_section("ICH type: subarachnoid hemorrhage", ich_sah)
  write_section("ICH type: intraparenchymal hemorrhage", ich_iph)
  write_section("ICH type: other", ich_other)
  
  close(con)
  
  message("Wrote ICD codes to: ", out_file)
}



### Finally, subset our data frame to only contain our columns of interest ###

df_crani <- df_crani %>%
  select(
    inc_key,
    sex,
    ageyears,
    minority,
    race,
    ethnicity,
    tbipupillaryresponse,
    pupils,
    primarymethodpayment,
    verificationlevel,
    teachingstatus,
    hospitaltype,
    intent,
    mechanism,
    totalgcs,
    iss,
    totalventdays,
    totaliculos,
    finaldischargedays,
    hospdischargedisposition,
    icpparench,
    icpevdrain,
    icd_procedures,
    icd_diagnoses,
    ich_category,
    withdrawallst,
    withdrawallstdays,
    statedesignation,
    tbicerebralmonitordays,
    tbimidlineshift,
    hospitalprocedurestartdays,
    interfacilitytransfer,
    eddischargedisposition
  )


### Make a trach and gastro column based on ICD procedure codes ###

df_crani <- df_crani %>%
  mutate(trach = ifelse(
    str_detect(icd_procedures, str_c(trach_codes, collapse = "|")),
    1, 0
  ))

df_crani <- df_crani %>%
  mutate(gastro = ifelse(
    str_detect(icd_procedures, str_c(gastro_codes, collapse = "|")),
    1, 0
  ))

### Make columns based on injury code (AIS) ###

# Head injuries only
df_head <- df_ais %>% filter(str_detect(aispredot, "^1"))
df_nonhead <- df_ais %>% filter(!str_detect(aispredot, "^1"))

# Max AIS per patient
ais_head <- df_head %>%
  group_by(inc_key) %>%
  summarise(
    ais_head = if (all(is.na(aisseverity))) NA_real_
    else max(aisseverity, na.rm = TRUE),
    .groups = "drop"
  )

ais_nonhead <- df_nonhead %>%
  group_by(inc_key) %>%
  summarise(
    ais_nonhead = if (all(is.na(aisseverity))) NA_real_
    else max(aisseverity, na.rm = TRUE),
    .groups = "drop"
  )

# Merge both into one summary
ais_summary <- ais_head %>%
  full_join(ais_nonhead, by = "inc_key")

df_crani <- df_crani %>%
  left_join(ais_summary, by = "inc_key")

### Filter based on injury codes ###

# Patients with isolated blunt TBI defined as patients with any Head-Abbreviated Injury Scale (AIS) and non-Head AIS score <3.

isolated_tbi <- df_crani %>%
  mutate(ais_nonhead = ifelse(is.na(ais_nonhead), 0, ais_nonhead)) %>%
  filter(ais_head > 1 & ais_nonhead < 3)
filter_log <- log_step(isolated_tbi, "Isolated blunt TBI: AIS head > 1 & AIS non-head < 3", filter_log)

### Make dataset consistent across years ###

# for older years, change teaching status 0 = nonteaching 1= university
# --- Clean & convert teachingstatus for isolated_tbi only ---
if ("teachingstatus" %in% names(isolated_tbi)) {
  ts <- isolated_tbi$teachingstatus |>
    as.character() |>
    tolower() |>
    trimws()
  
  isolated_tbi$teachingstatus <- dplyr::case_when(
    ts %in% c("academic", "university") ~ 1L,
    ts %in% c("community", "nonteaching") ~ 0L,
    ts %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9") ~ as.integer(ts),
    TRUE ~ NA_integer_
  )
}

#for 2021 after teaching status (academic is 1, others are 0)
isolated_tbi$teachingstatus <- ifelse(isolated_tbi$teachingstatus == 1, 1, 0)

isolated_tbi$year <- year

if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

write_csv(isolated_tbi, paste(output_path, "/", year, "cleaned.csv", sep=""))
write_csv(filter_log, paste(output_path, "/", year, "filtering_summary.csv", sep=""))

######## part 2
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

input_dir <- "final_data"
output_dir <- "final_data_merged"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# Merge filtering_summary.csv files (sum by step)
# -----------------------------
summary_files <- list.files(
  input_dir,
  pattern = "filtering_summary\\.csv$",
  full.names = TRUE
)

if (length(summary_files) == 0) {
  message("No *filtering_summary.csv files found in ", input_dir)
} else {
  summaries <- lapply(summary_files, function(f) {
    read_csv(f, show_col_types = FALSE)
  })
  
  step_order <- summaries[[1]]$step
  
  summary_all <- bind_rows(summaries)
  
  if (!("step" %in% names(summary_all))) {
    stop("Merged filtering summaries do not contain a 'step' column.")
  }
  
  numeric_cols <- names(summary_all)[vapply(summary_all, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, "step")
  
  if (length(numeric_cols) == 0) {
    stop("No numeric columns found to sum in filtering summaries (expected e.g. 'n').")
  }
  
  summary_merged <- summary_all %>%
    group_by(step) %>%
    summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  summary_merged <- summary_merged %>%
    mutate(step = factor(step, levels = step_order)) %>%
    arrange(step) %>%
    mutate(step = as.character(step))
  
  out_summary <- file.path(output_dir, "filtering_summary.csv")
  write_csv(summary_merged, out_summary)
  message("Wrote merged filtering summary to: ", out_summary)
}

# -----------------------------
# Merge cleaned.csv files (row-bind)
# -----------------------------
cleaned_files <- list.files(
  input_dir,
  pattern = "cleaned\\.csv$",
  full.names = TRUE
)

if (length(cleaned_files) == 0) {
  message("No *cleaned.csv files found in ", input_dir)
} else {
  # Read first file to define canonical column names and order
  first_df <- read_csv(cleaned_files[1], show_col_types = FALSE)
  canonical_names <- names(first_df)
  
  dfs <- vector("list", length(cleaned_files))
  dfs[[1]] <- first_df
  
  if (length(cleaned_files) > 1) {
    for (i in 2:length(cleaned_files)) {
      df_i <- read_csv(cleaned_files[i], show_col_types = FALSE)
      
      if (!identical(names(df_i), canonical_names)) {
        stop(
          "Column names/order mismatch in file: ", cleaned_files[i], "\n",
          "Expected: ", paste(canonical_names, collapse = ", "), "\n",
          "Got:      ", paste(names(df_i), collapse = ", ")
        )
      }
      
      dfs[[i]] <- df_i
    }
  }
  
  cleaned_merged <- bind_rows(dfs)
  
  out_cleaned <- file.path(output_dir, "cleaned.csv")
  write_csv(cleaned_merged, out_cleaned)
  message("Wrote merged cleaned data to: ", out_cleaned)
}
