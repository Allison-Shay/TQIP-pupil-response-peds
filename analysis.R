library(stringr)
library(tidyverse)
library(dplyr)
library(MatchIt)  # For propensity matching
library(readr)
library(cobalt) # For post-match balance checks
library(sandwich)
library(lmtest)
library(dplyr)
library(purrr)
library(tibble)
library(estimatr)


###### File paths #########

fname <- "final_data_merged/cleaned.csv"
output_dir <- "analysis"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

###### Read in the data ######

raw_df <- read.csv(fname)

###### Establish pupillary response variables #######

raw_df$pupils <- NA_character_
raw_df$pupils[raw_df$tbipupillaryresponse == 3] <-"ABPR"
raw_df$pupils[raw_df$tbipupillaryresponse == 1] <- "PPR"
df <- raw_df

###### Create a data summary table #######

transform_analysis_vars <- function(df) {

  # Safety check
  required_cols <- c(
    "ageyears", "iss", "minority", "sex", "teachingstatus", "verificationlevel",
    "trach", "gastro", "icpparench", "icpevdrain",
    "withdrawallst", "tbimidlineshift", "ich_category",
    "statedesignation", "hospdischargedisposition", "totalgcs",
    "race", "ethnicity", "primarymethodpayment", "hospitaltype", "intent", "mechanism",
    "pupils", "tbipupillaryresponse"
  )

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  df %>%
    mutate(
      # Core demographics
      minority = factor(minority, levels = c(0, 1),
                        labels = c("White", "Minority")),
      sex = factor(sex, levels = c(1, 2),
                   labels = c("Male", "Female")),
      age_cat = case_when(
        ageyears >= 0 & ageyears <= 2  ~ "Age 0-2 years",
        ageyears > 2 & ageyears <= 5 ~ "Age 3-5 years",
        ageyears > 5 & ageyears <= 9 ~ "Age 6-9 years",
        ageyears > 9 & ageyears <= 12 ~ "Age 10-12 years",
        ageyears > 12 ~ "Age >12 years",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Age 18-30 years", "Age 31-45 years", "Age 46-60 years", "Age 61-75 years", "Age >75 years")),
      race = factor(race, levels = c("white", "americanindian", "asian", "black", "pacificislander", "raceother", "multiple"),
                        labels = c("White", "American Indian", "Asian", "Black", "Pacific Islander", "Other", "Multiple")), 
      ethnicity = factor(ethnicity, levels = c(1, 2),
                   labels = c("Hispanic/Latino", "Non-Hispanic/Latino")),

      # Hospital characteristics
      teachingstatus = factor(teachingstatus),
      verificationlevel = factor(verificationlevel),
      hospitaltype = case_when(
        hospitaltype == 1  ~ "For profit",
        hospitaltype == 2  ~ "Non-profit",
        hospitaltype == 3  ~ "Government",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("For profit", "Non-profit", "Government")),

      # Insurance
      primarymethodpayment = case_when(
        primarymethodpayment == 1  ~ "Medicaid",
        primarymethodpayment == 2  ~ "Other",
        primarymethodpayment == 3  ~ "Self-pay",
        primarymethodpayment == 4  ~ "Private insurance",
        primarymethodpayment == 6  ~ "Medicare",
        primarymethodpayment == 7  ~ "Other",
        primarymethodpayment == 10  ~ "Other",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Medicaid", "Other", "Self-pay", "Private insurance", "Medicare")),

      # Procedures
      trach  = factor(trach, levels = c(0, 1), labels = c("No", "Yes")),
      gastro = factor(gastro, levels = c(0, 1), labels = c("No", "Yes")),
      icpparench = factor(icpparench, levels = c(0, 1), labels = c("No", "Yes")),
      icpevdrain = factor(icpevdrain, levels = c(0, 1), labels = c("No", "Yes")),

      # Outcomes / clinical decisions
      withdrawallst = factor(withdrawallst,
                             levels = c(1, 2),
                             labels = c("Yes", "No")),

      # Injury mechanism
      intent = case_when(
        intent == 1  ~ "Unintentional",
        intent == 2  ~ "Self-inflicted",
        intent == 3  ~ "Assault",
        intent == 4  ~ "Undetermined",
        intent == 5  ~ "Other",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Unintentional", "Self-inflicted", "Assault", "Undetermined", "Other")),
      mechanism = case_when(
        mechanism == 1 ~ "Other",
        mechanism == 2 ~ "Other",
        mechanism == 3  ~ "Fall",
        mechanism == 4 ~ "Other",
        mechanism == 5 ~ "Other",
        mechanism == 6  ~ "Firearm",
        mechanism == 7  ~ "Machinery",
        mechanism >= 8 & mechanism <= 13  ~ "Motor vehicle traffic injury",
        mechanism >= 14 & mechanism <= 16  ~ "Nontraffic transportation injury",
        mechanism >= 17 & mechanism <= 20  ~ "Other",
        mechanism == 21  ~ "Struck by/against",
        mechanism >= 22  ~ "Other",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Other", "Fall", "Firearm", "Machinery", "Motor vehicle traffic injury", "Nontraffic transportation injury", "Struck by/against")),

      # Transfer
      interfacilitytransfer = factor(interfacilitytransfer,
                             levels = c(1, 2),
                             labels = c("Yes", "No")),
      eddischargedisposition = case_when(
        eddischargedisposition == 11 ~ "ED transfer",
        eddischargedisposition != 11  ~ "Not ED transfer",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("ED transfer", "Not ED transfer")),
      
      # Injury characteristics
      tbimidlineshift = case_when(
        tbimidlineshift == 1 ~ "Yes",
        tbimidlineshift == 2 ~ "No",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Yes", "No")),
      tbipupillaryresponse = case_when(
        tbipupillaryresponse == 1 ~ "Both reactive",
        tbipupillaryresponse == 2 ~ "One reactive",
        tbipupillaryresponse == 3 ~ "Neither reactive",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Both reactive", "One reactive", "Neither reactive")),
      gcs_cat = case_when(
        !is.na(totalgcs) & totalgcs <= 8  ~ "Severe",
        !is.na(totalgcs) & totalgcs <= 12 ~ "Moderate",
        !is.na(totalgcs)                  ~ "Mild",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Mild", "Moderate", "Severe")),
      iss_cat = case_when(
        iss < 12  ~ "ISS < 12",
        iss >= 12 & iss <= 24 ~ "ISS 12-24",
        iss > 24 ~ "ISS > 24",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("ISS < 12", "ISS 12-24", "ISS > 24")),
      ich_category = factor(ich_category),
      statedesignation = factor(statedesignation),
      hospdischargedisposition = factor(hospdischargedisposition)
    )
}

df <- transform_analysis_vars(df)


max_levels_to_print <- 10  # threshold for printing categories

summary_df <- data.frame(
  column = character(),
  type = character(),
  na_count = integer(),
  value_summary = character(),
  stringsAsFactors = FALSE
)

for (col_name in names(df)) {
  x <- df[[col_name]]
  n_na <- sum(is.na(x))
  x_no_na <- x[!is.na(x)]

  # Numerical
  if (is.numeric(x)) {
    type <- "numerical"
    if (length(x_no_na) == 0) {
      value_summary <- "all values are NA"
    } else {
      value_summary <- paste0(
        min(x_no_na), " to ", max(x_no_na)
      )
    }

  # Categorical
  } else {
    type <- "categorical"
    levels <- unique(x_no_na)
    n_levels <- length(levels)

    if (n_levels <= max_levels_to_print) {
      value_summary <- paste(levels, collapse = ", ")
    } else {
      value_summary <- paste(n_levels, "categories")
    }
  }

  summary_df <- rbind(
    summary_df,
    data.frame(
      column = col_name,
      type = type,
      na_count = n_na,
      value_summary = value_summary,
      stringsAsFactors = FALSE
    )
  )
}

####### Print out the data summary table #######

write.csv(summary_df[order(summary_df$type, decreasing=TRUE),], file.path(output_dir, "raw_data_summary.csv"), row.names=FALSE)
cat("Rows:", nrow(df), "\nColumns:", ncol(df), "\n")

####### Filter the data based on criteria of excluding missing/bad vaules ########

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
log_step_n <- function(num, name, log_df) {
  rbind(
    log_df,
    data.frame(step = name, n = num, stringsAsFactors = FALSE)
  )
}
filter_log <- log_step(raw_df, "Raw", filter_log)


data <- raw_df

apply_analytic_filters <- function(data_frame) {
  data_frame %>%
  filter(!is.na(minority))%>%
  filter(!is.na(pupils))%>%
  filter(!is.na(tbipupillaryresponse), tbipupillaryresponse %in% c(1,2,3))%>%
  filter(!is.na(sex), sex %in% c(1,2))%>%
  filter(!is.na(teachingstatus))%>%
  filter(!is.na(verificationlevel))%>%
  filter(!is.na(totalgcs))%>%
  filter(!is.na(iss))%>%
  filter(!is.na(tbimidlineshift), tbimidlineshift %in% c(1,2))%>%
  filter(!is.na(statedesignation), statedesignation %in% c(1,2,3,4,6))%>% # 5 doesn't exist for statedesignation and 7 = "not applicable"
  # filter(!is.na(hospdischargedisposition))%>%   # Don't exclude upfront (avoid selection bias)
  # filter(!is.na(withdrawallst))%>%              # Don't exclude upfront
  filter(!is.na(ich_category))
}

data_analytic <- apply_analytic_filters(data)
data_analytic_mod <- transform_analysis_vars(data_analytic) # Simply change things from numerical to things like Yes/No Male/Female etc.

filter_log <- log_step(data_analytic, "Filtered (removed missing/bad values)", filter_log)

######## Run statistical analysis to compare groups ########

group_var <- "pupils"

exclude_vars <- character(0)

# ---- Variable lists from summary_df ----
num_vars <- summary_df %>%
  filter(type == "numerical") %>%
  pull(column) %>%
  setdiff(c("inc_key")) %>%
  setdiff(exclude_vars)

cat_vars <- summary_df %>%
  filter(type == "categorical") %>%
  pull(column) %>%
  setdiff(c(group_var)) %>%
  setdiff(exclude_vars)

# ---- Test helpers ----
test_numeric_wilcox <- function(df, var, group) {
  x <- df[[var]]
  g <- df[[group]]
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tryCatch(wilcox.test(x ~ g)$p.value, error = function(e) NA_real_)
}

test_categorical_chisq <- function(df, var, group) {
  x <- df[[var]]
  g <- df[[group]]
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tbl <- table(x, g)
  if (any(dim(tbl) < 2)) return(NA_real_)
  tryCatch(chisq.test(tbl)$p.value, error = function(e) NA_real_)
}

# ---- Run tests ----
results_num <- map_dfr(num_vars, function(v) {
  tibble(
    variable = v,
    type = "continuous",
    n_nonmissing = sum(!is.na(data_analytic_mod[[v]]) & !is.na(data_analytic_mod[[group_var]])),
    p_value = test_numeric_wilcox(data_analytic_mod, v, group_var)
  )
})

results_cat <- map_dfr(cat_vars, function(v) {
  # also record table size so you can spot "ICD-like" variables if you didn't exclude them
  x <- data_analytic_mod[[v]]
  g <- data_analytic_mod[[group_var]]
  ok <- !is.na(x) & !is.na(g)
  tbl <- table(x[ok], g[ok])
  tibble(
    variable = v,
    type = "categorical",
    n_nonmissing = sum(ok),
    n_levels = nrow(tbl),
    table_cells = prod(dim(tbl)),
    p_value = test_categorical_chisq(data_analytic_mod, v, group_var)
  )
})

table1_pvals <- bind_rows(results_num, results_cat) %>%
  mutate(
    p_value = ifelse(is.na(p_value), NA, signif(p_value, 3)),
    note = case_when(
      type == "categorical" & !is.na(table_cells) & table_cells > 20000 ~ "very large contingency table",
      TRUE ~ ""
    )
  ) %>%
  arrange(type, p_value)

write.csv(table1_pvals, file.path(output_dir, "raw_data_statistics.csv"), row.names=FALSE)


######## Run more detailed statistical analyses ############

group_var <- "pupils"
exclude_vars <- character(0)

# ---- settings ----
max_cat_levels_to_print <- 30   # cap printed levels per categorical var (prevents 50k ICD rows)
show_other_bucket <- TRUE       # if TRUE, collapses remaining levels into "Other"
digits_cont <- 2

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) "<0.001" else sprintf("%.3f", p)
}

fmt_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)


# ---- group labels and sizes ----
g_all <- data_analytic_mod[[group_var]]
g_levels <- sort(unique(na.omit(g_all)))
if (length(g_levels) != 2) stop("group_var must have exactly 2 non-NA levels", call. = FALSE)

g1 <- g_levels[1]
g2 <- g_levels[2]

N_total <- nrow(data_analytic_mod)
N_g1 <- sum(g_all == g1, na.rm = TRUE)
N_g2 <- sum(g_all == g2, na.rm = TRUE)

# ---- header rows (overall + group counts) ----
header_rows <- tibble(
  variable = c("N (encounters)", paste0("N: ", group_var, " = ", g1), paste0("N: ", group_var, " = ", g2)),
  level = "",
  overall = c(as.character(N_total), "", ""),
  group1 = c("", as.character(N_g1), ""),
  group2 = c("", "", as.character(N_g2)),
  p_value = "",
  missing = "",
  type = "header",
  note = ""
)

# ---- continuous summaries ----
summarise_continuous <- function(df, var, group, g1, g2) {
  x <- df[[var]]
  g <- df[[group]]

  overall_nmiss <- sum(is.na(x) | is.na(g))
  # (missing for this row = missing x OR missing group assignment)
  # If you prefer only missing x, change to sum(is.na(x))

  x_all <- x[!is.na(x) & !is.na(g)]
  x_g1 <- x[g == g1 & !is.na(x)]
  x_g2 <- x[g == g2 & !is.na(x)]

  p <- test_numeric_wilcox(df, var, group)

  tibble(
    variable = var,
    level = "",
    overall = if (length(x_all) == 0) NA_character_ else paste0(
      fmt_num(mean(x_all), digits_cont), " (", fmt_num(sd(x_all), digits_cont), "); ",
      "median ", fmt_num(median(x_all), digits_cont), " [", fmt_num(IQR(x_all), digits_cont), "]; ",
      "range ", fmt_num(min(x_all), digits_cont), "–", fmt_num(max(x_all), digits_cont)
    ),
    group1 = if (length(x_g1) == 0) NA_character_ else paste0(
      fmt_num(mean(x_g1), digits_cont), " (", fmt_num(sd(x_g1), digits_cont), "); ",
      "median ", fmt_num(median(x_g1), digits_cont), " [", fmt_num(IQR(x_g1), digits_cont), "]; ",
      "range ", fmt_num(min(x_g1), digits_cont), "–", fmt_num(max(x_g1), digits_cont)
    ),
    group2 = if (length(x_g2) == 0) NA_character_ else paste0(
      fmt_num(mean(x_g2), digits_cont), " (", fmt_num(sd(x_g2), digits_cont), "); ",
      "median ", fmt_num(median(x_g2), digits_cont), " [", fmt_num(IQR(x_g2), digits_cont), "]; ",
      "range ", fmt_num(min(x_g2), digits_cont), "–", fmt_num(max(x_g2), digits_cont)
    ),
    p_value = fmt_p(p),
    missing = as.character(sum(is.na(x))),
    type = "continuous",
    note = ""
  )
}

cont_rows <- map_dfr(num_vars, ~ summarise_continuous(data_analytic_mod, .x, group_var, g1, g2))

# ---- categorical summaries ----
# helper to optionally collapse to top levels
collapse_levels <- function(x, max_levels, show_other) {
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  tab <- sort(table(x, useNA = "no"), decreasing = TRUE)
  levs <- names(tab)

  if (length(levs) <= max_levels) return(factor(x, levels = levs))

  keep <- levs[seq_len(max_levels)]
  if (show_other) {
    x2 <- ifelse(x %in% keep, x, "Other")
    factor(x2, levels = c(keep, "Other"))
  } else {
    x2 <- ifelse(x %in% keep, x, NA_character_)
    factor(x2, levels = keep)
  }
}

summarise_categorical <- function(df, var, group, g1, g2, max_levels, show_other) {
  x_raw <- df[[var]]
  g <- df[[group]]

  # Collapse to top levels for display (prevents gigantic tables)
  x <- collapse_levels(x_raw, max_levels, show_other)

  p <- test_categorical_chisq(
    df %>% mutate(!!var := x),
    var,
    group
  )

  # For n (%) by group, denominator is non-missing group within that group
  denom_g1 <- sum(g == g1, na.rm = TRUE)
  denom_g2 <- sum(g == g2, na.rm = TRUE)

  # Overall denominator = all rows (or non-missing group) – choose what you prefer
  denom_all <- sum(!is.na(g))

  levels_x <- levels(x)
  out <- map_dfr(levels_x, function(lv) {
    n_all <- sum(x == lv, na.rm = TRUE)
    n1 <- sum(x == lv & g == g1, na.rm = TRUE)
    n2 <- sum(x == lv & g == g2, na.rm = TRUE)

    tibble(
      variable = var,
      level = lv,
      overall = paste0(n_all, " (", fmt_num(100 * n_all / denom_all, 1), "%)"),
      group1 = paste0(n1, " (", fmt_num(100 * n1 / denom_g1, 1), "%)"),
      group2 = paste0(n2, " (", fmt_num(100 * n2 / denom_g2, 1), "%)"),
      p_value = "",  # put p-value only on the variable header row below
      missing = "",
      type = "categorical_level",
      note = ""
    )
  })

  # add a variable header row that carries the p-value + missingness
  header <- tibble(
    variable = var,
    level = "",
    overall = "",
    group1 = "",
    group2 = "",
    p_value = fmt_p(p),
    missing = as.character(sum(is.na(x_raw))),
    type = "categorical",
    note = if (length(unique(na.omit(x_raw))) > max_levels) {
      paste0("shown top ", max_levels, if (show_other) " + Other" else "")
    } else ""
  )

  bind_rows(header, out)
}

cat_rows <- map_dfr(cat_vars, ~ summarise_categorical(
  data_analytic_mod, .x, group_var, g1, g2, max_cat_levels_to_print, show_other_bucket
))

# ---- final Table 1 ----
table1 <- bind_rows(header_rows, cont_rows, cat_rows) %>%
  rename(
    !!paste0(group_var, "=", g1) := group1,
    !!paste0(group_var, "=", g2) := group2
  )

write.csv(table1, file.path(output_dir, "raw_data_statistics_detailed.csv"), row.names=FALSE)


######## Make some new data variables and factorize some existing ones ########


data_analytic <- data_analytic %>%
  mutate(
    icpparench = ifelse(is.na(icpparench), 0, icpparench),
    icpevdrain = ifelse(is.na(icpevdrain), 0, icpevdrain),
    trach      = ifelse(is.na(trach), 0, trach),
    gastro     = ifelse(is.na(gastro), 0, gastro),
    neurosurg_any = ifelse(icpparench == 1 | icpevdrain == 1, 1, 0),
  )

data_analytic <- data_analytic %>%
  mutate(
    sex              = factor(sex),                      # 0/1 or M/F -> factor
    minority         = as.integer(minority),             # 1 = minority, 0 = NHW
    verificationlevel = factor(verificationlevel),
    teachingstatus   = factor(teachingstatus),
    pupils = factor(
      pupils,
      levels = c("PPR", "ABPR"),
      labels = c("At least one reactive", "Absent bilateral pupillary response")
    )
  )

data_analytic <- data_analytic %>%
  mutate(
    mort_inhospital = factor(case_when(
      hospdischargedisposition == 5  ~ 1L,
      hospdischargedisposition %in% c(1:99) ~ 0L,  # other valid codes
      TRUE ~ NA_integer_
    )),
    ltc_inhospital = factor(case_when(
      hospdischargedisposition == 12 ~ 1L,
      hospdischargedisposition %in% c(1:99) ~ 0L,
      TRUE ~ NA_integer_
    )),
    withdrawallst_bin = factor(case_when(
      withdrawallst == 2 ~ 0L,
      withdrawallst == 1 ~ 1L,
      TRUE ~ NA_integer_
    ))
)

######## Propensity matching ########


ps_formula <- pupils ~ ageyears + sex + iss +
  tbimidlineshift + ich_category +
  teachingstatus + verificationlevel +
  statedesignation + minority

    
data_ps <- transform_analysis_vars(data_analytic)

m.out <- matchit(
  ps_formula,
  data = data_ps,
  method = "nearest",
  ratio = 1,
  caliper = 0.2,
  exact = ~ gcs_cat     # <-- THIS IS NEEDED TO PROPENSITY MATCH GCS
)

matched <- match.data(m.out)

######## Display the propensity matching results ########


covar_labels <- c(
  distance = "Propensity score",
  ageyears = "Age (years)",
  sex_Female = "Sex",
  gcs_cat_Mild = "GCS: Mild (13–15)",
  gcs_cat_Moderate = "GCS: Moderate (9–12)",
  gcs_cat_Severe = "GCS: Severe (3-8)",
  iss = "Injury severity score (ISS)",
  tbimidlineshift_No = "Midline shift present",
  `ich_category_isolated SDH` = "Isolated SDH",
  `ich_category_isolated EDH` = "Isolated EDH",
  `ich_category_isolated SAH` = "Isolated SAH",
  `ich_category_isolated IPH` = "Isolated IPH",
  `ich_category_other/unspecified ICH` = "Isolated other/unspecified ICH",
  `ich_category_>=2 concomitant ICHs` = "Multiple concomitant ICH types",
  teachingstatus = "Hospital teaching status",
  verificationlevel_1 = "ACS-verified trauma level I",
  verificationlevel_2 = "ACS-verified trauma level II",
  verificationlevel_3 = "ACS-verified trauma level III",
  statedesignation_1 = "State trauma designation level I",
  statedesignation_2 = "State trauma designation level II",
  statedesignation_3 = "State trauma designation level III",
  statedesignation_4 = "State trauma designation level IV",
  statedesignation_6 = "State trauma designation level—other",
  minority_Minority = "Minority status"
)


    
out <- capture.output(summary(m.out))
writeLines(out, file.path(output_dir, "matchit_summary.txt"))
pdf(file.path(output_dir, "love_plot.pdf"), width = 7, height = 5)
love.plot(
  m.out,
  stats = "mean.diffs",
  abs = TRUE,
  threshold = 0.1,
  var.order = NULL,
  standardize = TRUE,
  binary = "std",
  colors = c("grey60", "black"),
  shapes = c(17, 16),
  var.names = covar_labels
)
dev.off()


####### Re-run statistics, this time for matched data ##########

make_summary_df <- function(df, max_levels_to_print = 10) {
  summary_df <- data.frame(
    column = character(),
    type = character(),
    na_count = integer(),
    value_summary = character(),
    stringsAsFactors = FALSE
  )
  for (col_name in names(df)) {
    x <- df[[col_name]]
    n_na <- sum(is.na(x))
    x_no_na <- x[!is.na(x)]
    if (is.numeric(x)) {
      type <- "numerical"
      value_summary <- if (length(x_no_na) == 0) "all values are NA" else paste0(min(x_no_na), " to ", max(x_no_na))
    } else {
      type <- "categorical"
      levs <- unique(x_no_na)
      n_levels <- length(levs)
      value_summary <- if (n_levels <= max_levels_to_print) paste(levs, collapse = ", ") else paste(n_levels, "categories")
    }
    summary_df <- rbind(
      summary_df,
      data.frame(column = col_name, type = type, na_count = n_na, value_summary = value_summary, stringsAsFactors = FALSE)
    )
  }
  summary_df
}

matched_summary_df <- make_summary_df(matched, max_levels_to_print = 10)
write.csv(matched_summary_df[order(matched_summary_df$type, decreasing=TRUE),],
          file.path(output_dir, "matched_data_summary.csv"), row.names=FALSE)

group_var <- "pupils"

exclude_vars <- c(
  "distance", "weights", "subclass"  # MatchIt columns
)

num_vars_matched <- matched_summary_df %>%
  filter(type == "numerical") %>%
  pull(column) %>%
  setdiff(exclude_vars)

cat_vars_matched <- matched_summary_df %>%
  filter(type == "categorical") %>%
  pull(column) %>%
  setdiff(c(group_var)) %>%
  setdiff(exclude_vars)

results_num_m <- map_dfr(num_vars_matched, function(v) {
  tibble(
    variable = v,
    type = "continuous",
    n_nonmissing = sum(!is.na(matched[[v]]) & !is.na(matched[[group_var]])),
    p_value = test_numeric_wilcox(matched, v, group_var)
  )
})

results_cat_m <- map_dfr(cat_vars_matched, function(v) {
  x <- matched[[v]]
  g <- matched[[group_var]]
  ok <- !is.na(x) & !is.na(g)
  tbl <- table(x[ok], g[ok])
  tibble(
    variable = v,
    type = "categorical",
    n_nonmissing = sum(ok),
    n_levels = nrow(tbl),
    table_cells = prod(dim(tbl)),
    p_value = test_categorical_chisq(matched, v, group_var)
  )
})

matched_table1_pvals <- bind_rows(results_num_m, results_cat_m) %>%
  mutate(
    p_value = ifelse(is.na(p_value), NA, signif(p_value, 3)),
    note = case_when(
      type == "categorical" & !is.na(table_cells) & table_cells > 20000 ~ "very large contingency table",
      TRUE ~ ""
    )
  ) %>%
  arrange(type, p_value)

write.csv(matched_table1_pvals, file.path(output_dir, "matched_data_statistics.csv"), row.names=FALSE)

g_all <- matched[[group_var]]
g_levels <- sort(unique(na.omit(g_all)))
g1 <- g_levels[1]
g2 <- g_levels[2]

# header rows
N_total <- nrow(matched)
N_g1 <- sum(g_all == g1, na.rm = TRUE)
N_g2 <- sum(g_all == g2, na.rm = TRUE)

header_rows <- tibble(
  variable = c("N (encounters)", paste0("N: ", group_var, " = ", g1), paste0("N: ", group_var, " = ", g2)),
  level = "",
  overall = c(as.character(N_total), "", ""),
  group1 = c("", as.character(N_g1), ""),
  group2 = c("", "", as.character(N_g2)),
  p_value = "",
  missing = "",
  type = "header",
  note = ""
)

cont_rows <- map_dfr(num_vars_matched, ~ summarise_continuous(matched, .x, group_var, g1, g2))

cat_rows <- map_dfr(cat_vars_matched, ~ summarise_categorical(
  matched, .x, group_var, g1, g2, max_cat_levels_to_print, show_other_bucket
))

table1_matched <- bind_rows(header_rows, cont_rows, cat_rows) %>%
  rename(
    !!paste0(group_var, "=", g1) := group1,
    !!paste0(group_var, "=", g2) := group2
  )

write.csv(table1_matched, file.path(output_dir, "matched_data_statistics_detailed.csv"), row.names=FALSE)




####### Prepare for regression analysis ########

# For regressions where we're going to exclude NA's

matched_mort <- matched %>%
  filter(!is.na(mort_inhospital))

matched_ltc <- matched %>%
  filter(!is.na(ltc_inhospital))

matched_wlt <- matched %>%
  filter(!is.na(withdrawallst_bin))

# Helper function: clustered-robust SE by subclass

cluster_se <- function(model, cluster) {
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M / (M - 1)) * ((N - 1) / (N - K))  # finite-sample correction
  
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  vcovCL <- sandwich::vcovCL(model, cluster = cluster)    #   dfc * sandwich(model, meat. = crossprod(uj) / N)
  list(coef = coef(model), vcov = vcovCL)
} 


####### Regression analyses: Risk ratio #########


run_rr_cluster <- function(formula, data, cluster) {
  mf <- model.frame(formula, data = data, na.action = na.omit)

  cl <- cluster
  names(cl) <- rownames(data)
  cl_used <- unname(cl[rownames(mf)])

  if (anyNA(cl_used)) {
    stop("Cluster ID could not be aligned for some model rows.", call. = FALSE)
  }

  y <- model.response(mf)
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    y <- trimws(as.character(y))
    if (all(c("No","Yes") %in% unique(y))) {
      mf[[1]] <- ifelse(y == "Yes", 1, ifelse(y == "No", 0, NA_real_))
    } else {
      levs <- sort(unique(na.omit(y)))
      if (length(levs) != 2) stop("Outcome must be binary (2 levels) for RR.", call. = FALSE)
      mf[[1]] <- ifelse(y == levs[2], 1, 0)
    }
  } else {
    mf[[1]] <- as.numeric(y)
  }

  fit <- glm(formula, data = mf, family = poisson(link = "log"))
  clv <- cluster_se(fit, cl_used)
  lmtest::coeftest(fit, vcov = clv$vcov)
}


parench_out_rr <- run_rr_cluster(icpparench ~ pupils, matched, matched$subclass)
evd_out_rr     <- run_rr_cluster(icpevdrain ~ pupils, matched, matched$subclass)
trach_out_rr   <- run_rr_cluster(trach ~ pupils, matched, matched$subclass)
gastro_out_rr  <- run_rr_cluster(gastro ~ pupils, matched, matched$subclass)


mort_out_rr <- run_rr_cluster(
  mort_inhospital ~ pupils,
  matched_mort,
  matched_mort$subclass
)
mort_out_rr

ltc_out_rr <- run_rr_cluster(
  ltc_inhospital ~ pupils,
  matched_ltc,
  matched_ltc$subclass
)
ltc_out_rr

wlt_out_rr <- run_rr_cluster(
  withdrawallst_bin ~ pupils,
  matched_wlt,
  matched_wlt$subclass
)
wlt_out_rr


####### Regression analyses: Logit #########


run_logit_cluster <- function(formula, data, cluster) {
  fit <- glm(formula, data = data, family = binomial)
  cl  <- cluster_se(fit, cluster)
  coefs <- coeftest(fit, vcov = cl$vcov)
  return(coefs)
}

#crani_out <- run_logit_cluster(crani ~ pupils, matched, matched$subclass)
#crani_out
parench_out <- run_logit_cluster(icpparench ~ pupils, matched, matched$subclass)
parench_out
evd_out      <- run_logit_cluster(icpevdrain ~ pupils, matched, matched$subclass)
evd_out
trach_out <- run_logit_cluster(trach ~ pupils, matched, matched$subclass)
trach_out
gastro_out <- run_logit_cluster(gastro ~ pupils, matched, matched$subclass)
gastro_out              


mort_out <- run_logit_cluster(
  mort_inhospital ~ pupils,
  matched_mort,
  matched_mort$subclass
)
mort_out

ltc_out <- run_logit_cluster(
  ltc_inhospital ~ pupils,
  matched_ltc,
  matched_ltc$subclass
)
ltc_out

wlt_out <- run_logit_cluster(
  withdrawallst_bin ~ pupils,
  matched_wlt,
  matched_wlt$subclass
)
wlt_out


# Linear regression with clustered SE
los_out <- lm_robust(finaldischargedays ~ pupils, data = matched, clusters = subclass)
icu_out <- lm_robust(totaliculos ~ pupils, data = matched, clusters = subclass)
vent_out <- lm_robust(totalventdays ~ pupils, data = matched, clusters = subclass)

los_out
icu_out
vent_out

# Cerebral monitoring days
matched_monitor <- matched %>% filter(!is.na(tbicerebralmonitordays))
monitor_days_out <- lm_robust(
  tbicerebralmonitordays ~ pupils,
  data = matched_monitor,
  clusters = subclass
)
monitor_days_out



# ---- helpers ----

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) "<0.001" else sprintf("%.3f", p)
}

fmt_ci <- function(lo, hi, digits = 2) {
  paste0(sprintf(paste0("%.", digits, "f"), lo),
         " to ",
         sprintf(paste0("%.", digits, "f"), hi))
}

# Risk ratios: input is coeftest matrix from run_rr_cluster()
summ_rr <- function(coeftest_mat, outcome, n_used, term = "pupils") {
  # coeftest_mat rows are (Intercept), pupils, etc.
  if (!(term %in% rownames(coeftest_mat))) {
    return(tibble(
      outcome = outcome, model = "Risk", n = n_used,
      estimate = NA_character_, ci_95 = NA_character_, p_value = NA_character_
    ))
  }

  b  <- coeftest_mat[term, "Estimate"]
  se <- coeftest_mat[term, "Std. Error"]
  p  <- coeftest_mat[term, "Pr(>|z|)"]

  # OR + Wald CI on log-odds scale
  or  <- exp(b)
  lo  <- exp(b - 1.96 * se)
  hi  <- exp(b + 1.96 * se)

  tibble(
    outcome = outcome,
    model = "Risk (RR)",
    n = n_used,
    estimate = sprintf("%.2f", or),
    ci_95 = fmt_ci(lo, hi, digits = 2),
    p_value = fmt_p(p)
  )
}


# Logistic: input is coeftest matrix from run_logit_cluster()
summ_logit <- function(coeftest_mat, outcome, n_used, term = "pupils") {
  # coeftest_mat rows are (Intercept), pupils, etc.
  if (!(term %in% rownames(coeftest_mat))) {
    return(tibble(
      outcome = outcome, model = "Logistic", n = n_used,
      estimate = NA_character_, ci_95 = NA_character_, p_value = NA_character_
    ))
  }

  b  <- coeftest_mat[term, "Estimate"]
  se <- coeftest_mat[term, "Std. Error"]
  p  <- coeftest_mat[term, "Pr(>|z|)"]

  # OR + Wald CI on log-odds scale
  or  <- exp(b)
  lo  <- exp(b - 1.96 * se)
  hi  <- exp(b + 1.96 * se)

  tibble(
    outcome = outcome,
    model = "Logistic (OR)",
    n = n_used,
    estimate = sprintf("%.2f", or),
    ci_95 = fmt_ci(lo, hi, digits = 2),
    p_value = fmt_p(p)
  )
}

# Linear: input is lm_robust object
summ_lm <- function(lmrob, outcome, n_used, term = "pupils") {
  s <- summary(lmrob)
  ct <- s$coefficients
  if (!(term %in% rownames(ct))) {
    return(tibble(
      outcome = outcome, model = "Linear (Δ days)", n = n_used,
      estimate = NA_character_, ci_95 = NA_character_, p_value = NA_character_
    ))
  }

  b  <- ct[term, "Estimate"]
  lo <- ct[term, "CI Lower"]
  hi <- ct[term, "CI Upper"]
  p  <- ct[term, "Pr(>|t|)"]

  tibble(
    outcome = outcome,
    model = "Linear (Δ days)",
    n = n_used,
    estimate = sprintf("%.2f", b),
    ci_95 = fmt_ci(lo, hi, digits = 2),
    p_value = fmt_p(p)
  )
}

# ---- build the paper table ----
# NOTE: if pupils is a factor, the term might be "pupilsPPR" (for pupillary reflexes present)

term_name <- "pupilsAbsent bilateral pupillary response"
              
results_table <- bind_rows(
  summ_logit(parench_out, "ICP parenchymal monitor", nrow(matched), term = term_name),
  summ_logit(evd_out,     "EVD placed",             nrow(matched), term = term_name),
  summ_logit(trach_out,   "Tracheostomy",           nrow(matched), term = term_name),
  summ_logit(gastro_out,  "Gastrostomy",            nrow(matched), term = term_name),

  summ_logit(mort_out,    "In-hospital mortality",  nrow(matched_mort), term = term_name),
  summ_logit(ltc_out,     "Discharge to LTC",       nrow(matched_ltc),  term = term_name),

  summ_logit(wlt_out,     "Withdrawal of LST",      nrow(matched_wlt),  term = term_name),

  summ_rr(parench_out_rr, "ICP parenchymal monitor", nrow(matched), term = term_name),
  summ_rr(evd_out_rr,     "EVD placed",             nrow(matched), term = term_name),
  summ_rr(trach_out_rr,   "Tracheostomy",           nrow(matched), term = term_name),
  summ_rr(gastro_out_rr,  "Gastrostomy",            nrow(matched), term = term_name),

  summ_rr(mort_out_rr,    "In-hospital mortality",  nrow(matched_mort), term = term_name),
  summ_rr(ltc_out_rr,     "Discharge to LTC",       nrow(matched_ltc),  term = term_name),

  summ_rr(wlt_out_rr,     "Withdrawal of LST",      nrow(matched_wlt),  term = term_name),

  summ_lm(los_out,        "Hospital LOS",           nrow(matched), term = term_name),
  summ_lm(icu_out,        "ICU LOS",                nrow(matched), term = term_name),
  summ_lm(vent_out,       "Ventilator days",        nrow(matched), term = term_name),

  summ_lm(monitor_days_out, "Cerebral monitoring days", nrow(matched_monitor), term = term_name)
) %>%
  mutate(
    contrast = "Absent vs Present pupillary reflexes"
  ) %>%
  select(outcome, model, contrast, n, estimate, ci_95, p_value)


write.csv(results_table, file.path(output_dir, "final_analysis.csv"), row.names = FALSE)

filter_log <- log_step_n(nrow(matched[matched$minority != "Minority",]), "(White)", filter_log)
filter_log <- log_step_n(nrow(matched[matched$minority == "Minority",]), "(Minority)", filter_log)
filter_log <- log_step_n(nrow(matched[matched$sex == "Male",]), "(Male)", filter_log)
filter_log <- log_step_n(nrow(matched[matched$sex == "Female",]), "(Female)", filter_log)
filter_log <- log_step_n(nrow(matched[matched$pupils != "Absent bilateral pupillary response",]), "Present pupillary reflexes", filter_log)
filter_log <- log_step_n(nrow(matched[matched$pupils == "Absent bilateral pupillary response",]), "Absent bilateral pupillary response", filter_log)
write_csv(filter_log, paste(output_dir, "/", "filtering_summary.csv", sep=""))


######### Output a table based on trach+peg vs. none in the matched pupil cohort #############

matched2 <- matched %>%
  mutate(
    trach_peg = case_when(
      trach == "Yes" | gastro == "Yes" ~ "Trach or PEG",
      trach == "No"  & gastro == "No"  ~ "None",
      TRUE ~ "None"
    ) %>% factor(levels = c("None", "Trach or PEG"))
  )

pupils_only <- matched2 %>%
  filter(pupils != "Absent bilateral pupillary response") %>%
  filter(trach_peg %in% c("None", "Trach or PEG")) %>%
  droplevels()

vars_cont <- c("ageyears", "iss", "totalgcs")

vars_cat <- c(
  "gcs_cat",
  "age_cat",
  "sex",
  "minority",
  "race",
  "ethnicity",
  "primarymethodpayment",
  "iss_cat",
  "mechanism",
  "intent",
  "verificationlevel",
  "statedesignation",
  "teachingstatus",
  "hospitaltype",
  "ich_category",
  "tbimidlineshift",
  "interfacilitytransfer",     # if present
  "eddischargedisposition"     # if present (your derived ED transfer variable)
)

vars_cont <- vars_cont[vars_cont %in% names(pupils_only)]
vars_cat  <- vars_cat[vars_cat %in% names(pupils_only)]

group_var2 <- "trach_peg"
g_all <- pupils_only[[group_var2]]
g_levels <- sort(unique(na.omit(g_all)))
if (length(g_levels) != 2) stop("Need exactly 2 levels for trach_peg after filtering.", call. = FALSE)
g1 <- g_levels[1]  # None
g2 <- g_levels[2]  # Trach+PEG

header_rows2 <- tibble::tibble(
  variable = c("N (encounters)", paste0("N: ", group_var2, " = ", g1), paste0("N: ", group_var2, " = ", g2)),
  level = "",
  overall = c(as.character(nrow(pupils_only)), "", ""),
  group1 = c("", as.character(sum(g_all == g1, na.rm = TRUE)), ""),
  group2 = c("", "", as.character(sum(g_all == g2, na.rm = TRUE))),
  p_value = "",
  missing = "",
  type = "header",
  note = ""
)

cont_rows2 <- purrr::map_dfr(vars_cont, ~ summarise_continuous(pupils_only, .x, group_var2, g1, g2))
cat_rows2  <- purrr::map_dfr(vars_cat,  ~ summarise_categorical(pupils_only, .x, group_var2, g1, g2, max_levels = 30, show_other = TRUE))

table_pupils_trachpeg <- dplyr::bind_rows(header_rows2, cont_rows2, cat_rows2) %>%
  dplyr::rename(
    !!paste0(group_var2, "=", g1) := group1,
    !!paste0(group_var2, "=", g2) := group2
  )

readr::write_csv(table_pupils_trachpeg, file.path(output_dir, "table_presentpupils_trachpeg_vs_none.csv"))

######### Output a table based on trach+peg vs. none in the matched absent pupils cohort #############

matched2 <- matched %>%
  mutate(
    trach_peg = case_when(
      trach == "Yes" | gastro == "Yes" ~ "Trach or PEG",
      trach == "No"  & gastro == "No"  ~ "None",
      TRUE ~ "None"
    ) %>% factor(levels = c("None", "Trach or PEG"))
  )

pupils_only <- matched2 %>%
  filter(pupils == "Absent bilateral pupillary response") %>%
  filter(trach_peg %in% c("None", "Trach or PEG")) %>%
  droplevels()

vars_cont <- c("ageyears", "iss", "totalgcs")

vars_cat <- c(
  "gcs_cat",
  "age_cat",
  "sex",
  "minority",
  "race",
  "ethnicity",
  "primarymethodpayment",
  "iss_cat",
  "mechanism",
  "intent",
  "verificationlevel",
  "statedesignation",
  "teachingstatus",
  "hospitaltype",
  "ich_category",
  "tbimidlineshift",
  "interfacilitytransfer",     # if present
  "eddischargedisposition"     # if present (your derived ED transfer variable)
)

vars_cont <- vars_cont[vars_cont %in% names(pupils_only)]
vars_cat  <- vars_cat[vars_cat %in% names(pupils_only)]

group_var2 <- "trach_peg"
g_all <- pupils_only[[group_var2]]
g_levels <- sort(unique(na.omit(g_all)))
if (length(g_levels) != 2) stop("Need exactly 2 levels for trach_peg after filtering.", call. = FALSE)
g1 <- g_levels[1]  # None
g2 <- g_levels[2]  # Trach+PEG

header_rows2 <- tibble::tibble(
  variable = c("N (encounters)", paste0("N: ", group_var2, " = ", g1), paste0("N: ", group_var2, " = ", g2)),
  level = "",
  overall = c(as.character(nrow(pupils_only)), "", ""),
  group1 = c("", as.character(sum(g_all == g1, na.rm = TRUE)), ""),
  group2 = c("", "", as.character(sum(g_all == g2, na.rm = TRUE))),
  p_value = "",
  missing = "",
  type = "header",
  note = ""
)

cont_rows2 <- purrr::map_dfr(vars_cont, ~ summarise_continuous(pupils_only, .x, group_var2, g1, g2))
cat_rows2  <- purrr::map_dfr(vars_cat,  ~ summarise_categorical(pupils_only, .x, group_var2, g1, g2, max_levels = 30, show_other = TRUE))

table_pupils_trachpeg <- dplyr::bind_rows(header_rows2, cont_rows2, cat_rows2) %>%
  dplyr::rename(
    !!paste0(group_var2, "=", g1) := group1,
    !!paste0(group_var2, "=", g2) := group2
  )

readr::write_csv(table_pupils_trachpeg, file.path(output_dir, "table_absentpupils_trachpeg_vs_none.csv"))
              
              
              
              
              
