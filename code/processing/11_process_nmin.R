# 11_process_nmin.R
# 28-day N mineralization: KCl-extractable NO3-N and NH4-N (Yale colorimetric
# analysis) joined to tube setup masses, blank-corrected, and converted to
# ug N per g dry soil. Also processes KCl extractions of the amendment material.
#
# Inputs:
#   data/raw/soil/nmin/Yale_inorgN_2026.xlsx        (mg N/L in extract)
#   data/raw/soil/cmin_mass/C-NMin_Mass_{1,2,3}.xlsx (tube setup masses)
#   data/raw/soil/manure_amendment/Manure Ammendment.xlsx
#   data/processed/gwc.csv, treatment_key.csv, dairy_one_manure.csv
# Outputs:
#   data/processed/nmin_tube.csv       one row per extracted tube
#   data/processed/nmin_plot.csv       plot x round: initial, incubated, net rates
#   data/processed/nmin_amendment.csv  amendment-material extracts
#
# Protocol (protocols/nmin, protocols/carbon_mineralization/nc_min_combined_workflow.docx):
#   ~6 g dry-equivalent soil + 25 mL 2 M KCl, shaken 30 min, settled overnight
#   at 4 C, filtered (Whatman 42). Initial tubes extracted at setup; incubated
#   tubes run through the 28-day C-min incubation at 65% WHC, then extracted.
#
# ug N / g dry soil = (C_sample - C_blank) [mg/L] * V_KCl [mL] / dry mass [g]
#
# Sample-tracking decisions (see notes at the bottom of this header):
#   Round 1: vial labels say D1 (A tubes) or D28 (B tubes). C-min round 1
#     incubated only the B tubes, so D1/D28 defines initial vs incubated and
#     the t0/t1 labels on the round-1 mass sheet are ignored.
#   Round 2: lab-number parity set t0 (odd) / t1 (even), but plots were not
#     evenly split, so 5 plots got 3 incubated + 1 initial tube and 4 plots the
#     reverse. "x" tubes (weighed 2025-08-05 from stored soil) were added to
#     fill gaps: x1-x5 initial (plots 1,5,9,14,15), x6-x9 incubated (4,6,11,12).
#     Vial labels for lab 98 (P13) and 101 (P10) disagree with the mass sheet
#     (P14, P12). The mass sheet is used: it gives every plot exactly 4 tubes,
#     and the printed tube labels were generated from it.
#   Duplicate instrument reads of one tube (labs 55, 90) are averaged.
#   The handwritten round-2 weighing log (data/raw/soil/paper_datasheets/min_*.jpg)
#   confirms the mass sheet for labs 98 (plot 14) and 101 (plot 12).
#   All tubes were refrigerated between weighing and incubation or extraction
#   (per J. Gewirtzman, 2026-09-28). Initial-extraction dates were not recorded.
#
# Incubation length: extraction dates were not recorded. Each incubated tube is
#   timed from its first C-min flush to the final C-min IRGA/LGR read, on the
#   assumption that it was extracted right after the last C-min read:
#     round 1: 2025-06-03 -> 2025-07-02 (29 d)
#     round 2: 2025-08-04 -> 2025-09-03 (30 d); x6-x9: 2025-08-07 -> 2025-09-03 (27 d)
#     round 3: 2025-11-25 -> 2025-12-23 (28 d)
#
# Soil water at extraction (used only when ADD_SOIL_WATER = TRUE):
#   initial tubes: field moisture (oven GWC)
#   incubated, round 1: TARGET_GWC (water added was not recorded)
#   incubated, round 2: field moisture + recorded water added
#   incubated, round 3: field moisture, or TARGET_GWC if ROUND3_WATER_ADDED

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

# --- Settings ----------------------------------------------------------------
V_KCL_ML            <- 25     # KCl added per tube (protocol)
ADD_SOIL_WATER      <- FALSE  # TRUE: extract volume = KCl + water held in soil
TARGET_GWC          <- 0.257  # moisture target hard-coded in the setup sheets (65% WHC)
ROUND3_WATER_ADDED  <- FALSE  # round 3 has no water record; assume none was added
INCLUDE_X_INITIAL   <- TRUE   # round-2 x1-x5 initial tubes (stored ~15 d first)
INCLUDE_X_INCUBATED <- TRUE   # round-2 x6-x9 incubated tubes
# Readings at or above this (mg N/L) are treated as at the analyzer ceiling.
# The manure extracts cluster at 15-17 mg/L while Dairy One ammonium predicts
# 40-55 mg/L for slurry, so the true value is only known to be >= the reading.
CEILING_MGL         <- 14.9
NEAR_CEILING_MGL    <- 12     # soil readings above this get a caution flag
# Tubes kept in analyses but flagged as suspect (may be excluded later)
SUSPECT_TUBES       <- c("2_x5")  # plot 15 initial x tube: NH4 ~4x its partner
# Plot corrections where the vial label conflicts with the mass sheet.
# Default trusts the mass sheet. Set to the vial plot to override.
LAB_PLOT_OVERRIDE   <- c()    # e.g. c("2_98" = 13, "2_101" = 10)

raw_path <- "data/raw/soil/nmin/Yale_inorgN_2026.xlsx"

treatment_key <- read.csv("data/processed/treatment_key.csv")
gwc <- read.csv("data/processed/gwc.csv")
gwc_plot <- gwc %>%
  group_by(plot, round = timepoint) %>%
  summarize(gwc = mean(gwc, na.rm = TRUE), .groups = "drop")

# --- 1. Read analytical results ---------------------------------------------
res <- read_excel(raw_path, sheet = "Final data") %>%
  select(sample_id = 1, no3_mgL = 2, nh4_mgL = 3) %>%
  filter(!is.na(sample_id)) %>%
  mutate(sample_id = str_squish(sample_id), row_order = row_number())

other_project <- str_detect(res$sample_id, "SRS|^CP\\d|^BL\\d")
cat(sprintf("Read %d rows; dropping %d from other projects (SRS, CP, BL)\n",
            nrow(res), sum(other_project)))
res <- res[!other_project, ]

# --- 2. Classify rows: blanks, soil tubes, amendment tubes -------------------
# Each blank batch is tied to the samples it was run alongside.
blank_batch <- function(id) {
  case_when(
    id %in% c("blank 1", "blank 2")            ~ "R1",
    id == "N min 2_blank"                      ~ "R2_initial",
    id == "N min 2_T1 blank"                   ~ "R2_incubated",
    id == "8/8/25_blank"                       ~ "R2_x_amend",
    id == "T3_blank"                           ~ "R3_initial",
    str_detect(id, "^N min 3_T1_Blank")        ~ "R3_incubated",
    TRUE ~ NA_character_
  )
}

res <- res %>%
  mutate(
    blank_batch = blank_batch(sample_id),
    type = case_when(
      !is.na(blank_batch)                          ~ "blank",
      str_detect(sample_id, "(?i)(slurry|compost)_Z") ~ "amendment",
      str_detect(sample_id, "N min \\d")           ~ "soil",
      TRUE ~ "unknown"
    )
  )
if (any(res$type == "unknown")) {
  stop("Unclassified sample IDs: ", paste(res$sample_id[res$type == "unknown"], collapse = "; "))
}

blanks <- res %>%
  filter(type == "blank") %>%
  group_by(blank_batch) %>%
  summarize(n_blank = n(),
            blank_no3_mgL = mean(no3_mgL), blank_nh4_mgL = mean(nh4_mgL),
            blank_no3_sd = sd(no3_mgL), blank_nh4_sd = sd(nh4_mgL),
            .groups = "drop")
cat("\nBlank means by batch (mg N/L):\n")
print(as.data.frame(blanks %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))

# --- 3. Soil tubes: parse IDs ------------------------------------------------
# e.g. "238_N min 3_P15 T0_B", "N min 2 x9_P12 T1_C", "59_N min 1 D1_P15 T1_A"
soil_pat <- "^(?:(\\d+)_)?N min (\\d)(?: x(\\d))?(?: (D1|D28))?_P(\\d+) (T\\d)_([A-C])$"
soil <- res %>%
  filter(type == "soil") %>%
  mutate(m = str_match(sample_id, soil_pat)) %>%
  mutate(
    round     = as.integer(m[, 3]),
    lab_no    = if_else(is.na(m[, 4]), m[, 2], paste0("x", m[, 4])),
    extr_day  = m[, 5],
    vial_plot = as.integer(m[, 6]),
    vial_tp   = tolower(m[, 7]),
    vial_rep  = m[, 8]
  ) %>%
  select(-m)
if (any(is.na(soil$round) | is.na(soil$lab_no))) {
  stop("Could not parse: ", paste(soil$sample_id[is.na(soil$round)], collapse = "; "))
}

# Duplicate reads of the same tube -> average
soil <- soil %>%
  group_by(round, lab_no, extr_day, vial_plot, vial_tp, vial_rep) %>%
  summarize(sample_id = first(sample_id), n_reads = n(),
            no3_mgL = mean(no3_mgL), nh4_mgL = mean(nh4_mgL), .groups = "drop")

# --- 4. Setup masses ---------------------------------------------------------
read_mass <- function(rnd) {
  read_excel(sprintf("data/raw/soil/cmin_mass/C-NMin_Mass_%d.xlsx", rnd),
             sheet = "Sheet1", col_types = "text") %>%
    filter(!is.na(Plot)) %>%
    transmute(round = rnd,
              lab_no = str_remove(Lab_No, "\\.0+$"),
              plot = as.integer(as.numeric(Plot)),
              mass_tp = Timepoint, mass_rep = Replicate,
              date_weighed = Date_Weighed,
              fresh_mass_g = as.numeric(Mass_Soil_g),
              water_added_g = as.numeric(Mass_Water_Added_g))
}
mass <- bind_rows(lapply(1:3, read_mass))

soil <- soil %>%
  left_join(mass, by = c("round", "lab_no"))
if (any(is.na(soil$fresh_mass_g))) {
  stop("No setup mass for: ", paste(soil$sample_id[is.na(soil$fresh_mass_g)], collapse = "; "))
}

# Apply any manual plot overrides
ov_key <- paste(soil$round, soil$lab_no, sep = "_")
has_ov <- ov_key %in% names(LAB_PLOT_OVERRIDE)
soil$plot[has_ov] <- LAB_PLOT_OVERRIDE[ov_key[has_ov]]

label_conflicts <- soil %>% filter(vial_plot != plot)
if (nrow(label_conflicts) > 0) {
  cat("\nVial label plot differs from mass sheet (mass sheet used):\n")
  print(as.data.frame(label_conflicts %>% select(sample_id, vial_plot, mass_sheet_plot = plot)))
}

# --- 5. Initial vs incubated, batch, dry mass --------------------------------
soil <- soil %>%
  mutate(
    x_tube = str_starts(lab_no, "x"),
    extraction = case_when(
      round == 1 & extr_day == "D1"  ~ "initial",
      round == 1 & extr_day == "D28" ~ "incubated",
      round %in% 2:3 & mass_tp == "t0" ~ "initial",
      round %in% 2:3 & mass_tp == "t1" ~ "incubated"
    ),
    blank_batch = case_when(
      round == 1 ~ "R1",
      round == 2 & x_tube & extraction == "initial" ~ "R2_x_amend",
      round == 2 & extraction == "initial"   ~ "R2_initial",
      round == 2 & extraction == "incubated" ~ "R2_incubated",
      round == 3 & extraction == "initial"   ~ "R3_initial",
      round == 3 & extraction == "incubated" ~ "R3_incubated"
    )
  ) %>%
  left_join(gwc_plot, by = c("plot", "round")) %>%
  left_join(blanks %>% select(blank_batch, blank_no3_mgL, blank_nh4_mgL), by = "blank_batch") %>%
  left_join(treatment_key, by = "plot") %>%
  mutate(
    dry_mass_g = fresh_mass_g / (1 + gwc),
    incubation_start = case_when(
      extraction == "initial" ~ as.Date(NA),
      round == 1 ~ as.Date("2025-06-03"),
      round == 2 & x_tube ~ as.Date("2025-08-07"),
      round == 2 ~ as.Date("2025-08-04"),
      round == 3 ~ as.Date("2025-11-25")),
    incubation_end = case_when(
      extraction == "initial" ~ as.Date(NA),
      round == 1 ~ as.Date("2025-07-02"),
      round == 2 ~ as.Date("2025-09-03"),
      round == 3 ~ as.Date("2025-12-23")),
    incubation_days = as.numeric(incubation_end - incubation_start),
    field_water_g = fresh_mass_g - dry_mass_g,
    soil_water_g = case_when(
      extraction == "initial" ~ field_water_g,
      round == 2 & !is.na(water_added_g) ~ field_water_g + water_added_g,
      round == 3 & !ROUND3_WATER_ADDED ~ field_water_g,
      TRUE ~ pmax(field_water_g, dry_mass_g * TARGET_GWC)),
    gwc_at_extraction = soil_water_g / dry_mass_g,
    extract_vol_ml = V_KCL_ML + if (ADD_SOIL_WATER) soil_water_g else 0,
    no3_mgL_corr = no3_mgL - blank_no3_mgL,
    nh4_mgL_corr = nh4_mgL - blank_nh4_mgL,
    no3_ug_g = no3_mgL_corr * extract_vol_ml / dry_mass_g,
    nh4_ug_g = nh4_mgL_corr * extract_vol_ml / dry_mass_g,
    tin_ug_g = no3_ug_g + nh4_ug_g,
    include = case_when(
      x_tube & extraction == "initial"   ~ INCLUDE_X_INITIAL,
      x_tube & extraction == "incubated" ~ INCLUDE_X_INCUBATED,
      TRUE ~ TRUE
    ),
    flag = paste0(
      if_else(no3_mgL_corr < 0, "no3_below_blank;", ""),
      if_else(nh4_mgL_corr < 0, "nh4_below_blank;", ""),
      if_else(x_tube, "x_tube_stored_soil;", ""),
      if_else(paste(round, lab_no, sep = "_") %in% SUSPECT_TUBES, "suspect_outlier;", ""),
      if_else(pmax(no3_mgL, nh4_mgL) >= CEILING_MGL, "at_analyzer_ceiling;",
              if_else(pmax(no3_mgL, nh4_mgL) > NEAR_CEILING_MGL, "near_analyzer_ceiling;", "")),
      if_else(n_reads > 1, "duplicate_read_averaged;", ""),
      if_else(vial_plot != plot, "vial_label_plot_conflict;", "")
    ),
    flag = na_if(str_remove(flag, ";$"), "")
  )

nmin_tube <- soil %>%
  arrange(round, extraction, plot, lab_no) %>%
  select(round, plot, treatment, extraction, lab_no, sample_id, x_tube, include,
         blank_batch, incubation_days, fresh_mass_g, gwc, dry_mass_g,
         water_added_g, gwc_at_extraction, extract_vol_ml,
         no3_mgL, nh4_mgL, blank_no3_mgL, blank_nh4_mgL,
         no3_ug_g, nh4_ug_g, tin_ug_g, flag)

cat("\nTubes per round x extraction (included):\n")
print(with(filter(nmin_tube, include), table(round, extraction)))

# --- 6. Plot-level net mineralization ---------------------------------------
tube_means <- nmin_tube %>%
  filter(include) %>%
  group_by(round, plot, treatment, extraction) %>%
  summarize(n = n(),
            no3 = mean(no3_ug_g), nh4 = mean(nh4_ug_g), tin = mean(tin_ug_g),
            days = mean(incubation_days),
            .groups = "drop")

nmin_plot <- tube_means %>%
  pivot_wider(names_from = extraction, values_from = c(n, no3, nh4, tin, days),
              names_glue = "{extraction}_{.value}") %>%
  transmute(
    round, plot, treatment,
    n_initial = initial_n, n_incubated = incubated_n,
    initial_no3_ug_g = initial_no3, initial_nh4_ug_g = initial_nh4,
    initial_tin_ug_g = initial_tin,
    incubated_no3_ug_g = incubated_no3, incubated_nh4_ug_g = incubated_nh4,
    incubated_tin_ug_g = incubated_tin,
    incubation_days = incubated_days,
    net_min_ug_g   = incubated_tin - initial_tin,
    net_nitr_ug_g  = incubated_no3 - initial_no3,
    net_ammon_ug_g = incubated_nh4 - initial_nh4,
    net_min_rate_ug_g_d  = net_min_ug_g / incubation_days,
    net_nitr_rate_ug_g_d = net_nitr_ug_g / incubation_days
  ) %>%
  arrange(round, plot)

# --- 7. Amendment material ---------------------------------------------------
# e.g. "N min compost_Z1A T0_Aa", "Slurry 1 P slurry_Z6B T0_Cb"
amend_mass <- read_excel("data/raw/soil/manure_amendment/Manure Ammendment.xlsx",
                         sheet = "Sheet1", col_types = "text") %>%
  filter(!is.na(Plot)) %>%
  transmute(amendment_type = tolower(Plot), replicate = Replicate,
            fresh_mass_g = as.numeric(Mass_Soil_g))

manure <- read.csv("data/processed/dairy_one_manure.csv") %>%
  group_by(amendment_type) %>%
  summarize(total_solids_pct = mean(total_solids_pct),
            total_n_pct_fresh = mean(total_n_pct),
            d1_nh4_pct_min = min(ammonium_n_pct), d1_nh4_pct_max = max(ammonium_n_pct),
            .groups = "drop")

nmin_amendment <- res %>%
  filter(type == "amendment") %>%
  mutate(m = str_match(sample_id, "(?i)(slurry|compost)_Z(\\d)([AB]) T0_([A-C][ab])"),
         amendment_type = tolower(m[, 2]),
         bottle = paste0("Z", m[, 3]),
         replicate = m[, 5],
         blank_batch = "R2_x_amend") %>%
  select(-m) %>%
  left_join(amend_mass, by = c("amendment_type", "replicate")) %>%
  left_join(blanks %>% select(blank_batch, blank_no3_mgL, blank_nh4_mgL), by = "blank_batch") %>%
  left_join(manure, by = "amendment_type") %>%
  mutate(
    dry_mass_g = fresh_mass_g * total_solids_pct / 100,
    no3_ug = (no3_mgL - blank_no3_mgL) * V_KCL_ML,
    nh4_ug = (nh4_mgL - blank_nh4_mgL) * V_KCL_ML,
    no3_ug_g_fresh = no3_ug / fresh_mass_g,
    nh4_ug_g_fresh = nh4_ug / fresh_mass_g,
    no3_ug_g_dry = no3_ug / dry_mass_g,
    nh4_ug_g_dry = nh4_ug / dry_mass_g,
    # share of Dairy One total N that the extract recovered as mineral N
    min_n_pct_of_total_n = (no3_ug + nh4_ug) / fresh_mass_g / 1e4 /
      total_n_pct_fresh * 100,
    # Extract NH4 (mg/L) that Dairy One's ammonium range would predict for this tube
    expected_nh4_mgL_low  = d1_nh4_pct_min * 1e4 * fresh_mass_g / V_KCL_ML,
    expected_nh4_mgL_high = d1_nh4_pct_max * 1e4 * fresh_mass_g / V_KCL_ML,
    no3_censored = no3_mgL >= CEILING_MGL,
    nh4_censored = nh4_mgL >= CEILING_MGL,
    flag = case_when(
      no3_censored | nh4_censored ~ "at analyzer ceiling: censored values are lower bounds",
      TRUE ~ NA_character_)
  ) %>%
  select(amendment_type, bottle, replicate, sample_id, fresh_mass_g,
         total_solids_pct, dry_mass_g, no3_mgL, nh4_mgL,
         no3_censored, nh4_censored,
         no3_ug_g_fresh, nh4_ug_g_fresh, no3_ug_g_dry, nh4_ug_g_dry,
         total_n_pct_fresh, min_n_pct_of_total_n,
         expected_nh4_mgL_low, expected_nh4_mgL_high, flag) %>%
  arrange(amendment_type, bottle, replicate)

# --- 8. Write ----------------------------------------------------------------
write.csv(nmin_tube, "data/processed/nmin_tube.csv", row.names = FALSE)
write.csv(nmin_plot, "data/processed/nmin_plot.csv", row.names = FALSE)
write.csv(nmin_amendment, "data/processed/nmin_amendment.csv", row.names = FALSE)
cat(sprintf("\nWrote nmin_tube.csv (%d tubes), nmin_plot.csv (%d plot-rounds), nmin_amendment.csv (%d)\n",
            nrow(nmin_tube), nrow(nmin_plot), nrow(nmin_amendment)))
