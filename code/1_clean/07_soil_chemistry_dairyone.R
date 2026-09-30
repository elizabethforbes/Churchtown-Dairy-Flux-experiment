# 07_soil_chemistry_dairyone.R
# Dairy One soil tests (Mehlich-3 and Morgan extractable nutrients, pH, OM, CEC,
# base saturation) for the 15 plots at each of the three soil samplings.
# Input:  data/raw/dairy_one/soil/DairyOne_soil.csv
#         (annotated copy of the lab export DairyOne_soildata.csv: column 1 holds
#          the sampling timepoint T1-T3, Desc 1 = plot, Desc 3 = treatment;
#          lab Field Name 1-45 runs plots 1-15 for T1, then T2, then T3)
# Output: data/intermediate/dairy_one_clean.csv  one row per plot x timepoint
# Received by Dairy One 2026-01-21. Nitrate-N, total N, S, Fe, Mn, Cu, B, Mo were
# not reported.

library(dplyr)

treatment_key <- read.csv("data/intermediate/treatment_key.csv")

raw <- read.csv("data/raw/dairy_one/soil/DairyOne_soil.csv", check.names = FALSE)
names(raw)[1] <- "timepoint_label"

dairy_one_clean <- raw %>%
  transmute(
    timepoint = as.integer(sub("^T", "", trimws(timepoint_label))),
    plot = as.integer(sub("plot_", "", trimws(`Desc 1`))),
    lab_field = as.integer(`Field Name`),
    om_pct = as.numeric(`Organic Matter %`),
    ph = as.numeric(pH),
    buffer_ph = as.numeric(`Buffer pH`),
    cec_meq100g = as.numeric(CEC),
    exch_acidity_meq100g = as.numeric(`Exch Acidity`),
    base_sat_total_pct = as.numeric(`Base Saturation Total`),
    base_sat_ca_pct = as.numeric(`Base Saturation Ca`),
    base_sat_mg_pct = as.numeric(`Base Saturation Mg`),
    base_sat_k_pct = as.numeric(`Base Saturation K`),
    p_ppm = as.numeric(`P ppm`),
    k_ppm = as.numeric(`K ppm`),
    ca_ppm = as.numeric(`Ca ppm`),
    mg_ppm = as.numeric(`Mg ppm`),
    na_ppm = as.numeric(`Na ppm`),
    zn_ppm = as.numeric(`Zn ppm`),
    al_ppm = as.numeric(`Al ppm`),
    morgan_p_lb_ac = as.numeric(`Morgan P lb/A`),
    morgan_k_lb_ac = as.numeric(`Morgan K lb/A`),
    morgan_ca_lb_ac = as.numeric(`Morgan  Ca lb/A`),
    morgan_mg_lb_ac = as.numeric(`Morgan Mg lb/A`)
  ) %>%
  filter(!is.na(plot)) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, timepoint, everything()) %>%
  arrange(timepoint, plot)

# Sanity check: lab field number should equal (timepoint - 1) * 15 + plot
stopifnot(all(dairy_one_clean$lab_field == (dairy_one_clean$timepoint - 1) * 15 + dairy_one_clean$plot))

write.csv(dairy_one_clean, "data/intermediate/dairy_one_clean.csv", row.names = FALSE)
cat("Wrote data/intermediate/dairy_one_clean.csv\n")
cat(sprintf("  %d rows (15 plots x %d timepoints)\n", nrow(dairy_one_clean), n_distinct(dairy_one_clean$timepoint)))
