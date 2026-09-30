# 08_effect_synthesis.R
# Standardized effect sizes (Hedges' g) for every amendment x response contrast, with a
# Benjamini-Hochberg adjusted p (p_adj) applied only within the two broad screening
# panels (Dairy One soil tests; forage composition). Fluxes, soil microbial C and N
# assays and biomass were targeted tests (p_adj = NA).
# Input:  output/tables/treatment_effects.csv (02_treatment_effects.R)
# Output: output/tables/effect_synthesis.csv (used by Fig 5)

source("code/lib/setup.R")

eff <- read.csv("output/tables/treatment_effects.csv")

date_std <- c("29 May" = "29 May", "May" = "29 May", "21 Jul" = "21 Jul", "Jul" = "21 Jul",
              "14 Oct" = "14 Oct", "Oct" = "14 Oct")
labs_tbl <- tribble(
  ~metric,                      ~label,                     ~panel,
  "first_week_total_CO2",       "CO2, days 1-6",            "GHG fluxes",
  "first_week_total_CH4",       "CH4, days 1-6",            "GHG fluxes",
  "first_week_total_N2O",       "N2O, days 1-6",            "GHG fluxes",
  "season_total_CO2",           "CO2, season",              "GHG fluxes",
  "season_total_CH4",           "CH4, season",              "GHG fluxes",
  "season_total_N2O",           "N2O, season",              "GHG fluxes",
  "tin_ug_g",                   "Extractable mineral N",    "Soil N and C cycling",
  "initial_nh4_ug_g",           "Extractable NH4+",         "Soil N and C cycling",
  "initial_no3_ug_g",           "Extractable NO3-",         "Soil N and C cycling",
  "net_min_rate_ug_g_d",        "Net N mineralization",     "Soil N and C cycling",
  "net_nitr_rate_ug_g_d",       "Net nitrification",        "Soil N and C cycling",
  "sir_ug_co2c_hr_g",           "Substrate-induced resp.",  "Soil N and C cycling",
  "cmin_rate_ug_co2c_g_d",      "C mineralization",         "Soil N and C cycling",
  "dairyone_ph",                "pH",                       "Soil tests",
  "dairyone_om_pct",            "Organic matter",           "Soil tests",
  "dairyone_cec_meq100g",       "CEC",                      "Soil tests",
  "dairyone_base_sat_total_pct","Base saturation",          "Soil tests",
  "dairyone_p_ppm",             "P (Mehlich-3)",            "Soil tests",
  "dairyone_k_ppm",             "K (Mehlich-3)",            "Soil tests",
  "dairyone_ca_ppm",            "Ca (Mehlich-3)",           "Soil tests",
  "dairyone_mg_ppm",            "Mg (Mehlich-3)",           "Soil tests",
  "biomass_g_m2",               "Aboveground biomass",      "Biomass",
  "crude_protein_pct",          "Crude protein",            "Plant",
  "avail_protein_pct",          "Available protein",        "Plant",
  "adicp_pct",                  "ADICP",                    "Plant",
  "ndicp_pct",                  "NDICP",                    "Plant",
  "andf_pct",                   "aNDF",                     "Plant",
  "adf_pct",                    "ADF",                      "Plant",
  "lignin_pct",                 "Lignin",                   "Plant",
  "nfc_pct",                    "NFC",                      "Plant",
  "starch_pct",                 "Starch",                   "Plant",
  "water_sol_carbs_pct",        "WSC",                      "Plant",
  "simple_sugars_pct",          "Simple sugars",            "Plant",
  "crude_fat_pct",              "Crude fat",                "Plant",
  "tdn_pct",                    "TDN",                      "Plant",
  "ash_pct",                    "Ash",                      "Plant",
  "ca_pct",                     "Ca",                       "Plant",
  "p_pct",                      "P",                        "Plant",
  "mg_pct",                     "Mg",                       "Plant",
  "k_pct",                      "K",                        "Plant",
  "s_pct",                      "S",                        "Plant"
)
PANELS <- c("GHG fluxes", "Soil N and C cycling", "Soil tests", "Plant")

syn <- eff %>% inner_join(labs_tbl, by = "metric") %>%
  mutate(sampling = unname(date_std[group]),
         sampling = factor(ifelse(is.na(sampling), "single", sampling), levels = c("29 May", "21 Jul", "14 Oct", "single")),
         treatment = as_trt(treatment),
         sig = welch_p < 0.05) %>%
  group_by(panel) %>%
  mutate(p_adj = if (first(panel) %in% c("Soil tests", "Plant")) p.adjust(welch_p, "BH") else NA_real_) %>%
  ungroup()
stopifnot(!anyNA(syn$hedges_g))
write.csv(syn %>% mutate(label_order = match(metric, labs_tbl$metric)) %>%
            select(panel, label, label_order, metric, group, sampling, treatment, hedges_g, g_lo, g_hi, diff, ci_lo, ci_hi,
                   control_mean, welch_p, p_adj, anova_p),
          "output/tables/effect_synthesis.csv", row.names = FALSE)

n_sig <- sum(syn$sig); n_q <- sum(syn$p_adj < 0.05, na.rm = TRUE)
cat(sprintf("  %d effects; %d with Welch p < 0.05 (uncorrected), %d with BH-adjusted p < 0.05 (screening panels)\n", nrow(syn), n_sig, n_q))
