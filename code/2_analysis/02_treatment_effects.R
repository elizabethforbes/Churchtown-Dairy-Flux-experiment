# 02_treatment_effects.R
# Amendment - control contrasts for every plot-level response: difference in means
# with Welch 95% CI and p, % of the control mean, Hedges' g (95% CI), and the one-way
# ANOVA p across the three treatments (within each sampling where relevant).
#   Cumulative fluxes (season; days 1-6), soil microbial and N-cycling metrics,
#   aboveground biomass, forage composition, and Dairy One soil tests.
# Also tests forage composition as a whole (PERMANOVA, 9,999 permutations).
# Input:  output/tables/ghg_totals_by_plot.csv, soil_metrics_by_plot.csv (01_plot_totals.R);
#         data/clean/biomass.csv, forage_quality.csv, soil_chemistry.csv
# Output: output/tables/treatment_effects.csv, forage_permanova.csv

source("code/lib/setup.R")
effects <- list()

# --- cumulative fluxes -------------------------------------------------------------------
tot <- read.csv("output/tables/ghg_totals_by_plot.csv") %>% mutate(treatment = as_trt(treatment))
flux_cols <- c(CO2 = "CO2_C_g_m2", CH4 = "CH4_C_mg_m2", N2O = "N2O_N_mg_m2")
for (per in c("season", "first_week")) for (g in names(flux_cols)) {
  d <- tot %>% filter(period == per) %>% mutate(val = .data[[flux_cols[g]]])
  key <- if (per == "season") "season" else "pulse"
  effects[[paste0(key, "_", g)]] <- diff_vs_control(d, val) %>%
    mutate(metric = paste0(if (per == "season") "season_total_" else "first_week_total_", g),
           group = if (per == "season") "29 May-14 Oct" else "29 May-3 Jun", anova_p = anova_p(d, val)$p)
}

# --- soil microbial and N-cycling metrics, per sampling ------------------------------------
soil <- read.csv("output/tables/soil_metrics_by_plot.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(round_lab, levels = ROUND_LABELS))
for (v in c("sir_ug_co2c_hr_g", "cmin_rate_ug_co2c_g_d", "net_min_rate_ug_g_d", "net_nitr_rate_ug_g_d",
            "initial_nh4_ug_g", "initial_no3_ug_g", "tin_ug_g")) {
  pv <- anova_p(soil, !!sym(v), round_lab)
  effects[[v]] <- diff_vs_control(soil, !!sym(v), round_lab) %>%
    mutate(metric = v, group = as.character(round_lab)) %>%
    left_join(pv %>% transmute(group = as.character(round_lab), anova_p = p), by = "group") %>% select(-round_lab)
}

fmt <- function(d) d %>% transmute(metric, group, treatment, control_mean = signif(control_mean, 3),
                                   diff = signif(diff, 3), ci_lo = signif(lo, 3), ci_hi = signif(hi, 3),
                                   pct_of_control = round(pct, 1), welch_p = round(p, 3), anova_p = round(anova_p, 3),
                                   hedges_g = round(hedges_g, 2), g_lo = round(g_lo, 2), g_hi = round(g_hi, 2))
eff <- fmt(bind_rows(effects))

# --- plants: biomass and forage composition ---------------------------------------------
FORAGE_VARS <- c("crude_protein_pct", "avail_protein_pct", "adicp_pct", "ndicp_pct", "andf_pct", "adf_pct",
                 "lignin_pct", "nfc_pct", "starch_pct", "water_sol_carbs_pct", "simple_sugars_pct",
                 "crude_fat_pct", "tdn_pct", "ash_pct", "ca_pct", "p_pct", "mg_pct", "k_pct", "s_pct")
plant <- clean_csv("biomass.csv") %>% group_by(plot, treatment) %>%
  summarize(biomass = mean(dry_matter_g_m2), .groups = "drop") %>%
  left_join(clean_csv("forage_quality.csv") %>% select(-treatment), by = "plot") %>% mutate(treatment = as_trt(treatment))
fstat <- plant %>% select(plot, treatment, all_of(FORAGE_VARS)) %>%
  pivot_longer(-c(plot, treatment), names_to = "var", values_to = "value") %>% group_by(var) %>%
  summarize(p = summary(aov(value ~ treatment))[[1]][1, "Pr(>F)"], .groups = "drop")
perm <- { set.seed(1); vegan::adonis2(scale(plant[, FORAGE_VARS]) ~ treatment, data = plant,
                                      method = "euclidean", permutations = 9999) }
write.csv(tibble(test = "PERMANOVA, forage composition (scaled, Euclidean, 9999 permutations)",
                 F = signif(perm$F[1], 3), R2 = signif(perm$R2[1], 3), p = perm$`Pr(>F)`[1]),
          "output/tables/forage_permanova.csv", row.names = FALSE)
plant_eff <- bind_rows(
  diff_vs_control(plant, biomass) %>% mutate(metric = "biomass_g_m2", anova_p = anova_p(plant, biomass)$p),
  bind_rows(lapply(FORAGE_VARS, function(v) diff_vs_control(plant, !!sym(v)) %>%
    mutate(metric = v, anova_p = fstat$p[fstat$var == v])))) %>%
  mutate(group = "Oct harvest") %>% fmt()

# --- Dairy One soil tests, per sampling ------------------------------------------------
SOILTEST_VARS <- c("ph", "om_pct", "cec_meq100g", "base_sat_total_pct", "p_ppm", "k_ppm", "ca_ppm", "mg_ppm")
d1 <- clean_csv("soil_chemistry.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(ROUND_LABELS[timepoint], levels = ROUND_LABELS))
soiltest_eff <- bind_rows(lapply(SOILTEST_VARS, function(v) {
  pv <- anova_p(d1, !!sym(v), round_lab)
  diff_vs_control(d1, !!sym(v), round_lab) %>% left_join(pv, by = "round_lab") %>%
    mutate(metric = paste0("dairyone_", v), anova_p = p.y, p = p.x, grp = as.character(round_lab))
})) %>% { d <- .; bind_rows(lapply(split(d, d$grp), function(x) fmt(x %>% mutate(group = x$grp[1])))) }

write.csv(bind_rows(eff, plant_eff, soiltest_eff), "output/tables/treatment_effects.csv", row.names = FALSE)
cat(sprintf("  wrote treatment_effects.csv; forage PERMANOVA p = %.3f\n", perm$`Pr(>F)`[1]))
