# 32_synthesis_figure.R
# Main Fig 5: soil tests and plant response -- the responses not shown in
# Figs 2-4 -- in one consolidated main-text figure.
#   (a) Dairy One soil tests at each sampling, as Hedges' g (amendment minus
#       control, pooled SD, small-sample corrected; 95% CI)
#   (b) aboveground biomass at the October harvest (plot values, mean and 95% CI)
#   (c) forage composition at the October harvest, as Hedges' g
# Open symbols: Welch p >= 0.05; filled: p < 0.05 (uncorrected).
# Also writes effect_synthesis.csv: Hedges' g and BH-FDR q-values for every
# amendment x response effect in the study. Benjamini-Hochberg adjustment is applied
# only within the two broad screening panels (soil chemistry; forage composition);
# fluxes, soil microbial C/N assays and biomass were targeted tests (p_adj = NA).
# Input:  output/tables/treatment_effects.csv (from 30_main_figures.R + 31_si_figures.R),
#         data/processed/biomass.csv
# Output: output/figures/main/fig5_soil_chemistry_plants.{pdf,png}; output/tables/effect_synthesis.csv

source("code/analysis/fig_setup.R")

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
write.csv(syn %>% select(panel, label, metric, group, treatment, hedges_g, g_lo, g_hi, diff, ci_lo, ci_hi,
                         control_mean, welch_p, p_adj, anova_p),
          "output/tables/effect_synthesis.csv", row.names = FALSE)

GLIM <- 4.5   # clip CIs for display; arrows mark clipped ends
panel_plot <- function(pn, title) {
  d <- syn %>% filter(panel == pn) %>%
    mutate(label = factor(label, levels = rev(labs_tbl$label[labs_tbl$panel == pn])),
           lo_c = pmax(g_lo, -GLIM), hi_c = pmin(g_hi, GLIM), g_c = pmax(pmin(hedges_g, GLIM), -GLIM))
  multi <- any(d$sampling != "single")
  pd <- position_dodge(width = if (multi) 0.7 else 0)
  ggplot(d, aes(y = label, group = sampling)) +
    annotate("rect", xmin = -0.8, xmax = 0.8, ymin = -Inf, ymax = Inf, fill = "grey95") +
    geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.3) +
    geom_linerange(aes(xmin = lo_c, xmax = hi_c, colour = treatment), position = pd, linewidth = 0.35) +
    geom_point(aes(x = g_c, colour = treatment, shape = treatment,
                   fill = ifelse(sig, as.character(treatment), "white"),
                   alpha = sampling), position = pd, size = 1.4, stroke = 0.4) +
    facet_grid(~ treatment, labeller = labeller(treatment = TRT_LABELS)) +
    scale_colour_trt(guide = "none") + scale_shape_trt(guide = "none") +
    scale_fill_manual(values = c(TRT_COLS, white = "white"), guide = "none") +
    scale_alpha_manual(values = c("29 May" = 0.45, "21 Jul" = 0.7, "14 Oct" = 1, single = 1), guide = "none") +
    scale_x_continuous(limits = c(-GLIM, GLIM), breaks = c(-4, -2, 0, 2, 4), oob = scales::squish) +
    labs(x = if (pn %in% c("Soil tests", "Plant")) "Hedges' g (amendment − control)" else NULL, y = NULL) +
    theme(panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.2),
          plot.title.position = "plot",
          panel.spacing.x = unit(6, "pt"), strip.text = element_text(hjust = 0.5),
          axis.text.y = element_text(size = rel(0.85)))
}
pa <- panel_plot("Soil tests", "Soil chemistry")
pc <- panel_plot("Plant", "Forage composition (Oct harvest)")
biomass <- read.csv("data/processed/biomass.csv") %>% group_by(plot, treatment) %>%
  summarize(biomass = mean(dry_matter_g_m2), .groups = "drop") %>% mutate(treatment = as_trt(treatment))
p_bio <- anova_p(biomass, biomass)$p
pb <- ggplot() + dot_ci_layers(biomass %>% mutate(x = treatment), trt_summary(biomass, biomass) %>% mutate(x = treatment),
                               x, biomass, pt_size = 1.3) +
  trt_axis() + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = expression(Aboveground~biomass~(g~m^{-2}))) +
  theme(plot.title.position = "plot", legend.position = "bottom")
fig6 <- (((pa / pb) + plot_layout(heights = c(1.35, 1))) | pc) + plot_layout(widths = c(1, 1), guides = "collect") +
  tags_pub() & theme(legend.position = "bottom")
save_fig(fig6, "fig5_soil_chemistry_plants", 180, 140)
old <- file.path(FIG_DIR, "main", paste0(rep(c("fig6_effect_synthesis", "fig6_soil_tests_plants", "fig5_soil_tests_plants"), each = 2), c(".pdf", ".png")))
invisible(file.remove(old[file.exists(old)]))

n_sig <- sum(syn$sig); n_q <- sum(syn$p_adj < 0.05, na.rm = TRUE)
cat(sprintf("  %d effects; %d with Welch p < 0.05 (uncorrected), %d with BH-adjusted p < 0.05 (screening panels)\n", nrow(syn), n_sig, n_q))
print(syn %>% filter(sig) %>% select(label, group, treatment, hedges_g, welch_p, p_adj) %>% as.data.frame())
cat("  wrote effect_synthesis.csv\n")
