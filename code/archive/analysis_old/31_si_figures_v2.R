# 31_si_figures.R
# Supplementary figures (output/figures/si). Run after 30_main_figures.R.
#   Fig S1  Amendment composition (N forms, total solids)
#   Fig S2  GHG fluxes as amendment minus control, by campaign
#   Fig S3  Soil N and C metrics as amendment minus control
#   Fig S4  Soil moisture and pH at sampling; handheld vs chamber probe
#   Fig S5  C mineralization time courses (all plots shown)
#   Fig S6  Extractable N pools, day 0 vs day 28
#   Fig S7  Plant response: biomass and forage composition

source("code/analysis/fig_setup.R")
ev <- function(x) eval(parse(text = x))
round_fac <- function(r) factor(ROUND_LABELS[as.character(r)], levels = ROUND_LABELS)
sampled_fac <- function(r) factor(paste0("Sampled ", ROUND_LABELS[as.character(r)]),
                                  levels = paste0("Sampled ", ROUND_LABELS))
diff_layers <- function(dodge = 0.45) list(
  zero_line(),
  geom_linerange(aes(ymin = lo, ymax = hi), position = position_dodge(dodge), linewidth = 0.45, show.legend = FALSE),
  geom_point(aes(fill = treatment, shape = treatment), position = position_dodge(dodge), size = 1.8, stroke = 0.4),
  scale_colour_trt(), scale_fill_trt(), scale_shape_trt()
)

# =============================================================================
# Fig S1: amendment composition
# =============================================================================
manure <- read.csv("data/processed/dairy_one_manure.csv") %>%
  mutate(treatment = as_trt(amendment_type), organic = organic_n_pct * 10,
         ammonium = ammonium_n_pct * 10, total_n = total_n_pct * 10,
         n_dry = total_n_pct / (total_solids_pct / 100))
man_long <- manure %>% select(treatment, organic, ammonium) %>%
  pivot_longer(-treatment, names_to = "form", values_to = "kg") %>%
  group_by(treatment, form) %>% summarize(kg = mean(kg), .groups = "drop") %>%
  mutate(form = factor(form, levels = c("ammonium", "organic")), key = paste(treatment, form))
fills <- c(setNames(TRT_COLS[c("compost", "slurry")], paste(c("compost", "slurry"), "organic")),
           setNames(colorspace::lighten(TRT_COLS[c("compost", "slurry")], 0.6), paste(c("compost", "slurry"), "ammonium")))
s1a <- ggplot(man_long, aes(treatment, kg, fill = key)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
  geom_point(data = manure, aes(treatment, total_n), inherit.aes = FALSE, position = position_nudge(x = 0.42),
             size = 1, colour = INK) +
  geom_text(data = man_long %>% group_by(treatment) %>% summarize(f = kg[form == "ammonium"] / sum(kg), top = sum(kg)),
            aes(treatment, top, label = sprintf("NH[4]^'+'*'-N:'~'%.0f%%'", 100 * f)), parse = TRUE,
            inherit.aes = FALSE, vjust = -0.5, size = 2.1, colour = INK) +
  scale_fill_manual(values = fills, guide = "none") + scale_x_discrete(labels = TRT_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = expression(N~(kg~Mg^{-1}~fresh~mass)), title = "Nitrogen as applied",
       subtitle = "Dark = organic N, light = ammonium N")
dot_amend <- function(v, ylab, title) ggplot(manure, aes(treatment, {{ v }}, colour = treatment, fill = treatment, shape = treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6, alpha = 0.25, colour = NA) +
  geom_point(size = 1.5, stroke = 0.4) +
  scale_colour_trt(guide = "none") + scale_fill_trt(guide = "none") + scale_shape_trt(guide = "none") +
  scale_x_discrete(labels = TRT_LABELS) + scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
  labs(x = NULL, y = ylab, title = title)
s1b <- dot_amend(total_solids_pct, "% of fresh mass", "Total solids (dry matter)")
s1c <- dot_amend(n_dry, "% of dry mass", "Total N, dry-mass basis")
figs1 <- (s1a | s1b | s1c) + tags_pub()
save_fig(figs1, "figS1_amendment_composition", 180, 70, "si")

# =============================================================================
# Fig S2: flux difference from control
# =============================================================================
flux_plot <- read.csv("data/processed/flux_estimates.csv") %>% mutate(date = as.Date(date)) %>%
  group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))
gl <- list(FCO2_DRY = c("expression(bold(CO[2]))", "expression(Delta*CO[2]~(mu*mol~m^{-2}~s^{-1}))"),
           FCH4_DRY = c("expression(bold(CH[4]))", "expression(Delta*CH[4]~(nmol~m^{-2}~s^{-1}))"),
           FN2O     = c("expression(bold(N[2]*O))", "expression(Delta*N[2]*O~(nmol~m^{-2}~s^{-1}))"))
s2 <- lapply(names(gl), function(v) {
  dv <- diff_vs_control(flux_plot, !!sym(v), date)
  ggplot(dv, aes(date, diff, colour = treatment)) +
    annotate("rect", xmin = -Inf, xmax = APPLICATION_DATE, ymin = -Inf, ymax = Inf, fill = "grey95") +
    application_line() + diff_layers(dodge = 3) +
    geom_line(position = position_dodge(3), linewidth = 0.4, show.legend = FALSE) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(x = NULL, y = ev(gl[[v]][2]), title = ev(gl[[v]][1]))
})
figs2 <- wrap_plots(s2, ncol = 1) + plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(figs2, "figS2_flux_vs_control", 120, 150, "si")

# =============================================================================
# Fig S3: soil metrics difference from control
# =============================================================================
soil <- read.csv("output/tables/soil_metrics_by_plot.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(round_lab, levels = ROUND_LABELS))
soil_metrics <- tribble(
  ~col,                    ~title,                     ~ylab,
  "initial_nh4_ug_g",      "Extractable ammonium",     "expression(Delta*NH[4]^'+'*'-N'~(mu*g~N~g^{-1}))",
  "initial_no3_ug_g",      "Extractable nitrate",      "expression(Delta*NO[3]^'-'*'-N'~(mu*g~N~g^{-1}))",
  "net_min_rate_ug_g_d",   "Net N mineralization",     "expression(Delta~(mu*g~N~g^{-1}~d^{-1}))",
  "net_nitr_rate_ug_g_d",  "Net nitrification",        "expression(Delta~(mu*g~N~g^{-1}~d^{-1}))",
  "sir_ug_co2c_hr_g",      "Substrate-induced resp.",  "expression(Delta~(mu*g~CO[2]*'-C'~g^{-1}~h^{-1}))",
  "cmin_rate_ug_co2c_g_d", "C mineralization (28 d)",  "expression(Delta~(mu*g~CO[2]*'-C'~g^{-1}~d^{-1}))"
)
s3 <- lapply(seq_len(nrow(soil_metrics)), function(i) {
  m <- soil_metrics[i, ]
  ggplot(diff_vs_control(soil, !!sym(m$col), round_lab), aes(round_lab, diff, colour = treatment)) +
    diff_layers() + labs(x = "Soil sampling", y = ev(m$ylab), title = m$title)
})
figs3 <- wrap_plots(s3, ncol = 3) + plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(figs3, "figS3_soil_vs_control", 180, 115, "si")

# =============================================================================
# Fig S4: soil moisture and pH at sampling; instrument comparison
# =============================================================================
gwc <- read.csv("data/processed/gwc.csv") %>% group_by(plot, treatment, timepoint) %>%
  summarize(gwc = mean(gwc), .groups = "drop") %>% mutate(treatment = as_trt(treatment), round_lab = round_fac(timepoint))
ph <- read.csv("data/processed/ph.csv") %>% group_by(plot, treatment, timepoint) %>%
  summarize(ph = mean(ph, na.rm = TRUE), .groups = "drop") %>% mutate(treatment = as_trt(treatment), round_lab = round_fac(timepoint))
s4a <- ggplot() + dot_ci_layers(gwc, trt_summary(gwc, gwc, round_lab), round_lab, gwc) +
  labs(x = "Soil sampling", y = expression(Gravimetric~water~(g~g^{-1})), title = "Soil moisture at sampling")
s4b <- ggplot() + dot_ci_layers(ph, trt_summary(ph, ph, round_lab), round_lab, ph) +
  labs(x = "Soil sampling", y = "pH", title = "Soil pH")
env  <- read.csv("data/processed/chamber_env.csv") %>% mutate(date = as.Date(date))
hand <- read.csv("data/processed/field_metadata.csv") %>% mutate(date = as.Date(date))
both <- inner_join(hand %>% select(date, plot, collar, h_vwc = mean_vwc, h_t = soil_temp_c),
                   env %>% select(date, plot, collar, treatment, p_vwc = vwc, p_t = soil_temp_c),
                   by = c("date", "plot", "collar")) %>%
  mutate(h_vwc = h_vwc / 100, date_lab = format(date, "%d %b"))
cmp <- function(x, y, xl, yl, title) {
  r <- cor(both[[x]], both[[y]], use = "complete.obs")
  ggplot(both, aes(.data[[x]], .data[[y]])) +
    geom_abline(slope = 1, intercept = 0, colour = MUTED, linetype = "22", linewidth = 0.3) +
    geom_point(aes(shape = date_lab), size = 1.2, colour = INK, alpha = 0.7, stroke = 0.35) +
    annotate("text", x = -Inf, y = Inf, label = sprintf("r = %.2f, n = %d", r, sum(complete.cases(both[, c(x, y)]))),
             hjust = -0.1, vjust = 1.3, size = 2.1, colour = MUTED) +
    scale_shape_manual(values = c(1, 2, 0, 5), name = NULL) +
    labs(x = xl, y = yl, title = title)
}
s4c <- cmp("h_t", "p_t", "Handheld probe, 10 cm (°C)", "Chamber probe (°C)", "Temperature, same collars")
s4d <- cmp("h_vwc", "p_vwc", expression(Handheld~(m^3~m^{-3})), expression(Chamber~probe~(m^3~m^{-3})), "Moisture, same collars")
figs4 <- ((s4a | s4b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")) /
  ((s4c | s4d) + plot_layout(guides = "collect") & theme(legend.position = "bottom")) + tags_pub()
save_fig(figs4, "figS4_soil_conditions_instruments", 180, 125, "si")

# =============================================================================
# Fig S5: C-min time courses with all plots
# =============================================================================
ctr <- read.csv("data/processed/cmin_timeresolved.csv", colClasses = c(lab_no = "character")) %>%
  filter(is.na(flag), !is.na(cmin_rate_ug_co2c_hr_g)) %>%
  group_by(plot, treatment, timepoint, day) %>%
  summarize(rate = mean(cmin_rate_ug_co2c_hr_g), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment), round_lab = sampled_fac(timepoint))
cts <- trt_summary(ctr, rate, round_lab, day)
figs5 <- ggplot() +
  geom_line(data = ctr, aes(day, rate, colour = treatment, group = plot), linewidth = 0.25, alpha = 0.35,
            position = position_dodge(0.8), show.legend = FALSE) +
  geom_point(data = ctr, aes(day, rate, colour = treatment), shape = 16, size = 0.7, alpha = 0.45,
             position = position_dodge(0.8), show.legend = FALSE) +
  geom_linerange(data = cts, aes(day, ymin = mean - se, ymax = mean + se, colour = treatment),
                 position = position_dodge(0.8), linewidth = 0.4, show.legend = FALSE) +
  geom_line(data = cts, aes(day, mean, colour = treatment), position = position_dodge(0.8), linewidth = 0.6, show.legend = FALSE) +
  geom_point(data = cts, aes(day, mean, colour = treatment, fill = treatment, shape = treatment),
             position = position_dodge(0.8), size = 1.6, stroke = 0.35) +
  facet_wrap(~ round_lab, nrow = 1) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  scale_x_continuous(breaks = c(0, 7, 14, 21, 28)) +
  labs(x = "Day of incubation (20 °C, 65% WHC)", y = expression(CO[2]*'-C'~(mu*g~g^{-1}~h^{-1})))
save_fig(figs5, "figS5_cmin_timecourses", 180, 70, "si")

# =============================================================================
# Fig S6: N pools, day 0 vs day 28
# =============================================================================
nmin <- read.csv("data/processed/nmin_plot.csv") %>% mutate(treatment = as_trt(treatment))
pools <- nmin %>%
  select(plot, treatment, round, initial_nh4_ug_g, initial_no3_ug_g, incubated_nh4_ug_g, incubated_no3_ug_g) %>%
  pivot_longer(-c(plot, treatment, round), names_to = c("stage", "form"),
               names_pattern = "(initial|incubated)_(nh4|no3)_ug_g") %>%
  mutate(stage = factor(stage, levels = c("initial", "incubated"), labels = c("Day 0", "Day 28")),
         form = factor(form, levels = c("nh4", "no3"),
                       labels = c("NH[4]^'+'*'-N'~(mu*g~N~g^{-1})", "NO[3]^'-'*'-N'~(mu*g~N~g^{-1})")),
         round_lab = sampled_fac(round))
figs6 <- ggplot() + zero_line() +
  dot_ci_layers(pools, trt_summary(pools, value, form, round_lab, stage), stage, value) +
  facet_grid(form ~ round_lab, scales = "free_y", switch = "y", labeller = labeller(form = label_parsed)) +
  labs(x = NULL, y = NULL) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "plain", hjust = 0.5))
save_fig(figs6, "figS6_nmin_pools", 180, 95, "si")

# =============================================================================
# Fig S7: plant response
# =============================================================================
biomass <- read.csv("data/processed/biomass.csv")
forage  <- read.csv("data/processed/dairy_one_forage.csv")
plant <- biomass %>% group_by(plot, treatment) %>% summarize(biomass = mean(dry_matter_g_m2), .groups = "drop") %>%
  left_join(forage %>% select(-treatment), by = "plot") %>% mutate(treatment = as_trt(treatment))
pv_bio <- anova_p(plant, biomass)$p
s7a <- ggplot() + dot_ci_layers(plant %>% mutate(x = treatment), trt_summary(plant, biomass) %>% mutate(x = treatment), x, biomass, pt_size = 1.3) +
  annotate("text", x = 2, y = Inf, label = sprintf("p = %.2f", pv_bio), vjust = 1.3, size = 2.1, colour = MUTED) +
  trt_axis() + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL, y = expression(Dry~mass~(g~m^{-2})), title = "Aboveground biomass")

# Forage composition: every variable as a z-score across the 15 plots, so all
# share one axis; groups ordered by nutritional role.
vars <- tribble(
  ~var,                   ~label,              ~group,
  "crude_protein_pct",    "Crude protein",     "Protein",
  "avail_protein_pct",    "Available protein", "Protein",
  "adicp_pct",            "ADICP",             "Protein",
  "ndicp_pct",            "NDICP",             "Protein",
  "andf_pct",             "aNDF",              "Fibre",
  "adf_pct",              "ADF",               "Fibre",
  "lignin_pct",           "Lignin",            "Fibre",
  "nfc_pct",              "NFC",               "Carbohydrate & energy",
  "starch_pct",           "Starch",            "Carbohydrate & energy",
  "water_sol_carbs_pct",  "WSC",               "Carbohydrate & energy",
  "simple_sugars_pct",    "Simple sugars",     "Carbohydrate & energy",
  "crude_fat_pct",        "Crude fat",         "Carbohydrate & energy",
  "tdn_pct",              "TDN",               "Carbohydrate & energy",
  "ash_pct",              "Ash",               "Minerals",
  "ca_pct", "Ca", "Minerals", "p_pct", "P", "Minerals", "mg_pct", "Mg", "Minerals",
  "k_pct", "K", "Minerals", "s_pct", "S", "Minerals"
) %>% mutate(group = factor(group, levels = c("Protein", "Fibre", "Carbohydrate & energy", "Minerals")))
fz <- plant %>% select(plot, treatment, all_of(vars$var)) %>%
  pivot_longer(-c(plot, treatment), names_to = "var", values_to = "value") %>%
  group_by(var) %>% mutate(z = (value - mean(value)) / sd(value)) %>% ungroup() %>%
  left_join(vars, by = "var") %>% mutate(label = factor(label, levels = rev(vars$label)))
fstat <- fz %>% group_by(var, label, group) %>%
  summarize(p = summary(aov(value ~ treatment))[[1]][1, "Pr(>F)"], .groups = "drop") %>%
  mutate(q = p.adjust(p, "BH"), star = case_when(q < 0.05 ~ "*", p < 0.05 ~ "†", TRUE ~ ""))
fsum <- fz %>% group_by(label, group, treatment) %>%
  summarize(m = mean(z), se = sd(z) / sqrt(n()), .groups = "drop") %>% mutate(treatment = as_trt(treatment))
perm <- { set.seed(1); vegan::adonis2(scale(plant[, vars$var]) ~ treatment, data = plant,
                                      method = "euclidean", permutations = 9999) }
pd <- position_dodge(width = 0.7)
s7b <- ggplot(fsum, aes(m, label, colour = treatment)) +
  geom_vline(xintercept = 0, colour = RULE, linewidth = 0.3) +
  geom_point(data = fz %>% mutate(treatment = as_trt(treatment)), aes(z, label, colour = treatment),
             position = position_jitterdodge(jitter.height = 0.12, jitter.width = 0, dodge.width = 0.7, seed = 1),
             shape = 16, size = 0.6, alpha = 0.4, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = m - se, xmax = m + se), position = pd, height = 0, linewidth = 0.4, show.legend = FALSE) +
  geom_point(aes(fill = treatment, shape = treatment), position = pd, size = 1.5, stroke = 0.35) +
  geom_text(data = fstat, aes(x = Inf, y = label, label = star), inherit.aes = FALSE, hjust = 1.2, size = 3, colour = INK) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y",
             labeller = as_labeller(c(Protein = "Protein", Fibre = "Fibre",
                                      `Carbohydrate & energy` = "Carb. & energy", Minerals = "Minerals"))) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  labs(x = "z-score (standardized across plots)", y = NULL, title = sprintf("Forage composition (PERMANOVA p = %.2f)", perm$`Pr(>F)`[1]),
       subtitle = "Mean \u00b1 SE; \u2020 p < 0.05 unadjusted") +
  theme(strip.text.y = element_text(angle = -90, face = "plain", size = 6, colour = MUTED, hjust = 0.5),
        panel.grid.major.y = element_blank(), panel.spacing.y = unit(4, "pt"))
figs7 <- (s7a | s7b) + plot_layout(widths = c(1, 2), guides = "collect") + tags_pub() &
  theme(legend.position = "bottom")
save_fig(figs7, "figS7_plant_response", 180, 125, "si")

# Append plant metrics to the treatment-effects table
eff <- read.csv("output/tables/treatment_effects.csv")
plant_eff <- bind_rows(
  diff_vs_control(plant, biomass) %>% mutate(metric = "biomass_g_m2", anova_p = pv_bio),
  bind_rows(lapply(vars$var, function(v) diff_vs_control(plant, !!sym(v)) %>%
    mutate(metric = v, anova_p = fstat$p[fstat$var == v])))
) %>% transmute(metric, group = "Oct harvest", treatment, control_mean = signif(control_mean, 3),
                diff = signif(diff, 3), ci_lo = signif(lo, 3), ci_hi = signif(hi, 3),
                pct_of_control = round(pct, 1), welch_p = round(p, 3), anova_p = round(anova_p, 3))
write.csv(bind_rows(eff %>% filter(group != "Oct harvest"), plant_eff), "output/tables/treatment_effects.csv", row.names = FALSE)
cat(sprintf("  appended plant metrics to treatment_effects.csv; forage PERMANOVA p = %.3f\n", perm$`Pr(>F)`[1]))
