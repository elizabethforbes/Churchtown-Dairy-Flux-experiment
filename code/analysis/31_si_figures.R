# 31_si_figures.R
# Supplementary figures (output/figures/si). Run after 30_main_figures.R.
#   Fig 1   Study design: plot map and timeline (main text)
#   Fig S1  Amendment composition
#   Fig S2  Flux drivers (written by 30_main_figures.R)
#   Fig S3  Soil pH, organic matter, CEC, base saturation and Mehlich-3 nutrients by sampling round
#   Fig S4  Soil moisture and pH at sampling; temperature-moisture covariation (handheld probe)
#   Fig S5  C mineralization time courses (all plots shown)
#   Fig S6  Extractable N pools, day 0 vs day 28
# Also appends plant and soil-test effects to treatment_effects.csv (used by Fig 4).

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
  scale_fill_manual(values = fills, guide = "none") + scale_x_discrete(labels = TRT_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = expression(Total~N~(kg~Mg^{-1}~fresh~mass)))
dot_amend <- function(v, ylab) ggplot(manure, aes(treatment, {{ v }}, colour = treatment, fill = treatment, shape = treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6, alpha = 0.25, colour = NA) +
  geom_point(size = 1.5, stroke = 0.4) +
  scale_colour_trt(guide = "none") + scale_fill_trt(guide = "none") + scale_shape_trt(guide = "none") +
  scale_x_discrete(labels = TRT_LABELS) + scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, NA)) +
  labs(x = NULL, y = ylab)
s1b <- dot_amend(total_solids_pct, "Total solids (% of fresh mass)")
s1c <- dot_amend(n_dry, "Total N (% of dry mass)")
# plot map from RTK-GPS corners and collars (UTM 18N), relative to the south-west corner
key <- read.csv("data/processed/treatment_key.csv")
corners <- read.csv("data/raw/field_metadata/KT-CTD-Plots.csv") %>% transmute(plot = as.integer(Name), E = Easting, N = Northing, z = Elevation)
collars <- read.csv("data/raw/field_metadata/KT-CTD-Collars.csv") %>% transmute(plot = as.integer(sub("[A-C]$", "", Name)), E = Easting, N = Northing)
E0 <- min(corners$E); N0 <- min(corners$N); zmin <- min(corners$z)
poly <- corners %>% group_by(plot) %>% mutate(ang = atan2(N - mean(N), E - mean(E))) %>% arrange(plot, ang) %>% ungroup() %>%
  left_join(key, by = "plot") %>% mutate(x = E - E0, y = N - N0, treatment = as_trt(treatment))
cent <- poly %>% group_by(plot, treatment) %>% summarize(x = mean(x), y = mean(y), dz = mean(z) - zmin, .groups = "drop")
s1map <- ggplot() +
  geom_polygon(data = poly, aes(x, y, group = plot, fill = treatment), colour = INK, linewidth = 0.25, alpha = 0.35) +
  geom_point(data = collars %>% mutate(x = E - E0, y = N - N0), aes(x, y), shape = 16, size = 0.5, colour = INK) +
  geom_text(data = cent, aes(x - 2.4, y + 0.6, label = plot), size = 2.3, fontface = "bold", colour = INK, hjust = 1) +
  geom_text(data = cent, aes(x - 2.4, y - 0.7, label = sprintf("%.2f", dz)), size = 1.7, colour = MUTED, hjust = 1) +
  annotate("segment", x = max(poly$x) + 2, xend = max(poly$x) + 2, y = 0, yend = 4,
           arrow = arrow(length = unit(3, "pt")), linewidth = 0.3, colour = INK) +
  annotate("text", x = max(poly$x) + 2, y = 5, label = "N", size = 2.2, colour = INK) +
  annotate("segment", x = 0, xend = 5, y = -2, yend = -2, linewidth = 0.5, colour = INK) +
  annotate("text", x = 2.5, y = -3.2, label = "5 m", size = 2, colour = INK) +
  coord_equal(clip = "off") + scale_fill_trt() +
  labs(x = NULL, y = NULL) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid.major.y = element_blank())

# timeline of manure application, flux campaigns, soil sampling and harvest
flux_dates <- sort(unique(as.Date(read.csv("data/processed/flux_estimates.csv")$date)))
harvest <- unique(as.Date(read.csv("data/processed/biomass.csv")$sampling_date))
rows <- c("Manure applied", "GHG flux", "Soil sampling", "Biomass harvest")
ev_points <- bind_rows(
  tibble(row = "Manure applied", date = APPLICATION_DATE, kind = "event"),
  tibble(row = "GHG flux", date = flux_dates) %>% mutate(kind = if_else(date < APPLICATION_DATE, "pre", "post")),
  tibble(row = "Soil sampling", date = as.Date(ROUND_DATES), kind = "event"),
  tibble(row = "Biomass harvest", date = harvest, kind = "event"))
s1time <- ggplot(ev_points, aes(date, row)) +
  geom_vline(xintercept = APPLICATION_DATE, colour = MUTED, linewidth = 0.3, linetype = "22") +
  geom_point(aes(shape = kind), size = 1.7, colour = INK, fill = "white", stroke = 0.45) +
  scale_shape_manual(values = c(event = 18, pre = 21, post = 16), labels = c(pre = "Before application", post = "After application"),
                     breaks = c("pre", "post"), name = "Flux campaigns") +
  scale_y_discrete(limits = rev(rows)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = as.Date(c("2025-05-01", "2025-10-31")), expand = expansion(0)) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "inside", legend.position.inside = c(0.99, 0.02), legend.justification = c(1, 0),
        legend.direction = "vertical", legend.background = element_blank())

fig1 <- ((s1map + theme(legend.position = "bottom")) | s1time) + plot_layout(widths = c(1.25, 1)) + tags_pub()
save_fig(fig1, "fig1_design", 180, 95)
figs1 <- (s1a | s1b | s1c) + tags_pub()
save_fig(figs1, "figS1_amendment_composition", 180, 70, "si")

# =============================================================================
# Fig S4: soil moisture and pH at sampling; instrument comparison
# =============================================================================
gwc <- read.csv("data/processed/gwc.csv") %>% group_by(plot, treatment, timepoint) %>%
  summarize(gwc = mean(gwc), .groups = "drop") %>% mutate(treatment = as_trt(treatment), round_lab = round_fac(timepoint))
ph <- read.csv("data/processed/ph.csv") %>% group_by(plot, treatment, timepoint) %>%
  summarize(ph = mean(ph, na.rm = TRUE), .groups = "drop") %>% mutate(treatment = as_trt(treatment), round_lab = round_fac(timepoint))
s4a <- ggplot() + dot_ci_layers(gwc, trt_summary(gwc, gwc, round_lab), round_lab, gwc) +
  labs(x = "Soil sampling", y = expression(Gravimetric~water~(g~g^{-1})))
s4b <- ggplot() + dot_ci_layers(ph, trt_summary(ph, ph, round_lab), round_lab, ph) +
  labs(x = "Soil sampling", y = "pH (1:1 water)")

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
# Plant response statistics (effects feed main Fig 4)
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

# Forage composition as % difference from the control mean: keeps each
# variable's effect size interpretable (z-scores hide magnitude). Points are
# individual amendment plots; symbols are the mean difference with Welch 95% CI.
vars <- tribble(
  ~var,                   ~label,              ~group,
  "crude_protein_pct",    "Crude protein",     "Protein",
  "avail_protein_pct",    "Available protein", "Protein",
  "adicp_pct",            "ADICP",             "Protein",
  "ndicp_pct",            "NDICP",             "Protein",
  "andf_pct",             "aNDF",              "Fibre",
  "adf_pct",              "ADF",               "Fibre",
  "lignin_pct",           "Lignin",            "Fibre",
  "nfc_pct",              "NFC",               "Carbohydrate",
  "starch_pct",           "Starch",            "Carbohydrate",
  "water_sol_carbs_pct",  "WSC",               "Carbohydrate",
  "simple_sugars_pct",    "Simple sugars",     "Carbohydrate",
  "crude_fat_pct",        "Crude fat",         "Energy",
  "tdn_pct",              "TDN",               "Energy",
  "ash_pct",              "Ash",               "Minerals",
  "ca_pct", "Ca", "Minerals", "p_pct", "P", "Minerals", "mg_pct", "Mg", "Minerals",
  "k_pct", "K", "Minerals", "s_pct", "S", "Minerals"
) %>% mutate(group = factor(group, levels = c("Protein", "Fibre", "Carbohydrate", "Energy", "Minerals")))
flong <- plant %>% select(plot, treatment, all_of(vars$var)) %>%
  pivot_longer(-c(plot, treatment), names_to = "var", values_to = "value") %>%
  left_join(vars, by = "var")
ctl_mean <- flong %>% filter(treatment == "control") %>% group_by(var) %>% summarize(cm = mean(value))
fpts <- flong %>% left_join(ctl_mean, by = "var") %>% mutate(pct = 100 * (value / cm - 1)) %>%
  filter(treatment != "control") %>% mutate(label = factor(label, levels = rev(vars$label)))
fdiff <- bind_rows(lapply(vars$var, function(v) diff_vs_control(plant, !!sym(v)) %>% mutate(var = v))) %>%
  left_join(vars, by = "var") %>% mutate(label = factor(label, levels = rev(vars$label)))
fstat <- flong %>% group_by(var) %>%
  summarize(p = summary(aov(value ~ treatment))[[1]][1, "Pr(>F)"], .groups = "drop") %>%
  mutate(q = p.adjust(p, "BH")) %>% left_join(vars, by = "var")
perm <- { set.seed(1); vegan::adonis2(scale(plant[, vars$var]) ~ treatment, data = plant,
                                      method = "euclidean", permutations = 9999) }
n_sig <- sum(fstat$q < 0.05)
pd <- position_dodge(width = 0.6)
s7b <- ggplot(fdiff, aes(pct, label, colour = treatment)) +
  geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.3, linetype = "22") +
  geom_point(data = fpts, aes(pct, label, colour = treatment),
             position = position_jitterdodge(jitter.height = 0.1, jitter.width = 0, dodge.width = 0.6, seed = 1),
             shape = 16, size = 0.7, alpha = 0.45, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = pct_lo, xmax = pct_hi), position = pd, height = 0, linewidth = 0.45, show.legend = FALSE) +
  geom_point(aes(fill = treatment, shape = treatment), position = pd, size = 1.6, stroke = 0.35) +
  { if (n_sig > 0) geom_text(data = fstat %>% filter(q < 0.05) %>% mutate(label = factor(label, levels = rev(vars$label))),
                             aes(x = Inf, y = label, label = "*"), inherit.aes = FALSE, hjust = 1.5, size = 3.5) } +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  guides(colour = "none", fill = "none", shape = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), breaks = seq(-100, 200, 50)) +
  coord_cartesian(xlim = c(-100, 200)) +
  labs(x = "Difference from control mean", y = NULL, title = "Forage composition",
       subtitle = sprintf("PERMANOVA p = %.2f; %s", perm$`Pr(>F)`[1],
                          if (n_sig == 0) "none differ after FDR" else paste(n_sig, "differ after FDR (*)"))) +
  theme(strip.text.y = element_text(angle = 0, face = "bold", size = 6.5, hjust = 0),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.25),
        panel.spacing.y = unit(7, "pt"))
# (plant-response panels are no longer saved; biomass and forage effects are shown in main Fig 4)

hand <- read.csv("data/processed/field_metadata.csv") %>% mutate(date = as.Date(date))

# =============================================================================
# Fig S4 (c): temperature-moisture covariation in the handheld probe data
# =============================================================================
cov_panel <- function(d, x, y, title) {
  cm <- d %>% group_by(date) %>% summarize(x = mean(.data[[x]], na.rm = TRUE), y = mean(.data[[y]], na.rm = TRUE)) %>% filter(!is.nan(x), !is.nan(y))
  ok <- complete.cases(d[[x]], d[[y]])
  ggplot(d, aes(.data[[x]], .data[[y]])) +
    geom_point(colour = "grey70", shape = 16, size = 0.6, alpha = 0.6) +
    geom_point(data = cm, aes(x, y), colour = INK, size = 1.6) +
    labs(x = "Soil temperature, 10 cm (°C)", y = expression(VWC~(m^3~m^{-3})))
}
s4e <- cov_panel(hand %>% filter(date > APPLICATION_DATE) %>% mutate(W = mean_vwc / 100), "soil_temp_c", "W", "Handheld soil probe")
# (The chamber probe is used only to gap-fill handheld temperatures, 13_gapfill_soil_temp.R.)
figs4 <- ((s4a | s4b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")) /
  (s4e | plot_spacer()) + plot_layout(heights = c(1, 1)) + tags_pub()
save_fig(figs4, "figS4_soil_conditions", 180, 125, "si")

# =============================================================================
# Fig S3: Dairy One soil tests by sampling round
# =============================================================================
d1 <- read.csv("data/processed/dairy_one_clean.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(c("May", "Jul", "Oct")[timepoint], levels = c("May", "Jul", "Oct")))
d1_vars <- tribble(
  ~col,                  ~title,                 ~ylab,
  "ph",                  "pH (Dairy One)",       "'pH'",
  "om_pct",              "Organic matter",       "'Organic matter (%, LOI)'",
  "cec_meq100g",         "CEC",                  "expression(CEC~(meq~100~g^{-1}))",
  "base_sat_total_pct",  "Base saturation",      "'Base saturation (%)'",
  "p_ppm",               "Phosphorus",           "'Mehlich-3 P (ppm)'",
  "k_ppm",               "Potassium",            "'Mehlich-3 K (ppm)'",
  "ca_ppm",              "Calcium",              "'Mehlich-3 Ca (ppm)'",
  "mg_ppm",              "Magnesium",            "'Mehlich-3 Mg (ppm)'"
)
s9 <- lapply(seq_len(nrow(d1_vars)), function(i) {
  m <- d1_vars[i, ]; v <- sym(m$col)
  pv <- anova_p(d1, !!v, round_lab)
  ggplot() + dot_ci_layers(d1, trt_summary(d1, !!v, round_lab), round_lab, !!v) +
    geom_text(data = pv, aes(round_lab, Inf, label = ifelse(p < 0.05, "*", "")), vjust = 1.1, size = 3.2) +
    labs(x = NULL, y = ev(m$ylab))
})
figs9 <- wrap_plots(s9, ncol = 4) + plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(figs9, "figS3_soil_chemistry", 180, 105, "si")

# =============================================================================
# Append plant and soil-test metrics to the treatment-effects table
# =============================================================================
eff <- read.csv("output/tables/treatment_effects.csv")
fmt <- function(d, grp) d %>% transmute(metric, group = grp, treatment, control_mean = signif(control_mean, 3),
                                        diff = signif(diff, 3), ci_lo = signif(lo, 3), ci_hi = signif(hi, 3),
                                        pct_of_control = round(pct, 1), welch_p = round(p, 3), anova_p = round(anova_p, 3),
            hedges_g = round(hedges_g, 2), g_lo = round(g_lo, 2), g_hi = round(g_hi, 2))
plant_eff <- bind_rows(
  diff_vs_control(plant, biomass) %>% mutate(metric = "biomass_g_m2", anova_p = pv_bio),
  bind_rows(lapply(vars$var, function(v) diff_vs_control(plant, !!sym(v)) %>%
    mutate(metric = v, anova_p = fstat$p[fstat$var == v])))) %>% fmt("Oct harvest")
soiltest_eff <- bind_rows(lapply(d1_vars$col, function(v) {
  pv <- anova_p(d1, !!sym(v), round_lab)
  diff_vs_control(d1, !!sym(v), round_lab) %>% left_join(pv, by = "round_lab") %>%
    mutate(metric = paste0("dairyone_", v), anova_p = p.y, p = p.x, grp = as.character(round_lab))
})) %>% { d <- .; bind_rows(lapply(split(d, d$grp), function(x) fmt(x, x$grp[1]))) }
eff <- eff %>% filter(group != "Oct harvest", !grepl("^dairyone_", metric))
write.csv(bind_rows(eff, plant_eff, soiltest_eff), "output/tables/treatment_effects.csv", row.names = FALSE)
cat(sprintf("  appended plant + soil-test metrics; forage PERMANOVA p = %.3f; forage variables significant after FDR: %d\n",
            perm$`Pr(>F)`[1], n_sig))
