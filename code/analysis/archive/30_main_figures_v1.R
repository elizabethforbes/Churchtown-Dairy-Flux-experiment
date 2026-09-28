# 30_main_figures.R
# Main-text figures (output/figures/main) and the treatment-effects table.
#   Fig 1  Study timeline and amendment N composition
#   Fig 2  Soil GHG fluxes: time series, difference from control, season totals
#   Fig 3  Soil biogeochemistry across sampling rounds (N pools, N and C
#          mineralization, SIR); Fig 3 alt = difference from control
#   Fig 4  Aboveground biomass and forage quality
# Statistics: plots are the experimental unit (n = 5). Subsamples (collars,
# tubes) are averaged within plot first. Error bars are 95% CI of the mean;
# differences from control use Welch 95% CI.

source("code/analysis/fig_setup.R")
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
effects <- list()   # collects rows for output/tables/treatment_effects.csv

# =============================================================================
# Fig 1: timeline + amendments
# =============================================================================
flux_raw <- read.csv("data/processed/flux_estimates.csv") %>% mutate(date = as.Date(date))
biomass  <- read.csv("data/processed/biomass.csv") %>% mutate(sampling_date = as.Date(sampling_date))
manure   <- read.csv("data/processed/dairy_one_manure.csv")

rows <- c("Manure applied", "GHG flux", "Soil sampling", "Lab incubations", "Biomass harvest")
ev_points <- bind_rows(
  tibble(row = "Manure applied", date = APPLICATION_DATE, kind = "event"),
  tibble(row = "GHG flux", date = sort(unique(flux_raw$date))) %>%
    mutate(kind = if_else(date < APPLICATION_DATE, "pre", "post")),
  tibble(row = "Soil sampling", date = as.Date(ROUND_DATES), kind = "event"),
  tibble(row = "Biomass harvest", date = unique(biomass$sampling_date), kind = "event")
) %>% mutate(row = factor(row, levels = rev(rows)))
incub <- tibble(row = factor("Lab incubations", levels = rev(rows)),
                start = as.Date(c("2025-06-03", "2025-08-04", "2025-11-25")),
                end   = as.Date(c("2025-07-02", "2025-09-03", "2025-12-23")),
                label = c("Round 1", "Round 2", "Round 3"))
sampling_links <- tibble(round = 1:3, from = as.Date(ROUND_DATES), to = incub$start,
                         y0 = factor("Soil sampling", levels = rev(rows)),
                         y1 = factor("Lab incubations", levels = rev(rows)))

f1a <- ggplot() +
  geom_vline(xintercept = APPLICATION_DATE, colour = MUTED, linewidth = 0.3, linetype = "22") +
  geom_segment(data = sampling_links, aes(x = from, xend = to, y = y0, yend = y1),
               colour = RULE, linewidth = 0.3) +
  geom_segment(data = incub, aes(x = start, xend = end, y = row, yend = row),
               linewidth = 2.2, colour = "grey35", lineend = "butt") +
  geom_text(data = incub, aes(x = start + (end - start) / 2, y = row, label = label),
            vjust = -1.1, size = 2.3, colour = INK) +
  geom_point(data = ev_points, aes(x = date, y = row, shape = kind), size = 1.8,
             colour = INK, fill = "white", stroke = 0.45) +
  scale_shape_manual(values = c(event = 18, pre = 21, post = 16),
                     labels = c(pre = "Pre-application flux", post = "Post-application flux"),
                     breaks = c("pre", "post"), name = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(add = c(8, 8))) +
  scale_y_discrete(limits = rev(rows)) +
  labs(x = "2025", y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "top")

man <- manure %>%
  mutate(treatment = as_trt(amendment_type),
         `Organic N` = organic_n_pct * 10, `Ammonium N` = ammonium_n_pct * 10,
         total_n = total_n_pct * 10)
man_long <- man %>% select(treatment, `Organic N`, `Ammonium N`) %>%
  pivot_longer(-treatment, names_to = "form", values_to = "kg") %>%
  group_by(treatment, form) %>% summarize(kg = mean(kg), .groups = "drop") %>%
  mutate(form = factor(form, levels = c("Ammonium N", "Organic N")),
         fillkey = paste(treatment, form))
fill_vals <- c(setNames(TRT_COLS[c("compost", "slurry")], paste(c("compost", "slurry"), "Organic N")),
               setNames(colorspace::lighten(TRT_COLS[c("compost", "slurry")], 0.6),
                        paste(c("compost", "slurry"), "Ammonium N")))
f1b <- ggplot(man_long, aes(treatment, kg, fill = fillkey)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
  geom_point(data = man, aes(treatment, total_n), inherit.aes = FALSE,
             position = position_nudge(x = 0.42), size = 1, colour = INK, shape = 16) +
  geom_text(data = man_long %>% group_by(treatment) %>%
              summarize(nh4 = kg[form == "Ammonium N"] / sum(kg), top = sum(kg)),
            aes(treatment, top, label = sprintf("NH[4]^'+'*'-N:'~'%.0f%%'", 100 * nh4)),
            parse = TRUE, inherit.aes = FALSE, vjust = -0.5, size = 2.1, colour = INK) +
  scale_fill_manual(values = fill_vals, guide = "none") +
  scale_x_discrete(labels = TRT_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = expression(Total~N~(kg~N~Mg^{-1}~fresh)),
       title = "Amendment N", subtitle = "Light = ammonium N; dots = samples")

f1c <- ggplot(man, aes(treatment, total_solids_pct, colour = treatment, fill = treatment, shape = treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6, alpha = 0.25, colour = NA) +
  geom_point(size = 1.4, stroke = 0.4) +
  scale_colour_trt(guide = "none") + scale_fill_trt(guide = "none") + scale_shape_trt(guide = "none") +
  scale_x_discrete(labels = TRT_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)), limits = c(0, NA)) +
  labs(x = NULL, y = "Total solids (% fresh mass)", title = "Dry matter")

fig1 <- f1a / free(f1b | f1c) + plot_layout(heights = c(1, 1.2)) + tags_pub()
save_fig(fig1, "fig1_design_amendments", 180, 110)

# =============================================================================
# Fig 2: GHG fluxes
# =============================================================================
gases <- tribble(
  ~col,       ~gas,  ~ylab,                                              ~dlab,                                                        ~cum_factor,       ~cum_lab,
  "FCO2_DRY", "CO2", "expression(CO[2]~(mu*mol~m^{-2}~s^{-1}))",         "expression(Delta*CO[2]~(mu*mol~m^{-2}~s^{-1}))",             86400 * 12.011e-6, "expression(CO[2]*'-C'~(g~m^{-2}))",
  "FCH4_DRY", "CH4", "expression(CH[4]~(nmol~m^{-2}~s^{-1}))",           "expression(Delta*CH[4]~(nmol~m^{-2}~s^{-1}))",               86400 * 12.011e-6, "expression(CH[4]*'-C'~(mg~m^{-2}))",
  "FN2O",     "N2O", "expression(N[2]*O~(nmol~m^{-2}~s^{-1}))",          "expression(Delta*N[2]*O~(nmol~m^{-2}~s^{-1}))",              86400 * 28.013e-6, "expression(N[2]*O*'-N'~(mg~m^{-2}))"
)
gas_title <- c(CO2 = "expression(bold(CO[2]))", CH4 = "expression(bold(CH[4]))", N2O = "expression(bold(N[2]*O))")

flux_plot <- flux_raw %>%
  group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))

post <- flux_plot %>% filter(date > APPLICATION_DATE)
season_days <- as.numeric(max(post$date) - min(post$date))
cum_plot <- post %>%
  arrange(date) %>%
  group_by(plot, treatment) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), function(v) {
    t <- as.numeric(date - min(date)); ok <- !is.na(v)
    sum(diff(t[ok]) * (head(v[ok], -1) + tail(v[ok], -1)) / 2)   # trapezoid, flux x days
  }), .groups = "drop")

pdodge <- function(d) position_dodge(width = 2.5)
flux_rows <- lapply(seq_len(nrow(gases)), function(i) {
  g <- gases[i, ]; v <- sym(g$col)
  ts <- trt_summary(flux_plot, !!v, date)
  dv <- diff_vs_control(flux_plot, !!v, date)
  cp <- cum_plot %>% mutate(val = !!v * g$cum_factor)
  cs <- trt_summary(cp, val)
  pv <- anova_p(cp, val)$p
  dc <- diff_vs_control(cp, val) %>% mutate(metric = paste0("cumulative_", g$gas))
  effects[[paste0("flux_", g$gas)]] <<- dc %>% mutate(group = "Season", anova_p = pv)

  first <- i == 1; last <- i == nrow(gases)
  p_ts <- ggplot(ts, aes(date, mean, colour = treatment)) +
    annotate("rect", xmin = -Inf, xmax = APPLICATION_DATE, ymin = -Inf, ymax = Inf, fill = "grey95") +
    application_line() +
    { if (g$gas != "CO2") zero_line() } +
    geom_linerange(aes(ymin = mean - se, ymax = mean + se), position = pdodge(), linewidth = 0.35, show.legend = FALSE) +
    geom_line(position = pdodge(), linewidth = 0.4, show.legend = FALSE) +
    geom_point(aes(fill = treatment, shape = treatment), position = pdodge(), size = 1.3, stroke = 0.35) +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(expand = expansion(mult = 0.08)) +
    labs(x = NULL, y = eval(parse(text = g$ylab)),
         title = if (first) "Flux (mean \u00b1 SE)" else NULL)
  p_d <- ggplot(dv, aes(date, diff, colour = treatment)) +
    annotate("rect", xmin = -Inf, xmax = APPLICATION_DATE, ymin = -Inf, ymax = Inf, fill = "grey95") +
    application_line() + zero_line() +
    geom_linerange(aes(ymin = lo, ymax = hi), position = pdodge(), linewidth = 0.35, show.legend = FALSE) +
    geom_line(position = pdodge(), linewidth = 0.4, show.legend = FALSE) +
    geom_point(aes(fill = treatment, shape = treatment), position = pdodge(), size = 1.3, stroke = 0.35) +
    scale_colour_trt(drop = FALSE) + scale_fill_trt(drop = FALSE) + scale_shape_trt(drop = FALSE) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(expand = expansion(mult = 0.08)) +
    labs(x = NULL, y = eval(parse(text = g$dlab)),
         title = if (first) "Amendment \u2212 control (95% CI)" else NULL)
  p_c <- ggplot() +
    { if (g$gas != "CO2") zero_line() } +
    dot_ci_layers(cp %>% mutate(x = treatment), cs %>% mutate(x = treatment), x, val) +
    annotate("text", x = 2, y = Inf, label = sprintf("ANOVA p = %.2f", pv), vjust = 1.2,
             size = 2.2, colour = MUTED) +
    trt_axis() +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = NULL, y = eval(parse(text = g$cum_lab)),
         title = if (first) "Season total" else NULL,
         subtitle = if (first) sprintf("%s\u2013%s", format(min(post$date), "%d %b"),
                                       format(max(post$date), "%d %b")) else NULL)
  no_leg <- guides(colour = "none", fill = "none", shape = "none")
  list(p_ts, p_d + no_leg, p_c + no_leg)
})
fig2 <- wrap_plots(unlist(flux_rows, recursive = FALSE), ncol = 3, widths = c(1.4, 1.4, 0.75)) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig2, "fig2_ghg_fluxes", 180, 150)

# =============================================================================
# Fig 3: soil biogeochemistry across rounds
# =============================================================================
lab  <- read.csv("data/processed/lab_assays_summary.csv")
nmin <- read.csv("data/processed/nmin_plot.csv")
soil <- lab %>%
  select(plot, treatment, round = timepoint, sir_ug_co2c_hr_g, cmin_rate_ug_co2c_g_d) %>%
  left_join(nmin %>% select(plot, round, initial_nh4_ug_g, initial_no3_ug_g,
                            net_min_rate_ug_g_d, net_nitr_rate_ug_g_d), by = c("plot", "round")) %>%
  mutate(treatment = as_trt(treatment),
         round_lab = factor(ROUND_LABELS[as.character(round)], levels = ROUND_LABELS))

soil_metrics <- tribble(
  ~col,                    ~title,                          ~ylab,
  "initial_nh4_ug_g",      "Extractable ammonium",          "expression(NH[4]^'+'*'-N'~(mu*g~N~g^{-1}))",
  "initial_no3_ug_g",      "Extractable nitrate",           "expression(NO[3]^'-'*'-N'~(mu*g~N~g^{-1}))",
  "net_min_rate_ug_g_d",   "Net N mineralization",          "expression(mu*g~N~g^{-1}~d^{-1})",
  "net_nitr_rate_ug_g_d",  "Net nitrification",             "expression(mu*g~N~g^{-1}~d^{-1})",
  "sir_ug_co2c_hr_g",      "Substrate-induced resp.", "expression(mu*g~CO[2]*'-C'~g^{-1}~h^{-1})",
  "cmin_rate_ug_co2c_g_d", "C mineralization (28 d)",       "expression(mu*g~CO[2]*'-C'~g^{-1}~d^{-1})"
)

soil_abs <- list(); soil_rel <- list()
for (i in seq_len(nrow(soil_metrics))) {
  m <- soil_metrics[i, ]; v <- sym(m$col)
  s  <- trt_summary(soil, !!v, round_lab)
  dv <- diff_vs_control(soil, !!v, round_lab)
  pv <- anova_p(soil, !!v, round_lab)
  effects[[m$col]] <- dv %>% mutate(metric = m$col, group = as.character(round_lab)) %>%
    left_join(pv %>% transmute(group = as.character(round_lab), anova_p = p), by = "group") %>%
    select(-round_lab)
  soil_abs[[i]] <- ggplot() +
    { if (grepl("net_", m$col)) zero_line() } +
    dot_ci_layers(soil %>% filter(!is.na(!!v)), s, round_lab, !!v) +
    geom_text(data = pv, aes(round_lab, Inf, label = ifelse(p < 0.05, sprintf("p = %.2f", p), "")),
              vjust = 1.2, size = 2.1, colour = INK) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = NULL, y = eval(parse(text = m$ylab)), title = m$title)
  soil_rel[[i]] <- ggplot(dv, aes(round_lab, diff, colour = treatment)) +
    zero_line() +
    geom_linerange(aes(ymin = lo, ymax = hi), position = position_dodge(0.45), linewidth = 0.45, show.legend = FALSE) +
    geom_point(aes(fill = treatment, shape = treatment), position = position_dodge(0.45),
               size = 2, stroke = 0.4) +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
    labs(x = NULL, y = eval(parse(text = m$ylab)), title = m$title)
}
fig3 <- wrap_plots(soil_abs, ncol = 3) + plot_layout(guides = "collect") + tags_pub() &
  theme(legend.position = "bottom")
save_fig(fig3, "fig3_soil_biogeochemistry", 180, 120)
fig3r <- wrap_plots(soil_rel, ncol = 3) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a",
                  caption = "Amendment minus control (mean difference, Welch 95% CI; n = 5 plots per treatment)") &
  theme(legend.position = "bottom", plot.caption = element_text(colour = MUTED, size = 6.5, hjust = 0))
save_fig(fig3r, "fig3_soil_biogeochemistry_vs_control", 180, 120)

# =============================================================================
# Fig 4: plant response
# =============================================================================
forage <- read.csv("data/processed/dairy_one_forage.csv") %>% mutate(treatment = as_trt(treatment))
plant <- biomass %>% group_by(plot, treatment) %>%
  summarize(biomass = mean(dry_matter_g_m2), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment)) %>%
  left_join(forage %>% select(plot, crude_protein_pct, andf_pct, rfv), by = "plot")
plant_metrics <- tribble(
  ~col,                ~title,                 ~ylab,
  "biomass",           "Biomass",        "expression(Dry~mass~(g~m^{-2}))",
  "crude_protein_pct", "Crude protein",  "'Crude protein (% DM)'",
  "andf_pct",          "Fibre (aNDF)",   "'aNDF (% DM)'"
)
plant_panels <- lapply(seq_len(nrow(plant_metrics)), function(i) {
  m <- plant_metrics[i, ]; v <- sym(m$col)
  s <- trt_summary(plant, !!v); pv <- anova_p(plant, !!v)$p
  effects[[m$col]] <<- diff_vs_control(plant, !!v) %>% mutate(metric = m$col, group = "Oct harvest", anova_p = pv)
  ggplot() + dot_ci_layers(plant %>% mutate(x = treatment), s %>% mutate(x = treatment), x, !!v) +
    annotate("text", x = 2, y = Inf, label = sprintf("ANOVA p = %.2f", pv), vjust = 1.2, size = 2.1, colour = MUTED) +
    trt_axis() +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = NULL, y = eval(parse(text = m$ylab)), title = m$title)
})

pca_vars <- forage %>% select(crude_protein_pct, avail_protein_pct, adicp_pct, adf_pct, andf_pct,
                              crude_fat_pct, tdn_pct, ca_pct, p_pct, mg_pct, k_pct, s_pct,
                              ash_pct, lignin_pct, ndicp_pct, starch_pct, nfc_pct,
                              water_sol_carbs_pct, simple_sugars_pct)
pca <- prcomp(pca_vars, center = TRUE, scale. = TRUE)
ve <- round(100 * pca$sdev^2 / sum(pca$sdev^2))
sc <- as_tibble(pca$x[, 1:2]) %>% mutate(treatment = forage$treatment)
ld <- as_tibble(pca$rotation[, 1:2], rownames = "var") %>%
  mutate(mag = sqrt(PC1^2 + PC2^2)) %>% slice_max(mag, n = 6) %>%
  mutate(k = 0.85 * max(abs(sc$PC1)) / max(abs(PC1)),
         label = recode(sub("_pct$", "", var), crude_protein = "CP", avail_protein = "avail. CP",
                        andf = "aNDF", adf = "ADF", tdn = "TDN", nfc = "NFC", ndicp = "NDICP",
                        adicp = "ADICP", water_sol_carbs = "WSC", simple_sugars = "sugars",
                        crude_fat = "fat", lignin = "lignin", ash = "ash", starch = "starch",
                        ca = "Ca", p = "P", mg = "Mg", k = "K", s = "S"))
p_pca <- ggplot(sc, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = RULE, linewidth = 0.25) +
  geom_vline(xintercept = 0, colour = RULE, linewidth = 0.25) +
  geom_segment(data = ld, aes(0, 0, xend = PC1 * k, yend = PC2 * k), colour = MUTED,
               linewidth = 0.3, arrow = arrow(length = unit(1.5, "pt"))) +
  geom_text(data = ld, aes(PC1 * k * 1.15, PC2 * k * 1.15, label = label), size = 2, colour = MUTED) +
  geom_point(aes(colour = treatment, fill = treatment, shape = treatment), size = 1.8, stroke = 0.4) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  labs(x = sprintf("PC1 (%d%%)", ve[1]), y = sprintf("PC2 (%d%%)", ve[2]), title = "Forage composition") +
  theme(panel.grid.major.y = element_blank())
fig4 <- (plant_panels[[1]] | plant_panels[[2]] | plant_panels[[3]] | p_pca) +
  plot_layout(widths = c(1, 1, 1, 1.35), guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig4, "fig4_plant_response", 180, 70)

# =============================================================================
# Treatment-effects table
# =============================================================================
eff <- bind_rows(effects) %>%
  transmute(metric, group, treatment, control_mean = signif(control_mean, 3),
            diff = signif(diff, 3), ci_lo = signif(lo, 3), ci_hi = signif(hi, 3),
            pct_of_control = round(pct, 1), welch_p = round(p, 3), anova_p = round(anova_p, 3))
write.csv(eff, "output/tables/treatment_effects.csv", row.names = FALSE)
cat("  wrote output/tables/treatment_effects.csv\n")
