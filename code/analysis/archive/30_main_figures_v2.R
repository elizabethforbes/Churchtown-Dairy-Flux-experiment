# 30_main_figures.R
# Main-text figures (output/figures/main) and the treatment-effects table.
#   Fig 1  Study timeline and field conditions through the season
#   Fig 2  Soil GHG fluxes: plot-level time series and season totals
#   Fig 3  Flux responses to temperature and moisture, by treatment
#   Fig 4  Soil N and C cycling across sampling rounds
# Statistics: plots are the experimental unit (n = 5). Subsamples (collars,
# tubes) are averaged within plot first. Error bars are 95% CI of the mean
# unless noted. Difference-from-control versions are in the SI (31_si_figures.R).

source("code/analysis/fig_setup.R")
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
effects <- list()   # rows for output/tables/treatment_effects.csv

flux_raw <- read.csv("data/processed/flux_estimates.csv") %>% mutate(date = as.Date(date))
env      <- read.csv("data/processed/chamber_env.csv") %>% mutate(date = as.Date(date))
hand     <- read.csv("data/processed/field_metadata.csv") %>% mutate(date = as.Date(date))
biomass  <- read.csv("data/processed/biomass.csv") %>% mutate(sampling_date = as.Date(sampling_date))

gases <- tribble(
  ~col,       ~gas,  ~name,                        ~ylab,                                         ~cum_factor,       ~cum_lab,
  "FCO2_DRY", "CO2", "expression(bold(CO[2]))",     "expression(CO[2]~(mu*mol~m^{-2}~s^{-1}))",   86400 * 12.011e-6, "expression(CO[2]*'-C'~(g~m^{-2}))",
  "FCH4_DRY", "CH4", "expression(bold(CH[4]))",     "expression(CH[4]~(nmol~m^{-2}~s^{-1}))",     86400 * 12.011e-6, "expression(CH[4]*'-C'~(mg~m^{-2}))",
  "FN2O",     "N2O", "expression(bold(N[2]*O))",    "expression(N[2]*O~(nmol~m^{-2}~s^{-1}))",    86400 * 28.013e-6, "expression(N[2]*O*'-N'~(mg~m^{-2}))"
)
ev <- function(x) eval(parse(text = x))
month_axis <- function() scale_x_date(date_breaks = "1 month", date_labels = "%b")
pre_shade <- function() annotate("rect", xmin = -Inf, xmax = APPLICATION_DATE, ymin = -Inf, ymax = Inf, fill = "grey95")

# =============================================================================
# Fig 1: timeline + field conditions
# =============================================================================
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
links <- tibble(from = as.Date(ROUND_DATES), to = incub$start,
                y0 = factor("Soil sampling", levels = rev(rows)), y1 = factor("Lab incubations", levels = rev(rows)))
date_lims <- as.Date(c("2025-05-01", "2025-12-26"))

f1a <- ggplot() +
  geom_vline(xintercept = APPLICATION_DATE, colour = MUTED, linewidth = 0.3, linetype = "22") +
  geom_segment(data = links, aes(x = from, xend = to, y = y0, yend = y1), colour = RULE, linewidth = 0.3) +
  geom_segment(data = incub, aes(x = start, xend = end, y = row, yend = row),
               linewidth = 2.2, colour = "grey35") +
  geom_text(data = incub, aes(x = start + (end - start) / 2, y = row, label = label),
            vjust = -1.1, size = 2.2, colour = INK) +
  geom_point(data = ev_points, aes(x = date, y = row, shape = kind), size = 1.8,
             colour = INK, fill = "white", stroke = 0.45) +
  scale_shape_manual(values = c(event = 18, pre = 21, post = 16),
                     labels = c(pre = "Pre-application flux", post = "Post-application flux"),
                     breaks = c("pre", "post"), name = NULL) +
  scale_y_discrete(limits = rev(rows)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = date_lims, expand = expansion(0)) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "top")

# Field conditions: chamber probe (every campaign) + handheld soil probe (digitized dates)
env_plot <- env %>% group_by(plot, treatment, date) %>%
  summarize(across(c(soil_temp_c, vwc, air_temp_c), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(across(c(soil_temp_c, vwc, air_temp_c), ~ ifelse(is.nan(.x), NA, .x)), treatment = as_trt(treatment))
hand_plot <- hand %>% group_by(plot, treatment, date) %>%
  summarize(ht = mean(soil_temp_c, na.rm = TRUE), hv = mean(mean_vwc, na.rm = TRUE) / 100, .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))
cond_panel <- function(v, hv, ylab, title) {
  s <- trt_summary(env_plot, {{ v }}, date)
  h <- hand_plot %>% group_by(date) %>% summarize(m = mean({{ hv }}, na.rm = TRUE), se = sd({{ hv }}, na.rm = TRUE) / sqrt(n()))
  ggplot() +
    pre_shade() + application_line() +
    geom_point(data = env_plot, aes(date, {{ v }}, colour = treatment), shape = 16, size = 0.6, alpha = 0.35,
               position = position_jitter(width = 1.2, height = 0, seed = 1), show.legend = FALSE) +
    geom_line(data = s, aes(date, mean, colour = treatment), linewidth = 0.4, show.legend = FALSE) +
    geom_point(data = s, aes(date, mean, colour = treatment, fill = treatment, shape = treatment), size = 1.3, stroke = 0.35) +
    geom_linerange(data = h, aes(date, ymin = m - se, ymax = m + se), colour = INK, linewidth = 0.35) +
    geom_point(data = h, aes(date, m), shape = 23, fill = "white", colour = INK, size = 1.6, stroke = 0.45) +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = date_lims, expand = expansion(0)) +
    labs(x = NULL, y = ylab, title = title)
}
f1b <- cond_panel(soil_temp_c, ht, "Temperature (°C)", "Temperature") +
  labs(subtitle = "Coloured: chamber probe (near-surface); ◇ handheld probes, 10 cm")
f1c <- cond_panel(vwc, hv, expression(Volumetric~water~(m^3~m^{-3})), "Soil moisture")
fig1 <- (f1a / f1b / f1c) + plot_layout(heights = c(1, 1, 1), guides = "collect") + tags_pub() &
  theme(legend.position = "bottom")
save_fig(fig1, "fig1_design_conditions", 180, 150)

# =============================================================================
# Fig 2: GHG fluxes (absolute, all plot means shown)
# =============================================================================
flux_plot <- flux_raw %>%
  group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))
post <- flux_plot %>% filter(date > APPLICATION_DATE)
cum_plot <- post %>% arrange(date) %>% group_by(plot, treatment) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), function(v) {
    t <- as.numeric(date - min(date)); ok <- !is.na(v)
    sum(diff(t[ok]) * (head(v[ok], -1) + tail(v[ok], -1)) / 2)
  }), .groups = "drop")

pd <- position_dodge(width = 3)
flux_rows <- lapply(seq_len(nrow(gases)), function(i) {
  g <- gases[i, ]; v <- sym(g$col); first <- i == 1
  ts <- trt_summary(flux_plot, !!v, date)
  cp <- cum_plot %>% mutate(val = !!v * g$cum_factor)
  cs <- trt_summary(cp, val); pv <- anova_p(cp, val)$p
  effects[[paste0("flux_", g$gas)]] <<- diff_vs_control(cp, val) %>%
    mutate(metric = paste0("season_total_", g$gas), group = "29 May-14 Oct", anova_p = pv)
  p_ts <- ggplot() +
    pre_shade() + application_line() +
    { if (g$gas != "CO2") zero_line() } +
    geom_point(data = flux_plot, aes(date, !!v, colour = treatment), shape = 16, size = 0.7, alpha = 0.35,
               position = position_jitterdodge(jitter.width = 1.5, dodge.width = 3, seed = 1), show.legend = FALSE) +
    geom_line(data = ts, aes(date, mean, colour = treatment), position = pd, linewidth = 0.45, show.legend = FALSE) +
    geom_linerange(data = ts, aes(date, ymin = mean - se, ymax = mean + se, colour = treatment),
                   position = pd, linewidth = 0.35, show.legend = FALSE) +
    geom_point(data = ts, aes(date, mean, colour = treatment, fill = treatment, shape = treatment),
               position = pd, size = 1.4, stroke = 0.35) +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() + month_axis() +
    labs(x = NULL, y = ev(g$ylab), title = ev(g$name),
         subtitle = if (first) "Plot means (small) and treatment mean ± SE" else NULL)
  p_c <- ggplot() +
    { if (g$gas != "CO2") zero_line() } +
    dot_ci_layers(cp %>% mutate(x = treatment), cs %>% mutate(x = treatment), x, val, pt_size = 1.3) +
    annotate("text", x = 2, y = Inf, label = sprintf("p = %.2f", pv), vjust = 1.3, size = 2.1, colour = MUTED) +
    trt_axis() +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    guides(colour = "none", fill = "none", shape = "none") +
    labs(x = NULL, y = ev(g$cum_lab), title = if (first) "Season total" else " ",
         subtitle = if (first) sprintf("%s–%s", format(min(post$date), "%d %b"),
                                       format(max(post$date), "%d %b")) else NULL)
  list(p_ts, p_c)
})
fig2 <- wrap_plots(unlist(flux_rows, recursive = FALSE), ncol = 2, widths = c(2.6, 1)) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig2, "fig2_ghg_fluxes", 180, 150)

# =============================================================================
# Fig 3: flux responses to temperature and moisture
# =============================================================================
# Collar-level measurements after application, joined to the chamber probe.
# Models: lmer(flux ~ driver * treatment + (1 | plot/collar)); CO2 vs T on log scale.
# Lines are fixed-effect predictions with 95% CI (fixed-effect uncertainty only).
# The chamber "soil" probe tracks chamber air temperature (within ~1-2 C), so
# temperature here is near-surface, not soil at depth.
resp <- flux_raw %>% filter(date > APPLICATION_DATE) %>%
  inner_join(env %>% select(date, plot, collar, soil_temp_c, vwc), by = c("date", "plot", "collar")) %>%
  mutate(treatment = as_trt(treatment))

resp_stats <- list()
resp_panels <- list()
for (i in seq_len(nrow(gases))) {
  g <- gases[i, ]; y <- g$col
  for (drv in c("soil_temp_c", "vwc")) {
    d <- resp %>% filter(!is.na(.data[[y]]), !is.na(.data[[drv]]))
    log_fit <- g$gas == "CO2" && drv == "soil_temp_c"   # exponential (Q10) model for CO2 vs T
    # Mixed model: repeated measures on collars nested in plots
    form <- as.formula(paste0(if (log_fit) paste0("log(", y, ")") else y,
                              " ~ ", drv, " * treatment + (1 | plot/collar)"))
    m <- suppressMessages(lmerTest::lmer(form, data = d))
    a <- anova(m)
    grid <- d %>% group_by(treatment) %>%
      summarize(xx = list(seq(quantile(.data[[drv]], 0.02), quantile(.data[[drv]], 0.98), length.out = 50)),
                .groups = "drop") %>%
      tidyr::unnest(xx)
    grid[[drv]] <- grid$xx
    X <- model.matrix(as.formula(paste0("~ ", drv, " * treatment")), data = grid)
    b <- lme4::fixef(m); V <- as.matrix(vcov(m))
    grid <- grid %>% mutate(fit = as.vector(X %*% b), se = sqrt(rowSums((X %*% V) * X)),
                            lo = fit - 1.96 * se, hi = fit + 1.96 * se)
    if (log_fit) grid <- grid %>% mutate(across(c(fit, lo, hi), exp))
    slopes <- c(control = unname(b[drv]),
                compost = unname(b[drv] + b[paste0(drv, ":treatmentcompost")]),
                slurry  = unname(b[drv] + b[paste0(drv, ":treatmentslurry")]))
    resp_stats[[paste(g$gas, drv)]] <- tibble(
      gas = g$gas, driver = drv, model = if (log_fit) "log-linear mixed (Q10)" else "linear mixed",
      treatment = TRT_LEVELS, slope = slopes,
      q10 = if (log_fit) exp(10 * slopes) else NA_real_,
      p_driver = a[drv, "Pr(>F)"], p_treatment = a["treatment", "Pr(>F)"],
      p_interaction = a[paste0(drv, ":treatment"), "Pr(>F)"], n = nrow(d))
    ylim <- quantile(d[[y]], c(0.01, 0.99))
    lab <- sprintf("driver p %s; slope × treatment p = %.2f",
                   ifelse(a[drv, "Pr(>F)"] < 0.001, "< 0.001", sprintf("= %.3f", a[drv, "Pr(>F)"])),
                   a[paste0(drv, ":treatment"), "Pr(>F)"])
    if (log_fit) lab <- paste0(lab, "\nQ10: ", paste(sprintf("%s %.1f", TRT_LABELS, exp(10 * slopes)), collapse = ", "))
    resp_panels[[length(resp_panels) + 1]] <- ggplot(d, aes(.data[[drv]], .data[[y]], colour = treatment)) +
      { if (g$gas != "CO2") zero_line() } +
      geom_point(shape = 16, size = 0.6, alpha = 0.3, show.legend = FALSE) +
      geom_ribbon(data = grid, aes(x = .data[[drv]], ymin = lo, ymax = hi, fill = treatment),
                  inherit.aes = FALSE, alpha = 0.15, show.legend = FALSE) +
      geom_line(data = grid, aes(x = .data[[drv]], y = fit, colour = treatment), linewidth = 0.6, show.legend = FALSE) +
      geom_point(data = grid[0, ], aes(x = .data[[drv]], y = fit, fill = treatment, shape = treatment), size = 2) +
      annotate("text", x = -Inf, y = Inf, label = lab, hjust = -0.03, vjust = 1.2, size = 1.9,
               colour = MUTED, lineheight = 0.9) +
      coord_cartesian(ylim = ylim) +
      scale_colour_trt(drop = FALSE) + scale_fill_trt(drop = FALSE) + scale_shape_trt(drop = FALSE) +
      labs(x = if (drv == "soil_temp_c") "Chamber probe temperature (°C)" else expression(Chamber~probe~VWC~(m^3~m^{-3})),
           y = ev(g$ylab), title = if (drv == "soil_temp_c") ev(g$name) else " ")
  }
}
fig3 <- wrap_plots(resp_panels, ncol = 2) + plot_layout(guides = "collect") + tags_pub() &
  theme(legend.position = "bottom")
save_fig(fig3, "fig3_flux_drivers", 180, 165)
resp_tab <- bind_rows(resp_stats) %>% mutate(across(c(slope, q10), ~ signif(.x, 3)),
                                             across(starts_with("p_"), ~ signif(.x, 3)))
write.csv(resp_tab, "output/tables/flux_driver_models.csv", row.names = FALSE)

# =============================================================================
# Fig 4: soil biogeochemistry across rounds
# =============================================================================
lab  <- read.csv("data/processed/lab_assays_summary.csv")
nmin <- read.csv("data/processed/nmin_plot.csv")
soil <- lab %>%
  select(plot, treatment, round = timepoint, sir_ug_co2c_hr_g, cmin_rate_ug_co2c_g_d) %>%
  left_join(nmin %>% select(plot, round, initial_nh4_ug_g, initial_no3_ug_g,
                            net_min_rate_ug_g_d, net_nitr_rate_ug_g_d), by = c("plot", "round")) %>%
  mutate(treatment = as_trt(treatment),
         round_lab = factor(ROUND_LABELS[as.character(round)], levels = ROUND_LABELS))
write.csv(soil, "output/tables/soil_metrics_by_plot.csv", row.names = FALSE)

soil_metrics <- tribble(
  ~col,                    ~title,                     ~ylab,
  "initial_nh4_ug_g",      "Extractable ammonium",     "expression(NH[4]^'+'*'-N'~(mu*g~N~g^{-1}))",
  "initial_no3_ug_g",      "Extractable nitrate",      "expression(NO[3]^'-'*'-N'~(mu*g~N~g^{-1}))",
  "net_min_rate_ug_g_d",   "Net N mineralization",     "expression(mu*g~N~g^{-1}~d^{-1})",
  "net_nitr_rate_ug_g_d",  "Net nitrification",        "expression(mu*g~N~g^{-1}~d^{-1})",
  "sir_ug_co2c_hr_g",      "Substrate-induced resp.",  "expression(mu*g~CO[2]*'-C'~g^{-1}~h^{-1})",
  "cmin_rate_ug_co2c_g_d", "C mineralization (28 d)",  "expression(mu*g~CO[2]*'-C'~g^{-1}~d^{-1})"
)
soil_abs <- lapply(seq_len(nrow(soil_metrics)), function(i) {
  m <- soil_metrics[i, ]; v <- sym(m$col)
  s <- trt_summary(soil, !!v, round_lab); pv <- anova_p(soil, !!v, round_lab)
  effects[[m$col]] <<- diff_vs_control(soil, !!v, round_lab) %>%
    mutate(metric = m$col, group = as.character(round_lab)) %>%
    left_join(pv %>% transmute(group = as.character(round_lab), anova_p = p), by = "group") %>% select(-round_lab)
  ggplot() +
    { if (grepl("net_", m$col)) zero_line() } +
    dot_ci_layers(soil %>% filter(!is.na(!!v)), s, round_lab, !!v) +
    geom_text(data = pv, aes(round_lab, Inf, label = ifelse(p < 0.05, sprintf("p = %.2f", p), "")),
              vjust = 1.2, size = 2.1, colour = INK) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = "Soil sampling", y = ev(m$ylab), title = m$title)
})
fig4 <- wrap_plots(soil_abs, ncol = 3) + plot_layout(guides = "collect") + tags_pub() &
  theme(legend.position = "bottom")
save_fig(fig4, "fig4_soil_biogeochemistry", 180, 120)

# =============================================================================
# Treatment-effects table (plant metrics are appended by 31_si_figures.R)
# =============================================================================
eff <- bind_rows(effects) %>%
  transmute(metric, group, treatment, control_mean = signif(control_mean, 3),
            diff = signif(diff, 3), ci_lo = signif(lo, 3), ci_hi = signif(hi, 3),
            pct_of_control = round(pct, 1), welch_p = round(p, 3), anova_p = round(anova_p, 3))
write.csv(eff, "output/tables/treatment_effects.csv", row.names = FALSE)
cat("  wrote output/tables/treatment_effects.csv, flux_driver_models.csv, soil_metrics_by_plot.csv\n")
