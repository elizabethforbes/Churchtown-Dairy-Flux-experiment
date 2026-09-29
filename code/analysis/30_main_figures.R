# 30_main_figures.R
# Main-text figures (output/figures/main) and summary tables (output/tables).
#   Fig 2  Season GHG fluxes with field conditions; season totals
#   Fig 3  The application pulse: first days after manure application
#   Fig 4  Soil C and N cycling across sampling rounds, grouped by process
#   Fig S2 Temperature and moisture as flux drivers (by treatment; and by the second driver)
# Main Figs 5-6 are written by 32_synthesis_figure.R and 33_ghg_budget.R; the design
# figure (Fig 1: plot map, timeline) and Fig S1 (amendment composition) by 31_si_figures.R.
# Plots are the experimental unit (n = 5). Subsamples (collars, tubes) are
# averaged within plot unless noted. Error bars: 95% CI of the mean unless noted.

source("code/analysis/fig_setup.R")
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
effects <- list()

flux_raw <- read.csv("data/processed/flux_estimates.csv") %>% mutate(date = as.Date(date))
env      <- read.csv("data/processed/chamber_env.csv") %>% mutate(date = as.Date(date))
biomass  <- read.csv("data/processed/biomass.csv") %>% mutate(sampling_date = as.Date(sampling_date))
manure   <- read.csv("data/processed/dairy_one_manure.csv")

gases <- tribble(
  ~col,       ~gas,  ~name,                     ~ylab,                                        ~cum_factor,       ~cum_lab,
  "FCO2_DRY", "CO2", "expression(bold(CO[2]))",  "expression(CO[2]~(mu*mol~m^{-2}~s^{-1}))",  86400 * 12.011e-6, "expression(CO[2]*'-C'~(g~m^{-2}))",
  "FCH4_DRY", "CH4", "expression(bold(CH[4]))",  "expression(CH[4]~(nmol~m^{-2}~s^{-1}))",    86400 * 12.011e-6, "expression(CH[4]*'-C'~(mg~m^{-2}))",
  "FN2O",     "N2O", "expression(bold(N[2]*O))", "expression(N[2]*O~(nmol~m^{-2}~s^{-1}))",   86400 * 28.013e-6, "expression(N[2]*O*'-N'~(mg~m^{-2}))"
)
ev <- function(x) eval(parse(text = x))

pre_shade <- function() annotate("rect", xmin = -Inf, xmax = APPLICATION_DATE, ymin = -Inf, ymax = Inf, fill = "grey95")
season_lims <- as.Date(c("2025-05-01", "2025-10-31"))
season_axis <- function() scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = season_lims, expand = expansion(0))

flux_plot <- flux_raw %>%
  group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))

# =============================================================================
# Application inputs (Table 1; the design figure is Fig 1 in 31_si_figures.R)
# =============================================================================
# Application amounts are the planned rates in the field notes ("Fertilizer
# Fun.docx"): 5 gal slurry and 18 lb compost per 3 x 3 m plot. Slurry density is
# taken as 1 kg/L. Composition is the Dairy One mean of 3 samples per amendment.
PLOT_AREA_M2 <- 9
APPLIED_FRESH_KG <- c(slurry = 5 * 3.785, compost = 18 * 0.4536)
inputs <- manure %>% group_by(amendment_type) %>%
  summarize(ts = mean(total_solids_pct) / 100, tn = mean(total_n_pct) / 100,
            nh4 = mean(ammonium_n_pct) / 100, org = mean(organic_n_pct) / 100, .groups = "drop") %>%
  mutate(fresh_kg_m2 = APPLIED_FRESH_KG[amendment_type] / PLOT_AREA_M2,
         dm_g_m2 = fresh_kg_m2 * ts * 1000,
         n_g_m2 = fresh_kg_m2 * tn * 1000, nh4_g_m2 = fresh_kg_m2 * nh4 * 1000, org_g_m2 = fresh_kg_m2 * org * 1000,
         treatment = as_trt(amendment_type))
write.csv(inputs %>% mutate(across(where(is.numeric), ~ signif(.x, 3)),
                            n_kg_ha = n_g_m2 * 10, dm_Mg_ha = dm_g_m2 / 100),
          "output/tables/application_inputs.csv", row.names = FALSE)


# =============================================================================
# Fig 2: season fluxes with field conditions; season totals
# =============================================================================
hand_env <- read.csv("data/processed/field_metadata.csv") %>% mutate(date = as.Date(date), vwc = mean_vwc / 100)
env_camp <- hand_env %>% mutate(soil_temp_c = soil_temp_filled_c) %>% group_by(date) %>%
  summarize(filled = all(soil_temp_source != "measured", na.rm = TRUE),
            across(c(soil_temp_c, vwc), list(m = ~ mean(.x, na.rm = TRUE), se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
            .groups = "drop") %>% mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))
env_panel <- function(m, se, ylab) ggplot(env_camp, aes(date, .data[[m]])) +
  pre_shade() + application_line() +
  geom_linerange(aes(ymin = .data[[m]] - .data[[se]], ymax = .data[[m]] + .data[[se]]), colour = INK, linewidth = 0.3) +
  geom_line(data = ~ filter(.x, !is.na(.data[[m]])), colour = INK, linewidth = 0.35) +
  geom_point(aes(shape = filled & m == "soil_temp_c_m"), colour = INK, fill = "white", size = 1.2, stroke = 0.4, show.legend = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21)) +
  season_axis() + scale_y_continuous(n.breaks = 3) + labs(x = NULL, y = ylab) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
biomass_date <- unique(biomass$sampling_date)
marks <- tibble(date = c(as.Date(ROUND_DATES), biomass_date), what = c(rep("Soil sampling", 3), "Harvest"))
f2t <- env_panel("soil_temp_c_m", "soil_temp_c_se", "Soil T (°C)") +
  geom_point(data = marks, aes(date, Inf), shape = 25, size = 1.3, colour = INK, fill = INK, inherit.aes = FALSE) +
  geom_text(data = marks, aes(date, Inf, label = c("S1", "S2", "S3", "H")), vjust = -0.9, size = 1.9, colour = INK, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(12, 6, 4, 4))
f2w <- env_panel("vwc_m", "vwc_se", "VWC")

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
  effects[[paste0("season_", g$gas)]] <<- diff_vs_control(cp, val) %>%
    mutate(metric = paste0("season_total_", g$gas), group = "29 May-14 Oct", anova_p = pv)
  p_ts <- ggplot() +
    pre_shade() + application_line() + { if (g$gas != "CO2") zero_line() } +
    geom_point(data = flux_plot, aes(date, !!v, colour = treatment), shape = 16, size = 0.7, alpha = 0.35,
               position = position_jitterdodge(jitter.width = 1.5, dodge.width = 3, seed = 1), show.legend = FALSE) +
    geom_line(data = ts, aes(date, mean, colour = treatment), position = pd, linewidth = 0.45, show.legend = FALSE) +
    geom_linerange(data = ts, aes(date, ymin = mean - se, ymax = mean + se, colour = treatment),
                   position = pd, linewidth = 0.35, show.legend = FALSE) +
    geom_point(data = ts, aes(date, mean, colour = treatment, fill = treatment, shape = treatment),
               position = pd, size = 1.4, stroke = 0.35) +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() + season_axis() +
    labs(x = NULL, y = ev(g$ylab))
  p_c <- ggplot() + { if (g$gas != "CO2") zero_line() } +
    dot_ci_layers(cp %>% mutate(x = treatment), cs %>% mutate(x = treatment), x, val, pt_size = 1.3) +
    trt_axis() + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    guides(colour = "none", fill = "none", shape = "none") +
    labs(x = NULL, y = ev(g$cum_lab), title = if (first) "Season total" else NULL)
  list(p_ts, p_c)
})
fig2 <- (f2t + plot_spacer() + f2w + plot_spacer() + wrap_plots(unlist(flux_rows, recursive = FALSE))) +
  plot_layout(ncol = 2, widths = c(2.6, 1), heights = c(0.45, 0.45, 1, 1, 1), guides = "collect") +
  tags_pub() & theme(legend.position = "bottom")
fig2 <- wrap_plots(c(list(f2t, plot_spacer(), f2w, plot_spacer()), unlist(flux_rows, recursive = FALSE)),
                   ncol = 2, widths = c(2.6, 1), heights = c(0.7, 0.7, 1, 1, 1)) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig2, "fig2_season_fluxes", 180, 175)

# =============================================================================
# Fig 3: the application pulse
# =============================================================================
# Window: last pre-application campaign (27 May, day -1) to 19 Jun (day 22).
# First-week total = trapezoid integral of each plot's flux over days 1-6
# (29 May, 30 May, 3 Jun), compared with control. A baseline-corrected version
# (minus each plot's 27 May flux) was rejected: single pre-application readings
# are noisy (control N2O on 27 May includes -2.7 nmol m-2 s-1) and would dominate.
win_dates <- as.Date(c("2025-05-27", "2025-05-29", "2025-05-30", "2025-06-03", "2025-06-19"))
day_of <- function(d) as.numeric(d - APPLICATION_DATE)
collar <- flux_raw %>% filter(date %in% win_dates) %>% mutate(day = day_of(date), treatment = as_trt(treatment))
wplot <- flux_plot %>% filter(date %in% win_dates) %>% mutate(day = day_of(date))
excess <- flux_plot %>% filter(date %in% win_dates[2:4]) %>% mutate(day = day_of(date)) %>%
  group_by(plot, treatment) %>% arrange(day) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), function(v) sum(diff(day) * (head(v, -1) + tail(v, -1)) / 2)),
            .groups = "drop")
# plot-level totals for the CO2-eq budget (Fig 6): season = 29 May-14 Oct, first week = days 1-6
bind_rows(cum_plot %>% mutate(period = "season"), excess %>% mutate(period = "first_week")) %>%
  transmute(plot, treatment, period, CO2_C_g_m2 = FCO2_DRY * 86400 * 12.011e-6,
            CH4_C_mg_m2 = FCH4_DRY * 86400 * 12.011e-6, N2O_N_mg_m2 = FN2O * 86400 * 28.013e-6) %>%
  write.csv("output/tables/ghg_totals_by_plot.csv", row.names = FALSE)
# CH4 emission events: CH4 is hotspot-driven, so plot means understate a real but
# patchy effect. Count collar measurements that were net CH4 sources, by
# treatment and period, and test slurry's first week against all other
# measurements (Fisher exact test).
ev_tab <- flux_raw %>%
  mutate(period = case_when(date < APPLICATION_DATE ~ "before application",
                            date <= as.Date("2025-06-03") ~ "days 1-6", TRUE ~ "19 Jun-14 Oct")) %>%
  group_by(period, treatment) %>%
  summarize(n = sum(!is.na(FCH4_DRY)), emission_events = sum(FCH4_DRY > 0, na.rm = TRUE),
            pct_events = round(100 * emission_events / n, 1),
            max_flux = round(max(FCH4_DRY, na.rm = TRUE), 2), .groups = "drop")
write.csv(ev_tab, "output/tables/ch4_emission_events.csv", row.names = FALSE)
is_sw1 <- flux_raw$treatment == "slurry" & flux_raw$date > APPLICATION_DATE & flux_raw$date <= as.Date("2025-06-03")
# An emission event = a collar measurement with net CH4 emission (flux > 0).
ev_p <- fisher.test(table(is_sw1, flux_raw$FCH4_DRY > 0))$p.value
ch4_event_lab <- sprintf("Net CH4 emission events: %d of %d slurry collars in days 1\u20136\nvs %d of %d measurements at all other times (Fisher p %s)",
                         sum(is_sw1 & flux_raw$FCH4_DRY > 0, na.rm = TRUE), sum(is_sw1),
                         sum(!is_sw1 & flux_raw$FCH4_DRY > 0, na.rm = TRUE), sum(!is_sw1),
                         ifelse(ev_p < 0.001, "< 0.001", sprintf("= %.3f", ev_p)))
pscale <- list(CO2 = scale_y_log10(), CH4 = scale_y_continuous(trans = pseudo_log_trans(sigma = 0.1), breaks = c(-0.5, 0, 0.5, 2, 5)),
               N2O = scale_y_continuous(trans = pseudo_log_trans(sigma = 0.2), breaks = c(-2, 0, 1, 4)))
pd2 <- position_dodge(width = 0.9)
pulse_rows <- lapply(seq_len(nrow(gases)), function(i) {
  g <- gases[i, ]; v <- sym(g$col); first <- i == 1
  ts <- trt_summary(wplot, !!v, day)
  ex <- excess %>% mutate(val = !!v * g$cum_factor)
  es <- trt_summary(ex, val); dv <- diff_vs_control(ex, val)
  effects[[paste0("pulse_", g$gas)]] <<- dv %>% mutate(metric = paste0("first_week_total_", g$gas), group = "29 May-3 Jun",
                                                       anova_p = anova_p(ex, val)$p)
  lab <- paste(sprintf("%s − control: %s [%s, %s]", TRT_LABELS[as.character(dv$treatment)],
                       signif(dv$diff, 2), signif(dv$lo, 2), signif(dv$hi, 2)), collapse = "\n")
  p_ts <- ggplot() +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = if (g$gas == "CO2") 0 else -Inf, ymax = Inf, fill = "grey95") +
    geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.3, linetype = "22") +
    { if (g$gas != "CO2") zero_line() } +
    geom_point(data = collar, aes(day, !!v, colour = treatment), shape = 1, size = 0.8, stroke = 0.3, alpha = 0.6,
               position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.9, seed = 2), show.legend = FALSE) +
    geom_line(data = ts, aes(day, mean, colour = treatment), position = pd2, linewidth = 0.45, show.legend = FALSE) +
    geom_linerange(data = ts, aes(day, ymin = mean - se, ymax = mean + se, colour = treatment),
                   position = pd2, linewidth = 0.35, show.legend = FALSE) +
    geom_point(data = ts, aes(day, mean, colour = treatment, fill = treatment, shape = treatment),
               position = pd2, size = 1.6, stroke = 0.35) +
    pscale[[g$gas]] +
    scale_x_continuous(breaks = c(-1, 1, 2, 6, 22)) +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
    labs(x = if (i == 3) "Days since application" else NULL, y = ev(g$ylab))
  p_e <- ggplot() + zero_line() +
    dot_ci_layers(ex %>% mutate(x = treatment), es %>% mutate(x = treatment), x, val, pt_size = 1.3) +
    trt_axis() + scale_y_continuous(expand = expansion(mult = c(0.05, 0.35))) +
    guides(colour = "none", fill = "none", shape = "none") +
    labs(x = NULL, y = ev(g$cum_lab), title = if (first) "Days 1–6 total" else NULL)
  list(p_ts, p_e)
})
fig3 <- wrap_plots(unlist(pulse_rows, recursive = FALSE), ncol = 2, widths = c(1.6, 1)) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig3, "fig3_application_pulse", 180, 160)

# =============================================================================
# Fig S2: soil temperature and moisture as flux drivers
# =============================================================================
# Drivers are the handheld probe readings at each collar (soil temperature at
# 10 cm; VWC, probe listed as 10 cm), which are only weakly correlated with each other (r ~ -0.3). The
# chamber's own probe tracks air temperature and was not used (Fig. S10).
# Models (collar-level, after application; random intercepts plot/collar):
#   CO2: Gamma(log) GLMM; CH4 and N2O: Gaussian LMM (ML). Candidate forms
#   null, T, W, T+W, T+W+W^2, TxW compared by AIC (table flux_driver_model_selection.csv).
hand <- read.csv("data/processed/field_metadata.csv") %>% mutate(date = as.Date(date))
resp <- flux_raw %>% filter(date > APPLICATION_DATE) %>%
  inner_join(hand %>% select(date, plot, collar, Ts = soil_temp_filled_c, W = mean_vwc), by = c("date", "plot", "collar")) %>%
  filter(!is.na(Ts), !is.na(W)) %>%
  mutate(W = W / 100, Tc = Ts - 20, Wc = W - 0.15, treatment = as_trt(treatment))
r_TW <- cor(resp$Ts, resp$W)
cands <- c(null = "1", T = "Tc", W = "Wc", `T+W` = "Tc + Wc", `T+W+W2` = "Tc + Wc + I(Wc^2)", `TxW` = "Tc * Wc")
fit_gas <- function(y, rhs) {
  f <- as.formula(paste(y, "~", rhs, "+ (1 | plot/collar)"))
  if (y == "FCO2_DRY") suppressWarnings(lme4::glmer(f, data = resp, family = Gamma(link = "log")))
  else suppressMessages(lmerTest::lmer(f, data = resp, REML = FALSE))
}
models <- lapply(gases$col, function(y) lapply(cands, function(r) fit_gas(y, r))); names(models) <- gases$gas
aic_tab <- bind_rows(lapply(names(models), function(g) tibble(gas = g, model = names(cands), AIC = sapply(models[[g]], AIC)))) %>%
  group_by(gas) %>% mutate(dAIC = round(AIC - min(AIC), 1), AIC = round(AIC, 1)) %>% ungroup() %>%
  mutate(family = if_else(gas == "CO2", "Gamma(log) GLMM", "Gaussian LMM (ML)"), n = nrow(resp))
write.csv(aic_tab, "output/tables/flux_driver_model_selection.csv", row.names = FALSE)
best <- aic_tab %>% group_by(gas) %>% slice_min(AIC, n = 1)
coef_tab <- bind_rows(lapply(names(models), function(g) {
  co <- summary(models[[g]]$`T+W`)$coefficients
  tibble(gas = g, term = rownames(co), estimate = signif(co[, 1], 3), se = signif(co[, 2], 3), p = signif(co[, ncol(co)], 3))
}))
write.csv(coef_tab, "output/tables/flux_driver_coefficients_TplusW.csv", row.names = FALSE)

TEMP_PAL <- colorspace::sequential_hcl(5, "Heat 2", rev = TRUE)
VWC_PAL  <- colorspace::sequential_hcl(5, "Teal", rev = TRUE)
vwc_scale  <- scale_colour_gradientn(colours = VWC_PAL, name = expression(VWC~(m^3~m^{-3})), limits = c(0, 0.4), oob = scales::squish)
temp_scale <- scale_colour_gradientn(colours = TEMP_PAL, name = "Soil T (°C)", limits = c(10, 30), oob = scales::squish)
pred_grid <- function(m, xvar, xseq, zvar, zvals, gamma = FALSE) {
  g <- expand_grid(x = xseq, z = zvals); names(g) <- c(xvar, zvar)
  g <- g %>% mutate(Tc = Ts - 20, Wc = W - 0.15)
  X <- model.matrix(delete.response(terms(lme4::nobars(formula(m)))), g)
  g$fit <- as.vector(X %*% lme4::fixef(m)); if (gamma) g$fit <- exp(g$fit)
  # only draw each curve over the x range actually observed near that z level
  zr <- diff(range(resp[[zvar]])) * 0.12
  rng <- lapply(zvals, function(z) range(resp[[xvar]][abs(resp[[zvar]] - z) <= zr]))
  keep <- mapply(function(x, z) { r <- rng[[match(z, zvals)]]; x >= r[1] & x <= r[2] }, g[[xvar]], g[[zvar]])
  g[keep, ]
}
lab_best <- function(g) { b <- best %>% filter(gas == !!g)
  sprintf("best model: %s (next ΔAIC %.1f)", b$model, sort(aic_tab$dAIC[aic_tab$gas == g])[2]) }

q10 <- exp(10 * lme4::fixef(models$CO2$`T+W`)["Tc"])
g_co2 <- pred_grid(models$CO2$TxW, "Ts", seq(quantile(resp$Ts, .02), quantile(resp$Ts, .98), length.out = 60),
                   "W", c(0.08, 0.15, 0.25), gamma = TRUE)
f4a <- ggplot(resp, aes(Ts, FCO2_DRY, colour = W)) +
  geom_point(shape = 16, size = 0.8, alpha = 0.75) +
  geom_line(data = g_co2, aes(Ts, fit, group = W, colour = W), linewidth = 0.8) +
  vwc_scale + coord_cartesian(ylim = c(0, quantile(resp$FCO2_DRY, 0.99))) +
  labs(x = "Soil temperature (°C)", y = ev(gases$ylab[1]))
g_ch4 <- pred_grid(models$CH4$TxW, "W", seq(quantile(resp$W, .02), quantile(resp$W, .98), length.out = 60),
                   "Ts", c(14, 19, 25))
f4b <- ggplot(resp, aes(W, FCH4_DRY, colour = Ts)) +
  zero_line() +
  geom_point(shape = 16, size = 0.8, alpha = 0.75) +
  geom_line(data = g_ch4, aes(W, fit, group = Ts, colour = Ts), linewidth = 0.8) +
  temp_scale + coord_cartesian(ylim = quantile(resp$FCH4_DRY, c(0.01, 0.99))) +
  labs(x = expression(Soil~VWC~(m^3~m^{-3})), y = ev(gases$ylab[2]))
n2o_fit <- lme4::fixef(models$N2O$W)
f4c <- ggplot(resp, aes(W, FN2O, colour = Ts)) +
  zero_line() +
  geom_point(shape = 16, size = 0.8, alpha = 0.75) +
  geom_abline(intercept = n2o_fit[1] - 0.15 * n2o_fit[2], slope = n2o_fit[2], colour = INK, linewidth = 0.7) +
  temp_scale + coord_cartesian(ylim = quantile(resp$FN2O, c(0.01, 0.99))) +
  labs(x = expression(Soil~VWC~(m^3~m^{-3})), y = ev(gases$ylab[3]))
figS8 <- (f4a | f4b | f4c) + tags_pub() &
  theme(legend.position = "bottom", legend.key.width = unit(16, "pt"), legend.key.height = unit(5, "pt"),
        legend.title = element_text(size = 6.5, vjust = 0.8), legend.text = element_text(size = 6))

# --- Fig S2 top row: one fitted line per treatment ---------------------------------
# CO2: LMM on log(CO2)  CO2 ~ T x treatment + W + T:W        (plotted vs T at median W)
# CH4: LMM              CH4 ~ W x treatment + T + T:W        (plotted vs W at median T)
# N2O: LMM              N2O ~ W x treatment                  (plotted vs W)
# Driver x treatment interaction tested by likelihood ratio against the model
# without it. Lines: fixed effects with 95% CI (fixed-effect uncertainty).
med_T <- median(resp$Ts); med_W <- median(resp$W)
trt_specs <- list(
  list(gas = "CO2", y = "FCO2_DRY", x = "Ts", rhs = "Tc * treatment + Wc + Tc:Wc", rhs0 = "Tc + treatment + Wc + Tc:Wc",
       gamma = TRUE, xlab = "Soil temperature (\u00b0C)", title = "expression(bold(CO[2])~vs~temperature)",
       note = sprintf("moisture held at %.2f", med_W)),
  list(gas = "CH4", y = "FCH4_DRY", x = "W", rhs = "Wc * treatment + Tc + Tc:Wc", rhs0 = "Wc + treatment + Tc + Tc:Wc",
       gamma = FALSE, xlab = "expression(Soil~VWC~(m^3~m^{-3}))", title = "expression(bold(CH[4])~vs~moisture)",
       note = sprintf("soil T held at %.0f \u00b0C", med_T)),
  list(gas = "N2O", y = "FN2O", x = "W", rhs = "Wc * treatment", rhs0 = "Wc + treatment",
       gamma = FALSE, xlab = "expression(Soil~VWC~(m^3~m^{-3}))", title = "expression(bold(N[2]*O)~vs~moisture)",
       note = "")
)
trt_tab <- list()
f4 <- lapply(trt_specs, function(sp) {
  # CO2 is fitted as log(CO2) in a linear mixed model (log-normal); the Gamma GLMM
  # with treatment interactions does not converge. Same log-scale Q10 interpretation.
  yy <- if (sp$gamma) paste0("log(", sp$y, ")") else sp$y
  f1 <- as.formula(paste(yy, "~", sp$rhs, "+ (1 | plot/collar)"))
  f0 <- as.formula(paste(yy, "~", sp$rhs0, "+ (1 | plot/collar)"))
  m1 <- suppressMessages(lme4::lmer(f1, data = resp, REML = FALSE)); m0 <- suppressMessages(lme4::lmer(f0, data = resp, REML = FALSE))
  p_int <- anova(m0, m1)$`Pr(>Chisq)`[2]
  xs <- seq(quantile(resp[[sp$x]], .02), quantile(resp[[sp$x]], .98), length.out = 60)
  grid <- expand_grid(treatment = as_trt(TRT_LEVELS), xx = xs) %>%
    mutate(Ts = if (sp$x == "Ts") xx else med_T, W = if (sp$x == "W") xx else med_W, Tc = Ts - 20, Wc = W - 0.15)
  X <- model.matrix(delete.response(terms(lme4::nobars(formula(m1)))), grid)
  b <- lme4::fixef(m1); V <- as.matrix(vcov(m1))
  grid <- grid %>% mutate(fit = as.vector(X %*% b), se = sqrt(rowSums((X %*% V) * X)), lo = fit - 1.96 * se, hi = fit + 1.96 * se)
  if (sp$gamma) grid <- grid %>% mutate(across(c(fit, lo, hi), exp))
  xc <- if (sp$x == "Ts") "Tc" else "Wc"
  sl <- c(control = unname(b[xc]), compost = unname(b[xc] + b[paste0(xc, ":treatmentcompost")]),
          slurry = unname(b[xc] + b[paste0(xc, ":treatmentslurry")]))
  trt_tab[[sp$gas]] <<- tibble(gas = sp$gas, driver = sp$x, treatment = TRT_LEVELS, slope = signif(sl, 3),
                               q10 = if (sp$gamma) round(exp(10 * sl), 2) else NA, p_driver_x_treatment = signif(p_int, 3), n = nrow(resp))
  lab <- if (sp$gamma) sprintf("Q10: %s\nslope × treatment p = %.2f\n%s", paste(sprintf("%s %.1f", TRT_LABELS, exp(10 * sl)), collapse = ", "), p_int, sp$note)
         else sprintf("slope × treatment p = %.2f%s", p_int, if (nzchar(sp$note)) paste0("\n", sp$note) else "")
  ggplot(resp, aes(.data[[sp$x]], .data[[sp$y]], colour = treatment)) +
    { if (!sp$gamma) zero_line() } +
    geom_point(shape = 16, size = 0.7, alpha = 0.35, show.legend = FALSE) +
    geom_ribbon(data = grid, aes(x = .data[[sp$x]], ymin = lo, ymax = hi, fill = treatment), inherit.aes = FALSE,
                alpha = 0.15, show.legend = FALSE) +
    geom_line(data = grid, aes(.data[[sp$x]], fit, colour = treatment), linewidth = 0.7, show.legend = FALSE) +
    geom_point(data = grid[0, ], aes(.data[[sp$x]], fit, fill = treatment, shape = treatment), size = 2) +
    coord_cartesian(ylim = if (sp$gamma) c(0, quantile(resp[[sp$y]], 0.99)) else quantile(resp[[sp$y]], c(0.01, 0.99))) +
    scale_colour_trt(drop = FALSE) + scale_fill_trt(drop = FALSE) + scale_shape_trt(drop = FALSE) +
    labs(x = if (grepl("expression", sp$xlab)) ev(sp$xlab) else sp$xlab, y = ev(gases$ylab[gases$gas == sp$gas]))
})
# Fig S2: (a-c) one fitted line per treatment; (d-f) the same data coloured by the second driver
s2_top <- wrap_plots(f4, nrow = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
s2_bot <- (f4a | f4b | f4c) &
  theme(legend.position = "bottom", legend.key.width = unit(14, "pt"), legend.key.height = unit(5, "pt"),
        legend.title = element_text(size = 6.5, vjust = 0.8), legend.text = element_text(size = 6))
figS2 <- (s2_top / s2_bot) + tags_pub()
save_fig(figS2, "figS2_flux_drivers", 180, 160, "si")
write.csv(bind_rows(trt_tab), "output/tables/flux_driver_slopes_by_treatment.csv", row.names = FALSE)
write.csv(resp %>% select(date, plot, collar, treatment, Ts, W, FCO2_DRY, FCH4_DRY, FN2O),
          "output/tables/flux_driver_data.csv", row.names = FALSE)

# =============================================================================
# Fig 4: soil C and N cycling across rounds, grouped by process
# =============================================================================
lab  <- read.csv("data/processed/lab_assays_summary.csv")
nmin <- read.csv("data/processed/nmin_plot.csv")
soil <- lab %>%
  select(plot, treatment, round = timepoint, sir_ug_co2c_hr_g, cmin_rate_ug_co2c_g_d) %>%
  left_join(nmin %>% select(plot, round, initial_nh4_ug_g, initial_no3_ug_g,
                            net_min_rate_ug_g_d, net_nitr_rate_ug_g_d), by = c("plot", "round")) %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(ROUND_LABELS[as.character(round)], levels = ROUND_LABELS))
write.csv(soil, "output/tables/soil_metrics_by_plot.csv", row.names = FALSE)
soil_metrics <- tribble(
  ~col,                    ~title,                  ~ylab,
  "sir_ug_co2c_hr_g",      "Active biomass (SIR)",  "expression(mu*g~CO[2]*'-C'~g^{-1}~h^{-1})",
  "cmin_rate_ug_co2c_g_d", "C mineralization",      "expression(mu*g~CO[2]*'-C'~g^{-1}~d^{-1})",
  "net_min_rate_ug_g_d",   "Net N mineralization",  "expression(mu*g~N~g^{-1}~d^{-1})",
  "net_nitr_rate_ug_g_d",  "Net nitrification",     "expression(mu*g~N~g^{-1}~d^{-1})",
  "initial_nh4_ug_g",      "Ammonium",              "expression(NH[4]^'+'*'-N'~(mu*g~g^{-1}))",
  "initial_no3_ug_g",      "Nitrate",               "expression(NO[3]^'-'*'-N'~(mu*g~g^{-1}))"
)
soil_abs <- lapply(seq_len(nrow(soil_metrics)), function(i) {
  m <- soil_metrics[i, ]; v <- sym(m$col)
  s <- trt_summary(soil, !!v, round_lab); pv <- anova_p(soil, !!v, round_lab)
  effects[[m$col]] <<- diff_vs_control(soil, !!v, round_lab) %>%
    mutate(metric = m$col, group = as.character(round_lab)) %>%
    left_join(pv %>% transmute(group = as.character(round_lab), anova_p = p), by = "group") %>% select(-round_lab)
  ggplot() + { if (grepl("net_", m$col)) zero_line() } +
    dot_ci_layers(soil %>% filter(!is.na(!!v)), s, round_lab, !!v) +
    geom_text(data = pv, aes(round_lab, Inf, label = ifelse(p < 0.05, "*", "")),
              vjust = 1.1, size = 3.2, colour = INK) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.14))) +
    labs(x = NULL, y = ev(m$ylab), title = m$title) +
    theme(plot.title = element_text(face = "plain"))
})
names(soil_abs) <- soil_metrics$col
# three labelled columns: microbial biomass and C mineralization | N transformations | extractable N
col_header <- function(txt) wrap_elements(full = grid::grobTree(
  grid::segmentsGrob(x0 = 0.04, x1 = 0.96, y0 = 0.15, y1 = 0.15, gp = grid::gpar(col = INK, lwd = 0.6)),
  grid::textGrob(txt, y = 0.55, gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = INK))), ignore_tag = TRUE)
col_block <- function(h, a, b) wrap_plots(col_header(h), soil_abs[[a]], soil_abs[[b]], ncol = 1, heights = c(0.09, 1, 1))
fig5 <- wrap_plots(col_block("Microbial biomass and C mineralization", "sir_ug_co2c_hr_g", "cmin_rate_ug_co2c_g_d"),
                   col_block("N transformations", "net_min_rate_ug_g_d", "net_nitr_rate_ug_g_d"),
                   col_block("Extractable N", "initial_nh4_ug_g", "initial_no3_ug_g"), nrow = 1) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig5, "fig4_soil_c_n", 180, 125)

# =============================================================================
# Treatment-effects table (plant and soil-test metrics appended by 31_si_figures.R)
# =============================================================================
eff <- bind_rows(effects) %>%
  transmute(metric, group, treatment, control_mean = signif(control_mean, 3),
            diff = signif(diff, 3), ci_lo = signif(lo, 3), ci_hi = signif(hi, 3),
            pct_of_control = round(pct, 1), welch_p = round(p, 3), anova_p = round(anova_p, 3),
            hedges_g = round(hedges_g, 2), g_lo = round(g_lo, 2), g_hi = round(g_hi, 2))
write.csv(eff, "output/tables/treatment_effects.csv", row.names = FALSE)
cat("  wrote treatment_effects.csv, application_inputs.csv, flux_driver_model_selection.csv, soil_metrics_by_plot.csv\n")
