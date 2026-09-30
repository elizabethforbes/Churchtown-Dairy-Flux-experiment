# fig2_season_fluxes.R
# Fig 2. Season: (a) soil temperature and (b) VWC at 10 cm (campaign mean +/- SE; open
# symbol = gap-filled temperature); (c, e, g) CO2, CH4 and N2O fluxes (plot means and
# treatment means +/- SE); (d, f, h) season totals per plot with treatment mean +/- 95% CI.
# Input: data/clean/ghg_fluxes.csv, field_probe_readings.csv; output/tables/ghg_totals_by_plot.csv

source("code/lib/setup.R")

gases <- tribble(
  ~col,       ~gas,  ~ylab,                                        ~tot,          ~cum_lab,
  "FCO2_DRY", "CO2", "expression(CO[2]~(mu*mol~m^{-2}~s^{-1}))",  "CO2_C_g_m2",  "expression(CO[2]*'-C'~(g~m^{-2}))",
  "FCH4_DRY", "CH4", "expression(CH[4]~(nmol~m^{-2}~s^{-1}))",    "CH4_C_mg_m2", "expression(CH[4]*'-C'~(mg~m^{-2}))",
  "FN2O",     "N2O", "expression(N[2]*O~(nmol~m^{-2}~s^{-1}))",   "N2O_N_mg_m2", "expression(N[2]*O*'-N'~(mg~m^{-2}))"
)
ev <- function(x) eval(parse(text = x))
pre_shade <- function() annotate("rect", xmin = -Inf, xmax = APPLICATION_DATE, ymin = -Inf, ymax = Inf, fill = "grey95")
season_lims <- as.Date(c("2025-05-01", "2025-10-31"))
season_axis <- function() scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = season_lims, expand = expansion(0))

flux_plot <- clean_csv("ghg_fluxes.csv") %>% mutate(date = as.Date(date)) %>%
  group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))
season_tot <- read.csv("output/tables/ghg_totals_by_plot.csv") %>% filter(period == "season") %>%
  mutate(treatment = as_trt(treatment))

# --- (a, b) field conditions ----------------------------------------------------------
hand_env <- clean_csv("field_probe_readings.csv") %>% mutate(date = as.Date(date), vwc = mean_vwc / 100)
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
marks <- tibble(date = as.Date(ROUND_DATES))   # soil samplings
f2t <- env_panel("soil_temp_c_m", "soil_temp_c_se", "Soil T (°C)") +
  geom_point(data = marks, aes(date, Inf), shape = 25, size = 1.3, colour = INK, fill = INK, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(12, 6, 4, 4))
f2w <- env_panel("vwc_m", "vwc_se", "VWC")

# --- (c-h) fluxes and season totals ----------------------------------------------------
pd <- position_dodge(width = 3)
flux_rows <- lapply(seq_len(nrow(gases)), function(i) {
  g <- gases[i, ]; v <- sym(g$col)
  ts <- trt_summary(flux_plot, !!v, date)
  cp <- season_tot %>% mutate(val = .data[[g$tot]])
  cs <- trt_summary(cp, val)
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
    labs(x = NULL, y = ev(g$cum_lab), title = NULL)
  list(p_ts, p_c)
})
fig2 <- wrap_plots(c(list(f2t, plot_spacer(), f2w, plot_spacer()), unlist(flux_rows, recursive = FALSE)),
                   ncol = 2, widths = c(2.6, 1), heights = c(0.7, 0.7, 1, 1, 1)) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig2, "fig2_season_fluxes", 180, 175)
