# fig3_application_pulse.R
# Fig 3. The application pulse: (a, c, e) collar and treatment-mean fluxes from the day
# before application (day -1) to day 22, with the time axis compressed between days 6 and 22;
# (b, d, f) cumulative flux over days 1-6 per plot with treatment mean +/- 95% CI.
# Input: data/clean/ghg_fluxes.csv; output/tables/ghg_totals_by_plot.csv

source("code/lib/setup.R")

gases <- tribble(
  ~col,       ~gas,  ~ylab,                                        ~tot,          ~cum_lab,
  "FCO2_DRY", "CO2", "expression(CO[2]~(mu*mol~m^{-2}~s^{-1}))",  "CO2_C_g_m2",  "expression(CO[2]*'-C'~(g~m^{-2}))",
  "FCH4_DRY", "CH4", "expression(CH[4]~(nmol~m^{-2}~s^{-1}))",    "CH4_C_mg_m2", "expression(CH[4]*'-C'~(mg~m^{-2}))",
  "FN2O",     "N2O", "expression(N[2]*O~(nmol~m^{-2}~s^{-1}))",   "N2O_N_mg_m2", "expression(N[2]*O*'-N'~(mg~m^{-2}))"
)
ev <- function(x) eval(parse(text = x))

flux_raw <- clean_csv("ghg_fluxes.csv") %>% mutate(date = as.Date(date))
flux_plot <- flux_raw %>% group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))
week_tot <- read.csv("output/tables/ghg_totals_by_plot.csv") %>% filter(period == "first_week") %>%
  mutate(treatment = as_trt(treatment))

# window: 27 May (day -1) to 19 Jun (day 22); days 6-22 drawn as a short gap with a break mark
win_dates <- as.Date(c("2025-05-27", "2025-05-29", "2025-05-30", "2025-06-03", "2025-06-19"))
day_of <- function(d) as.numeric(d - APPLICATION_DATE)
X_BREAK <- 7.6; xpos <- function(day) ifelse(day > 6, day - 22 + 9.2, day)
collar <- flux_raw %>% filter(date %in% win_dates) %>% mutate(day = xpos(day_of(date)), treatment = as_trt(treatment))
wplot <- flux_plot %>% filter(date %in% win_dates) %>% mutate(day = xpos(day_of(date)))

# log10 that passes +/-Inf through, so the axis-break mark sits on the axis
log10_inf <- scales::trans_new("log10_inf", function(x) ifelse(is.infinite(x), x, log10(x)), function(x) 10^x,
                               breaks = scales::log_breaks(10), domain = c(1e-100, Inf))
pscale <- list(CO2 = scale_y_continuous(trans = log10_inf, breaks = c(5, 10, 20)),
               CH4 = scale_y_continuous(trans = pseudo_log_trans(sigma = 0.1), breaks = c(-0.5, 0, 0.5, 2, 5)),
               N2O = scale_y_continuous(trans = pseudo_log_trans(sigma = 0.2), breaks = c(-2, 0, 1, 4)))
pd2 <- position_dodge(width = 0.9)
pulse_rows <- lapply(seq_len(nrow(gases)), function(i) {
  g <- gases[i, ]; v <- sym(g$col)
  ts <- trt_summary(wplot, !!v, day)
  ex <- week_tot %>% mutate(val = .data[[g$tot]])
  es <- trt_summary(ex, val)
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
    scale_x_continuous(breaks = xpos(c(-1, 1, 2, 6, 22)), labels = c(-1, 1, 2, 6, 22)) +
    annotation_custom(grid::textGrob("//", y = unit(0, "npc"), vjust = 0.45, gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = INK)),
                      xmin = X_BREAK, xmax = X_BREAK) +
    coord_cartesian(clip = "off") +
    scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
    labs(x = if (i == 3) "Days since application" else NULL, y = ev(g$ylab))
  p_e <- ggplot() + zero_line() +
    dot_ci_layers(ex %>% mutate(x = treatment), es %>% mutate(x = treatment), x, val, pt_size = 1.3) +
    trt_axis() + scale_y_continuous(expand = expansion(mult = c(0.05, 0.35))) +
    guides(colour = "none", fill = "none", shape = "none") +
    labs(x = NULL, y = ev(g$cum_lab), title = NULL)
  list(p_ts, p_e)
})
fig3 <- wrap_plots(unlist(pulse_rows, recursive = FALSE), ncol = 2, widths = c(1.6, 1)) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig3, "fig3_application_pulse", 180, 160)
