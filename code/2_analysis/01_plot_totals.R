# 01_plot_totals.R
# Plot-level quantities that the statistics and figures build on.
#   - Cumulative fluxes per plot: season (29 May-14 Oct) and first week (days 1-6,
#     29 May, 30 May, 3 Jun), trapezoid rule on plot means of the three collars.
#   - CH4 emission events: collar measurements with net CH4 emission (flux > 0), by
#     treatment and period, and Fisher's exact test of slurry days 1-6 against all
#     other measurements.
#   - Soil microbial and N-cycling metrics per plot and sampling.
# Input:  data/clean/ghg_fluxes.csv, soil_by_plot.csv
# Output: output/tables/ghg_totals_by_plot.csv, ch4_emission_events.csv,
#         ch4_event_test.csv, soil_metrics_by_plot.csv

source("code/lib/setup.R")

flux_raw <- clean_csv("ghg_fluxes.csv") %>% mutate(date = as.Date(date))
flux_plot <- flux_raw %>%
  group_by(plot, treatment, date) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment))

# --- cumulative fluxes ------------------------------------------------------------
post <- flux_plot %>% filter(date > APPLICATION_DATE)
cum_plot <- post %>% arrange(date) %>% group_by(plot, treatment) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), function(v) {
    t <- as.numeric(date - min(date)); ok <- !is.na(v)
    sum(diff(t[ok]) * (head(v[ok], -1) + tail(v[ok], -1)) / 2)
  }), .groups = "drop")
first_week <- as.Date(c("2025-05-29", "2025-05-30", "2025-06-03"))
excess <- flux_plot %>% filter(date %in% first_week) %>% mutate(day = as.numeric(date - APPLICATION_DATE)) %>%
  group_by(plot, treatment) %>% arrange(day) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), function(v) sum(diff(day) * (head(v, -1) + tail(v, -1)) / 2)),
            .groups = "drop")
# flux-days (umol or nmol m-2 s-1 x d) to g CO2-C, mg CH4-C and mg N2O-N m-2
bind_rows(cum_plot %>% mutate(period = "season"), excess %>% mutate(period = "first_week")) %>%
  transmute(plot, treatment, period, CO2_C_g_m2 = FCO2_DRY * 86400 * 12.011e-6,
            CH4_C_mg_m2 = FCH4_DRY * 86400 * 12.011e-6, N2O_N_mg_m2 = FN2O * 86400 * 28.013e-6) %>%
  write.csv("output/tables/ghg_totals_by_plot.csv", row.names = FALSE)

# --- CH4 emission events ------------------------------------------------------------
ev_tab <- flux_raw %>%
  mutate(period = case_when(date < APPLICATION_DATE ~ "before application",
                            date <= as.Date("2025-06-03") ~ "days 1-6", TRUE ~ "19 Jun-14 Oct")) %>%
  group_by(period, treatment) %>%
  summarize(n = sum(!is.na(FCH4_DRY)), emission_events = sum(FCH4_DRY > 0, na.rm = TRUE),
            pct_events = round(100 * emission_events / n, 1),
            max_flux = round(max(FCH4_DRY, na.rm = TRUE), 2), .groups = "drop")
write.csv(ev_tab, "output/tables/ch4_emission_events.csv", row.names = FALSE)
is_sw1 <- flux_raw$treatment == "slurry" & flux_raw$date > APPLICATION_DATE & flux_raw$date <= as.Date("2025-06-03")
src <- flux_raw$FCH4_DRY > 0
write.csv(tibble(slurry_days1_6_events = sum(is_sw1 & src, na.rm = TRUE), slurry_days1_6_n = sum(is_sw1),
                 other_events = sum(!is_sw1 & src, na.rm = TRUE), other_n = sum(!is_sw1),
                 fisher_p = signif(fisher.test(table(is_sw1, src))$p.value, 3)),
          "output/tables/ch4_event_test.csv", row.names = FALSE)

# --- soil metrics per plot and sampling -------------------------------------------------
soil <- clean_csv("soil_by_plot.csv") %>%
  select(plot, treatment, round = timepoint, sir_ug_co2c_hr_g, cmin_rate_ug_co2c_g_d,
         initial_nh4_ug_g, initial_no3_ug_g, net_min_rate_ug_g_d, net_nitr_rate_ug_g_d) %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(ROUND_LABELS[as.character(round)], levels = ROUND_LABELS),
         tin_ug_g = initial_nh4_ug_g + initial_no3_ug_g)
write.csv(soil, "output/tables/soil_metrics_by_plot.csv", row.names = FALSE)
cat("  wrote ghg_totals_by_plot.csv, ch4_emission_events.csv, ch4_event_test.csv, soil_metrics_by_plot.csv\n")
