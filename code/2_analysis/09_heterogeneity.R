# 09_heterogeneity.R
# Spatial heterogeneity of the amendment responses: hot spots vs uneven application.
#   (1) Plot scale: are amended plots more variable than controls (Fligner-Killeen test
#       on plot totals), and what share of each amendment's attributable emission comes
#       from its single highest plot?
#   (2) Collar scale: within-plot spread among the three collars on each campaign
#       (SD of collar fluxes within plot). Uneven spreading of slurry or compost within a
#       plot would raise within-plot spread right after application; pre-existing
#       hot spots would show before application too.
# Input:  output/tables/ghg_totals_by_plot.csv; data/clean/ghg_fluxes.csv
# Output: output/tables/heterogeneity_plot_totals.csv, heterogeneity_within_plot.csv

source("code/lib/setup.R")

# --- (1) plot totals ----------------------------------------------------------------
tot <- read.csv("output/tables/ghg_totals_by_plot.csv")
cols <- c(CO2 = "CO2_C_g_m2", CH4 = "CH4_C_mg_m2", N2O = "N2O_N_mg_m2")
plot_het <- bind_rows(lapply(c("first_week", "season"), function(per) bind_rows(lapply(names(cols), function(g) {
  d <- tot %>% filter(period == per) %>% transmute(plot, treatment, v = .data[[cols[g]]])
  ctl <- d$v[d$treatment == "control"]
  bind_rows(lapply(c("compost", "slurry"), function(tr) {
    x <- d$v[d$treatment == tr]; dd <- d %>% filter(treatment %in% c("control", tr))
    excess <- x - mean(ctl)
    tibble(period = per, gas = g, treatment = tr,
           sd_control = sd(ctl), sd_amended = sd(x), sd_ratio = sd(x) / sd(ctl),
           fligner_p = fligner.test(v ~ treatment, data = dd)$p.value,
           n_above_control_max = sum(x > max(ctl)), n_below_control_min = sum(x < min(ctl)),
           top_plot = d$plot[d$treatment == tr][which.max(x)],
           top_plot_share_of_attributable = if (sum(excess) > 0) max(excess) / sum(excess) else NA_real_)
  }))
})))) %>% mutate(across(where(is.numeric) & !c(n_above_control_max, n_below_control_min, top_plot), ~ signif(.x, 3)))
write.csv(plot_het, "output/tables/heterogeneity_plot_totals.csv", row.names = FALSE)

# --- (2) within-plot spread among collars ------------------------------------------------
fx <- clean_csv("ghg_fluxes.csv") %>% mutate(date = as.Date(date))
wp <- fx %>% group_by(date, plot, treatment) %>%
  summarize(across(c(FCO2_DRY, FCH4_DRY, FN2O), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"), n = n(), .groups = "drop") %>%
  mutate(period = case_when(date < APPLICATION_DATE ~ "before application",
                            date <= as.Date("2025-05-30") ~ "days 1-2",
                            date <= as.Date("2025-06-03") ~ "day 6",
                            TRUE ~ "19 Jun-14 Oct"))
within_tab <- wp %>% pivot_longer(starts_with("sd_"), names_to = "gas", values_to = "sd") %>%
  mutate(gas = c(sd_FCO2_DRY = "CO2", sd_FCH4_DRY = "CH4", sd_FN2O = "N2O")[gas]) %>%
  group_by(period, gas, treatment) %>%
  summarize(median_within_plot_sd = median(sd, na.rm = TRUE), max_within_plot_sd = max(sd, na.rm = TRUE),
            n_plot_dates = sum(!is.na(sd)), .groups = "drop")
# test: within-plot SD in days 1-2, slurry vs control (Wilcoxon on plot x date SDs)
test_rows <- bind_rows(lapply(c("CO2", "CH4", "N2O"), function(g) {
  col <- paste0("sd_", c(CO2 = "FCO2_DRY", CH4 = "FCH4_DRY", N2O = "FN2O")[g])
  bind_rows(lapply(c("before application", "days 1-2"), function(per) {
    d <- wp %>% filter(period == per)
    bind_rows(lapply(c("compost", "slurry"), function(tr) {
      a <- d[[col]][d$treatment == tr]; b <- d[[col]][d$treatment == "control"]
      tibble(period = per, gas = g, treatment = paste(tr, "vs control"),
             median_ratio = median(a, na.rm = TRUE) / median(b, na.rm = TRUE),
             wilcox_p = suppressWarnings(wilcox.test(a, b)$p.value))
    }))
  }))
}))
write.csv(bind_rows(within_tab %>% mutate(across(where(is.numeric), ~ signif(.x, 3))),
                    test_rows %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))),
          "output/tables/heterogeneity_within_plot.csv", row.names = FALSE)
cat("  wrote heterogeneity_plot_totals.csv, heterogeneity_within_plot.csv\n")
print(as.data.frame(plot_het)); print(as.data.frame(test_rows))
