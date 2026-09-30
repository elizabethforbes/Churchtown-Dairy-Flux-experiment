# 05_ghg_budget.R
# Non-CO2 greenhouse-gas budget in CO2-equivalents (GWP100, IPCC AR6: CH4 non-fossil
# 27.0, N2O 273), N2O emission factors, and the season totals used for scale (Fig 6).
#   Budget: CH4 (season), N2O in days 1-6 and over the rest of the season, net.
#   Attributable to the amendment: amendment minus control mean (Welch 95% CI).
#   N2O emission factor: attributable N2O-N as % of applied N (days 1-6; season).
#   Scale terms (g CO2 m-2): soil respiration (clipped collars; season total of daytime
#     closures), aboveground production (Oct harvest; C = 47% of dry mass, IPCC 2006
#     default for herbaceous biomass) and amendment C. Amendment C was not measured: it is
#     taken as 50% of volatile solids, with the VS shares of dry matter used for the storage
#     comparison (slurry 0.80, after ASAE D384.2 VS/TS = 0.85 for lactating dairy manure as
#     excreted; compost 0.55), i.e. 40% (slurry) and 27.5% (compost) of dry matter.
#   Metric sensitivity: GWP20, GTP100 (AR6 WG1 Table 7.15) and GWP* (Smith et al. 2021).
# Soil CO2 is not part of the budget: chamber CO2 is soil respiration, not net exchange.
# Input:  output/tables/ghg_totals_by_plot.csv; data/clean/amendment_application.csv, biomass.csv
# Output: output/tables/ghg_co2eq_by_plot.csv, ghg_attributable.csv, n2o_ef_by_plot.csv, n2o_ef_ci.csv,
#         ghg_context_by_plot.csv, ghg_context_means.csv, ghg_co2eq_budget.csv, ghg_co2eq_metrics.csv

source("code/lib/setup.R")

GWP_CH4 <- 27.0; GWP_N2O <- 273
tot <- read.csv("output/tables/ghg_totals_by_plot.csv")
app <- clean_csv("amendment_application.csv")
n_app <- app %>% transmute(treatment, n_mg_m2 = n_g_m2 * 1000)

ch4_eq <- function(mgC) mgC * 16 / 12 * GWP_CH4 / 1000     # g CO2-eq m-2
n2o_eq <- function(mgN) mgN * 44 / 28 * GWP_N2O / 1000
wide <- tot %>% select(plot, treatment, period, CH4_C_mg_m2, N2O_N_mg_m2) %>%
  pivot_wider(names_from = period, values_from = c(CH4_C_mg_m2, N2O_N_mg_m2)) %>%
  transmute(plot, treatment = as_trt(treatment),
            ch4 = ch4_eq(CH4_C_mg_m2_season),
            n2o_wk = n2o_eq(N2O_N_mg_m2_first_week),
            n2o_rest = n2o_eq(N2O_N_mg_m2_season - N2O_N_mg_m2_first_week),
            n2o = n2o_wk + n2o_rest, net = ch4 + n2o,
            n2o_n_wk = N2O_N_mg_m2_first_week, n2o_n_season = N2O_N_mg_m2_season)
write.csv(wide, "output/tables/ghg_co2eq_by_plot.csv", row.names = FALSE)
p_net <- anova_p(wide, net)$p

# --- attributable to the amendment -------------------------------------------------------
att <- bind_rows(lapply(c("ch4", "n2o_wk", "n2o", "net"), function(v) diff_vs_control(wide, !!sym(v)) %>% mutate(comp = v)))
write.csv(att, "output/tables/ghg_attributable.csv", row.names = FALSE)

# --- N2O emission factor -------------------------------------------------------------------
ctl <- wide %>% filter(treatment == "control") %>% summarize(wk = mean(n2o_n_wk), season = mean(n2o_n_season))
ef_plot <- wide %>% filter(treatment != "control") %>% mutate(treatment = as.character(treatment)) %>%
  left_join(n_app, by = "treatment") %>%
  transmute(plot, treatment, `Days 1–6` = 100 * (n2o_n_wk - ctl$wk) / n_mg_m2,
            Season = 100 * (n2o_n_season - ctl$season) / n_mg_m2) %>%
  pivot_longer(-c(plot, treatment), names_to = "period", values_to = "ef")
write.csv(ef_plot, "output/tables/n2o_ef_by_plot.csv", row.names = FALSE)
ef_ci <- bind_rows(lapply(c("n2o_n_wk", "n2o_n_season"), function(v) diff_vs_control(wide, !!sym(v)) %>%
  mutate(period = ifelse(v == "n2o_n_wk", "Days 1–6", "Season")))) %>%
  mutate(treatment = as.character(treatment)) %>% left_join(n_app, by = "treatment") %>%
  mutate(mean = 100 * diff / n_mg_m2, lo = 100 * lo / n_mg_m2, hi = 100 * hi / n_mg_m2)
write.csv(ef_ci %>% select(treatment, period, mean, lo, hi), "output/tables/n2o_ef_ci.csv", row.names = FALSE)

# --- season totals for scale ---------------------------------------------------------------
VS_FRAC <- c(slurry = 0.80, compost = 0.55)
C_FRAC <- 0.5 * VS_FRAC
co2 <- function(gC) gC * 44 / 12
rs <- tot %>% filter(period == "season") %>% transmute(plot, treatment = as_trt(treatment), val = co2(CO2_C_g_m2))
anpp <- clean_csv("biomass.csv") %>% group_by(plot, treatment) %>%
  summarize(val = co2(mean(dry_matter_g_m2) * 0.47), .groups = "drop") %>% mutate(treatment = as_trt(treatment))
amend_c <- bind_rows(lapply(c("slurry", "compost"), function(tr)
  tibble(treatment = as_trt(tr), val = co2(app$dm_g_m2[app$treatment == tr] * C_FRAC[[tr]]))))
att_net <- att %>% filter(comp == "net") %>% select(treatment, diff)
# signed: + to the atmosphere, - from the atmosphere or into soil
pts <- bind_rows(rs %>% mutate(row = "rs"), anpp %>% mutate(row = "anpp", val = -val),
                 wide %>% transmute(plot, treatment, val = n2o, row = "n2o"),
                 wide %>% transmute(plot, treatment, val = ch4, row = "ch4"))
mns <- pts %>% group_by(row, treatment) %>% summarize(val = mean(val), .groups = "drop") %>%
  bind_rows(att_net %>% transmute(row = "att", treatment, val = diff),
            amend_c %>% mutate(row = "amend", val = -val))
write.csv(pts, "output/tables/ghg_context_by_plot.csv", row.names = FALSE)
write.csv(mns, "output/tables/ghg_context_means.csv", row.names = FALSE)
brk <- att_net %>% left_join(amend_c, by = "treatment") %>% mutate(c_input_over_att = val / diff)
cat("  amendment C input (as CO2) relative to attributable non-CO2:\n")
print(as.data.frame(brk %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))))
cat(sprintf("  non-CO2 net as %% of soil respiration: %s\n",
            paste(sprintf("%s %.2f", levels(wide$treatment),
                          100 * tapply(wide$net, wide$treatment, mean) / tapply(rs$val, rs$treatment, mean)), collapse = ", ")))
cat(sprintf("  non-CO2 net as %% of ANPP C uptake: %s\n",
            paste(sprintf("%s %.1f", levels(wide$treatment),
                          100 * tapply(wide$net, wide$treatment, mean) / tapply(anpp$val, anpp$treatment, mean)), collapse = ", ")))

# --- metric sensitivity ------------------------------------------------------------------
# IPCC AR6 WG1 Table 7.15: CH4 non-fossil GWP20 79.7, GWP100 27.0, GTP100 4.7;
# N2O GWP20 273, GWP100 273, GTP100 233. GWP* (Smith et al. 2021) applies only to
# the CH4 term and depends on how emissions change over time: a new, sustained
# change in CH4 flux (e.g. applying slurry every year) is weighted 4.53 x GWP100
# for its first 20 years; a flux that has been constant for >20 years (the
# background soil CH4 sink) is weighted 0.28 x GWP100. N2O is treated like CO2
# (GWP* = GWP100).
metrics <- tribble(~metric, ~f_ch4_total, ~f_ch4_att, ~f_n2o,
                   "GWP100", 27.0, 27.0, 273,
                   "GWP20", 79.7, 79.7, 273,
                   "GTP100", 4.7, 4.7, 233,
                   "GWP* (sustained practice)", 0.28 * 27.0, 4.53 * 27.0, 273)
mass <- wide %>% transmute(plot, treatment, ch4_kg = ch4 / GWP_CH4, n2o_kg = n2o / GWP_N2O)  # g CH4, g N2O per m2
ctl_mass <- mass %>% filter(treatment == "control") %>% summarize(ch4 = mean(ch4_kg), n2o = mean(n2o_kg))
met_tab <- bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
  m <- metrics[i, ]
  mass %>% group_by(treatment) %>% summarize(ch4 = mean(ch4_kg), n2o = mean(n2o_kg), .groups = "drop") %>%
    transmute(metric = m$metric, treatment,
              ch4_co2eq = ch4 * m$f_ch4_total, n2o_co2eq = n2o * m$f_n2o, net_co2eq = ch4_co2eq + n2o_co2eq,
              attributable_co2eq = ifelse(treatment == "control", NA,
                                          (ch4 - ctl_mass$ch4) * m$f_ch4_att + (n2o - ctl_mass$n2o) * m$f_n2o))
})) %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))
write.csv(met_tab, "output/tables/ghg_co2eq_metrics.csv", row.names = FALSE)

# --- budget table ----------------------------------------------------------------------
bud <- wide %>% group_by(treatment) %>%
  summarize(across(c(ch4, n2o_wk, n2o_rest, n2o, net), list(mean = mean, se = ~ sd(.x) / sqrt(n()))), .groups = "drop") %>%
  mutate(treatment = as.character(treatment)) %>%
  left_join(ef_ci %>% select(treatment, period, mean, lo, hi) %>%
              pivot_wider(names_from = period, values_from = c(mean, lo, hi), names_glue = "EF_{period}_{.value}"),
            by = "treatment") %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)))
names(bud) <- gsub("–", "-", gsub(" ", "_", names(bud)))
write.csv(bud, "output/tables/ghg_co2eq_budget.csv", row.names = FALSE)
cat(sprintf("  wrote ghg_co2eq_budget.csv, ghg_co2eq_metrics.csv; net season CO2-eq ANOVA p = %.2f\n", p_net))
