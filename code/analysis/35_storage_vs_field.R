# 35_storage_vs_field.R
# Storage-phase vs field-phase non-CO2 emissions per kg of manure N, for the
# storage -> field framing of the manure chain.
#   Storage (IPCC 2019 Refinement, Vol. 4 Ch. 10, Tier 1, cool temperate moist zone):
#     CH4 = VS x B0 x 0.67 x MCF;  B0 = 0.24 m3 CH4 kg-1 VS (dairy, Table 10.16A)
#     MCF (Table 10.17): liquid/slurry 1-month storage 6%, 6-month 21%, 12-month 31%,
#       uncovered anaerobic lagoon 60%; composting intensive windrow 0.5%,
#       passive windrow 1%; solid storage 2%
#     Direct N2O EF3 (Table 10.21): liquid/slurry without crust 0, with crust 0.005;
#       composting windrows 0.005; solid storage 0.010 kg N2O-N kg-1 N
#   VS per kg N is taken from the applied materials (Dairy One total solids and N),
#   with an assumed VS share of dry matter (slurry 0.80, compost 0.55; not measured),
#   so storage values are per kg of N as applied. Indirect N2O (NH3 volatilization,
#   leaching) is excluded from both phases.
#   Field phase (this study): amendment-attributable CH4 + N2O (amendment minus
#   control mean), for days 1-6 and the season, per kg N applied, GWP100 (AR6).
# Output: output/tables/storage_vs_field_co2eq.csv

suppressPackageStartupMessages({library(dplyr); library(tidyr)})
GWP_CH4 <- 27.0; GWP_N2O <- 273
n2o_n_to_co2eq <- 44 / 28 * GWP_N2O          # kg CO2-eq per kg N2O-N
B0 <- 0.24; RHO_CH4 <- 0.67                   # m3 CH4 kg-1 VS; kg CH4 m-3
VS_FRAC <- c(slurry = 0.80, compost = 0.55)

inp <- read.csv("output/tables/application_inputs.csv")
vs_per_n <- setNames(inp$ts * VS_FRAC[inp$treatment] / inp$tn, inp$treatment)   # kg VS per kg N

storage <- tribble(
  ~material, ~system,                               ~MCF,  ~EF3,
  "slurry",  "Liquid/slurry, 1-month storage",      0.06,  0,
  "slurry",  "Liquid/slurry, 6-month storage",      0.21,  0,
  "slurry",  "Liquid/slurry, 6-month, natural crust", 0.21 * 0.6, 0.005,
  "slurry",  "Liquid/slurry, 12-month storage",     0.31,  0,
  "slurry",  "Uncovered anaerobic lagoon",          0.60,  0,
  "compost", "Composting, intensive windrow",       0.005, 0.005,
  "compost", "Composting, passive windrow",         0.010, 0.005,
  "compost", "Solid storage (stockpile)",           0.020, 0.010
) %>% mutate(phase = "Storage (IPCC Tier 1)",
             ch4_kg_per_kgN = vs_per_n[material] * B0 * RHO_CH4 * MCF,
             co2eq_ch4 = ch4_kg_per_kgN * GWP_CH4,
             co2eq_n2o = EF3 * n2o_n_to_co2eq,
             co2eq_total = co2eq_ch4 + co2eq_n2o)

# field phase from plot totals
tot <- read.csv("output/tables/ghg_totals_by_plot.csv")
n_app <- setNames(inp$n_g_m2, inp$treatment)            # g N m-2
ctl <- tot %>% filter(treatment == "control") %>% group_by(period) %>%
  summarize(ch4 = mean(CH4_C_mg_m2), n2o = mean(N2O_N_mg_m2), .groups = "drop")
field <- tot %>% filter(treatment != "control") %>% left_join(ctl, by = "period", suffix = c("", "_ctl")) %>%
  mutate(ch4_att_g = (CH4_C_mg_m2 - ch4) * 16 / 12 / 1000,             # g CH4 m-2
         n2o_att_gN = (N2O_N_mg_m2 - n2o) / 1000,                      # g N2O-N m-2
         per_kgN = 1 / n_app[treatment],                               # g -> per g N applied = kg per kg N
         ch4_kg_per_kgN = ch4_att_g * per_kgN,
         co2eq_ch4 = ch4_kg_per_kgN * GWP_CH4,
         co2eq_n2o = n2o_att_gN * per_kgN * n2o_n_to_co2eq,
         co2eq_total = co2eq_ch4 + co2eq_n2o) %>%
  group_by(material = treatment, period) %>%
  summarize(across(c(ch4_kg_per_kgN, co2eq_ch4, co2eq_n2o), mean),
            total_mean = mean(co2eq_total), total_lo = mean(co2eq_total) - qt(0.975, n() - 1) * sd(co2eq_total) / sqrt(n()),
            total_hi = mean(co2eq_total) + qt(0.975, n() - 1) * sd(co2eq_total) / sqrt(n()), .groups = "drop") %>%
  mutate(phase = "Field application (this study)",
         system = ifelse(period == "first_week", "Days 1-6 after application", "Season (29 May-14 Oct)"))

out <- bind_rows(
  storage %>% transmute(phase, material, system, ch4_kg_per_kgN, co2eq_ch4, co2eq_n2o, co2eq_total, total_lo = NA, total_hi = NA),
  field %>% transmute(phase, material, system, ch4_kg_per_kgN, co2eq_ch4, co2eq_n2o, co2eq_total = total_mean, total_lo, total_hi)
) %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))
write.csv(out, "output/tables/storage_vs_field_co2eq.csv", row.names = FALSE)
cat(sprintf("VS per kg N (assumed VS share): slurry %.1f, compost %.1f kg\n", vs_per_n["slurry"], vs_per_n["compost"]))
cat("Storage vs field, kg CO2-eq per kg manure N (GWP100):\n"); print(as.data.frame(out), row.names = FALSE)
