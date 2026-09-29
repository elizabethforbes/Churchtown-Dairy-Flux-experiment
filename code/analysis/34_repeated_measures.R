# 34_repeated_measures.R
# Repeated-measures (linear mixed-effects) tests of treatment effects over time,
# plus two supporting checks for the framing of null results.
#   (A) Collar fluxes: gas ~ treatment * date + (1 | plot/collar), all campaigns
#       (pre-application campaigns included as baseline). CO2 log-transformed.
#       Sensitivity: rank-transformed response (robust to the heavy CH4/N2O tails)
#       and plot-mean models (plot as the unit). Per-date amendment - control
#       contrasts from emmeans (Dunnett-adjusted within date).
#   (B) Soil variables: y ~ treatment * sampling + (1 | plot).
#   (C) Microbial metabolic quotient: C-mineralization rate per unit SIR biomass,
#       to test whether an unchanged flux could hide offsetting changes in
#       biomass and activity.
#   (D) N supply context: season N supply from net N mineralization (lab
#       potential), applied N and plant N uptake.
# Output: output/tables/rm_flux_anova.csv, rm_flux_contrasts.csv, rm_soil_anova.csv,
#         metabolic_quotient.csv, n_supply_context.csv

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(lmerTest); library(emmeans)})
emm_options(lmer.df = "satterthwaite", pbkrtest.limit = 0, lmerTest.limit = 1e5)
trt_lv <- c("control", "compost", "slurry")

fx <- read.csv("data/processed/flux_estimates.csv") %>%
  mutate(date = as.Date(date), treatment = factor(treatment, trt_lv),
         date_f = factor(format(date, "%d %b"), levels = unique(format(sort(unique(date)), "%d %b"))),
         plot = factor(plot), collar_id = interaction(plot, collar))
gases <- list(CO2 = quote(log(FCO2_DRY)), CH4 = quote(FCH4_DRY), N2O = quote(FN2O))

fit_one <- function(d, resp, form_re = "(1 | plot/collar)") {
  d$y <- eval(resp, d)
  lmer(as.formula(paste("y ~ treatment * date_f +", form_re)), data = d, REML = TRUE)
}
anova_rows <- list(); contr_rows <- list()
for (g in names(gases)) {
  d <- fx %>% filter(!is.na(eval(gases[[g]], fx)))
  variants <- list(
    collar = list(data = d, resp = gases[[g]], re = "(1 | plot/collar)"),
    collar_rank = list(data = d %>% mutate(r = rank(eval(gases[[g]], d))), resp = quote(r), re = "(1 | plot/collar)"),
    plot_mean = list(data = d %>% mutate(v = eval(gases[[g]], d)) %>% group_by(plot, treatment, date_f) %>%
                       summarize(v = mean(v), .groups = "drop"), resp = quote(v), re = "(1 | plot)"))
  for (vn in names(variants)) {
    v <- variants[[vn]]
    m <- fit_one(v$data, v$resp, v$re)
    a <- as.data.frame(anova(m, type = 3))
    anova_rows[[paste(g, vn)]] <- tibble(gas = g, model = vn, term = rownames(a),
                                         F = round(a$`F value`, 2), df_num = a$NumDF,
                                         df_den = round(a$DenDF, 1), p = signif(a$`Pr(>F)`, 3))
    if (vn == "collar") {
      em <- emmeans(m, ~ treatment | date_f)
      ct <- as.data.frame(contrast(em, "trt.vs.ctrl", ref = "control", adjust = "dunnett"))
      contr_rows[[g]] <- ct %>% transmute(gas = g, date = date_f, contrast, estimate = signif(estimate, 3),
                                          SE = signif(SE, 3), p_dunnett = signif(p.value, 3))
    }
  }
}
rm_anova <- bind_rows(anova_rows)
rm_contr <- bind_rows(contr_rows)
write.csv(rm_anova, "output/tables/rm_flux_anova.csv", row.names = FALSE)
write.csv(rm_contr, "output/tables/rm_flux_contrasts.csv", row.names = FALSE)
cat("\n(A) Flux repeated-measures LMM, type III (Satterthwaite):\n")
print(as.data.frame(rm_anova %>% filter(term != "(Intercept)")), row.names = FALSE)
cat("\nPer-date contrasts with p < 0.10 (collar model, CO2 on log scale):\n")
print(as.data.frame(rm_contr %>% filter(p_dunnett < 0.10)), row.names = FALSE)

# --- (B) soil variables -------------------------------------------------------------
soil <- read.csv("output/tables/soil_metrics_by_plot.csv") %>%
  mutate(treatment = factor(treatment, trt_lv), plot = factor(plot),
         round_lab = factor(round_lab, c("29 May", "21 Jul", "14 Oct")))
d1 <- read.csv("data/processed/dairy_one_clean.csv") %>%
  transmute(plot = factor(plot), round = timepoint, d1_ph = ph, d1_om_pct = om_pct, d1_p_ppm = p_ppm, d1_k_ppm = k_ppm)
soil <- soil %>% left_join(d1, by = c("plot", "round")) %>%
  mutate(mq = cmin_rate_ug_co2c_g_d / (sir_ug_co2c_hr_g * 24))          # (C) metabolic quotient (d-1 per d-1 of SIR)
soil_vars <- c("initial_nh4_ug_g", "initial_no3_ug_g", "net_min_rate_ug_g_d", "net_nitr_rate_ug_g_d",
               "sir_ug_co2c_hr_g", "cmin_rate_ug_co2c_g_d", "mq", "d1_ph", "d1_om_pct", "d1_p_ppm", "d1_k_ppm")
rm_soil <- bind_rows(lapply(soil_vars, function(v) {
  d <- soil %>% filter(!is.na(.data[[v]]))
  m <- lmer(as.formula(paste(v, "~ treatment * round_lab + (1 | plot)")), data = d)
  a <- as.data.frame(anova(m, type = 3))
  tibble(variable = v, term = rownames(a), F = round(a$`F value`, 2), p = signif(a$`Pr(>F)`, 3))
}))
write.csv(rm_soil, "output/tables/rm_soil_anova.csv", row.names = FALSE)
cat("\n(B) Soil repeated-measures LMM (treatment effects):\n")
print(as.data.frame(rm_soil %>% select(variable, term, p) %>% pivot_wider(names_from = term, values_from = p)), row.names = FALSE)

mq <- soil %>% group_by(round_lab, treatment) %>%
  summarize(sir = mean(sir_ug_co2c_hr_g, na.rm = TRUE), cmin = mean(cmin_rate_ug_co2c_g_d, na.rm = TRUE),
            mq = mean(mq, na.rm = TRUE), .groups = "drop") %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))
write.csv(mq, "output/tables/metabolic_quotient.csv", row.names = FALSE)
cat("\n(C) Metabolic quotient (C-min rate / SIR-derived daily rate), treatment means:\n"); print(as.data.frame(mq))

# --- (D) N supply context ------------------------------------------------------------
# Lab net mineralization (20 C, 65% WHC) is a potential rate; field rates are lower.
# Bulk density was not measured. Fine-earth mass for 0-15 cm uses SSURGO values for the plots' map unit
# (BlA, Blasdell channery loam, 0-25 cm horizon): fine-earth bulk density 1.25 g cm-3 and 32% coarse
# fragments by volume, giving 1.25e3 * (1 - 0.32) * 0.15 * 1e4 = 1.28e6 kg ha-1. Lab rates are per g of
# sieved (<2 mm) soil, so the fragment correction applies. The upper bound omits it (1.9e6 kg ha-1).
BD_FINE <- 1.25; FRAG_VOL <- 0.32
SOIL_KG_HA <- BD_FINE * 1e3 * (1 - FRAG_VOL) * 0.15 * 1e4
SOIL_KG_HA_NOFRAG <- BD_FINE * 1e3 * 0.15 * 1e4
season_d <- as.numeric(as.Date("2025-10-14") - as.Date("2025-05-29"))
nmin_mean <- mean(soil$net_min_rate_ug_g_d, na.rm = TRUE)
forage <- read.csv("data/processed/dairy_one_forage.csv")
bio <- read.csv("data/processed/biomass.csv") %>% group_by(plot) %>% summarize(dm = mean(dry_matter_g_m2))
upt <- forage %>% left_join(bio, by = "plot") %>%
  mutate(n_pct = crude_protein_pct / 6.25, n_uptake_kg_ha = dm * 10 * n_pct / 100) %>%
  group_by(treatment) %>% summarize(n_pct = mean(n_pct), n_uptake_kg_ha = mean(n_uptake_kg_ha), .groups = "drop")
nsup <- tibble(item = c("Net N mineralization, lab potential (mg N kg-1 d-1, mean)",
                        "Season N supply from mineralization at lab potential (kg N ha-1, 0-15 cm)",
                        "Applied N, slurry / compost (kg N ha-1)",
                        "Applied NH4-N, slurry / compost (kg N ha-1)",
                        "Initial mineral N, 29 May (kg N ha-1, 0-15 cm)",
                        "Season N supply, no coarse-fragment correction (kg N ha-1, 0-15 cm; upper bound)"),
               value = c(signif(nmin_mean, 3), round(nmin_mean * season_d * SOIL_KG_HA / 1e6),
                         "27.5 / 38.7", "2.8 / 0.5",
                         round(mean(with(soil[soil$round_lab == "29 May", ], initial_nh4_ug_g + initial_no3_ug_g), na.rm = TRUE) * SOIL_KG_HA / 1e6),
                         round(nmin_mean * season_d * SOIL_KG_HA_NOFRAG / 1e6)))
# Soil organic C stock (0-15 cm) for scale against the amendment C inputs: Dairy One LOI organic matter
# at the first sampling, OM assumed 50% C (Pribyl 2010), same fine-earth mass as above. Amendment C uses
# the dry matter applied and the assumed C fractions of 33_ghg_budget.R (slurry 25-40%, compost 15-30%).
OM_C <- 0.5
om1 <- mean(read.csv("data/processed/dairy_one_clean.csv") %>% filter(timepoint == 1) %>% pull(om_pct), na.rm = TRUE)
soc_g_m2 <- om1 / 100 * OM_C * SOIL_KG_HA * 1e3 / 1e4
app <- read.csv("output/tables/application_inputs.csv")
C_FRAC <- list(slurry = c(0.25, 0.40), compost = c(0.15, 0.30))
amend_c <- sapply(c("slurry", "compost"), function(tr) app$dm_g_m2[app$treatment == tr] * C_FRAC[[tr]])
nsup <- bind_rows(nsup, tibble(
  item = c("Soil organic C stock, 0-15 cm (g C m-2; LOI OM x 0.5, first sampling)",
           "Amendment C input, slurry / compost (g C m-2; assumed C fractions)",
           "Amendment C as % of 0-15 cm soil C stock, slurry / compost"),
  value = c(as.character(signif(soc_g_m2, 2)),
            sprintf("%.0f-%.0f / %.0f-%.0f", amend_c[1, "slurry"], amend_c[2, "slurry"], amend_c[1, "compost"], amend_c[2, "compost"]),
            sprintf("%.1f-%.1f / %.1f-%.1f", 100 * amend_c[1, "slurry"] / soc_g_m2, 100 * amend_c[2, "slurry"] / soc_g_m2,
                    100 * amend_c[1, "compost"] / soc_g_m2, 100 * amend_c[2, "compost"] / soc_g_m2))))
write.csv(bind_rows(nsup, upt %>% transmute(item = paste("Plant N uptake (Oct harvest), ", treatment, " (kg N ha-1; N %)"),
                                            value = sprintf("%.0f; %.2f%%", n_uptake_kg_ha, n_pct))),
          "output/tables/n_supply_context.csv", row.names = FALSE)
cat("\n(D) N supply context:\n"); print(as.data.frame(nsup)); print(as.data.frame(upt %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))))
cat(sprintf("Mehlich-3 P and K (29 May mean): P %.0f, K %.0f mg kg-1; Morgan P %.1f lb/A, Morgan K %.0f lb/A\n",
            mean(read.csv("data/processed/dairy_one_clean.csv")$p_ppm[read.csv("data/processed/dairy_one_clean.csv")$timepoint == 1]),
            mean(read.csv("data/processed/dairy_one_clean.csv")$k_ppm[read.csv("data/processed/dairy_one_clean.csv")$timepoint == 1]),
            mean(read.csv("data/processed/dairy_one_clean.csv")$morgan_p_lb_ac[read.csv("data/processed/dairy_one_clean.csv")$timepoint == 1]),
            mean(read.csv("data/processed/dairy_one_clean.csv")$morgan_k_lb_ac[read.csv("data/processed/dairy_one_clean.csv")$timepoint == 1])))

# --- (E) minimum detectable effects (plot totals; two-sample, alpha 0.05, power 0.8) ---
mde <- function(x_ctl, n = 5) {
  s <- sd(x_ctl); d <- power.t.test(n = n, sd = s, sig.level = 0.05, power = 0.8)$delta
  c(control_mean = mean(x_ctl), sd = s, mde = d, mde_pct = 100 * d / abs(mean(x_ctl)))
}
tot <- read.csv("output/tables/ghg_totals_by_plot.csv")
bio <- read.csv("data/processed/biomass.csv") %>% group_by(plot, treatment) %>% summarize(dm = mean(dry_matter_g_m2), .groups = "drop")
pooled_sd <- function(df, v) sqrt(mean(tapply(df[[v]], df$treatment, var)))   # pooled within-treatment SD
mde_tab <- bind_rows(lapply(list(
  c("season", "CO2_C_g_m2", "Season CO2 (g C m-2)"), c("season", "CH4_C_mg_m2", "Season CH4 (mg C m-2)"),
  c("season", "N2O_N_mg_m2", "Season N2O (mg N m-2)"), c("first_week", "N2O_N_mg_m2", "Days 1-6 N2O (mg N m-2)")),
  function(r) { d <- tot[tot$period == r[1], ]; s <- pooled_sd(d, r[2]); m <- mean(d[[r[2]]][d$treatment == "control"])
    dd <- power.t.test(n = 5, sd = s, sig.level = 0.05, power = 0.8)$delta
    tibble(response = r[3], control_mean = m, pooled_sd = s, mde = dd, mde_pct_of_control = 100 * dd / abs(m)) }))
s_bio <- pooled_sd(bio, "dm"); d_bio <- power.t.test(n = 5, sd = s_bio, sig.level = 0.05, power = 0.8)$delta
mde_tab <- bind_rows(mde_tab, tibble(response = "Biomass (g DM m-2)", control_mean = mean(bio$dm[bio$treatment == "control"]),
                                     pooled_sd = s_bio, mde = d_bio, mde_pct_of_control = 100 * d_bio / mean(bio$dm[bio$treatment == "control"]))) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)))
write.csv(mde_tab, "output/tables/minimum_detectable_effects.csv", row.names = FALSE)
cat("\n(E) Minimum detectable differences (n = 5 per treatment, alpha 0.05, power 0.8):\n"); print(as.data.frame(mde_tab))
