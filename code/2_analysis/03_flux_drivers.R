# 03_flux_drivers.R
# Soil temperature and moisture as drivers of collar fluxes after application.
# Drivers are the handheld readings at each collar (soil temperature at 10 cm, gap-filled
# where missing; VWC at 10 cm).
#   (1) Model selection (collar level, random intercepts plot/collar): CO2 Gamma(log)
#       GLMM; CH4 and N2O Gaussian LMM (ML). Candidates null, T, W, T+W, T+W+W2, TxW
#       compared by AIC.
#   (2) Driver x treatment: CO2 as log(CO2) LMM  ~ T x treatment + W + T:W;
#       CH4 ~ W x treatment + T + T:W; N2O ~ W x treatment. Interaction tested by
#       likelihood ratio; slopes (and Q10 for CO2) per treatment.
# Prediction grids for Fig S1 are written alongside the tables.
# Input:  data/clean/ghg_fluxes.csv, field_probe_readings.csv
# Output: output/tables/flux_driver_model_selection.csv, flux_driver_coefficients_TplusW.csv,
#         flux_driver_slopes_by_treatment.csv, flux_driver_data.csv,
#         flux_driver_pred_bydriver.csv, flux_driver_pred_bytreatment.csv

source("code/lib/setup.R")

flux_raw <- clean_csv("ghg_fluxes.csv") %>% mutate(date = as.Date(date))
hand <- clean_csv("field_probe_readings.csv") %>% mutate(date = as.Date(date))
resp <- flux_raw %>% filter(date > APPLICATION_DATE) %>%
  inner_join(hand %>% select(date, plot, collar, Ts = soil_temp_filled_c, W = mean_vwc), by = c("date", "plot", "collar")) %>%
  filter(!is.na(Ts), !is.na(W)) %>%
  mutate(W = W / 100, Tc = Ts - 20, Wc = W - 0.15, treatment = as_trt(treatment))
write.csv(resp %>% select(date, plot, collar, treatment, Ts, W, FCO2_DRY, FCH4_DRY, FN2O),
          "output/tables/flux_driver_data.csv", row.names = FALSE)
GASES <- c(CO2 = "FCO2_DRY", CH4 = "FCH4_DRY", N2O = "FN2O")

# --- (1) model selection ----------------------------------------------------------
cands <- c(null = "1", T = "Tc", W = "Wc", `T+W` = "Tc + Wc", `T+W+W2` = "Tc + Wc + I(Wc^2)", `TxW` = "Tc * Wc")
fit_gas <- function(y, rhs) {
  f <- as.formula(paste(y, "~", rhs, "+ (1 | plot/collar)"))
  if (y == "FCO2_DRY") suppressWarnings(lme4::glmer(f, data = resp, family = Gamma(link = "log")))
  else suppressMessages(lmerTest::lmer(f, data = resp, REML = FALSE))
}
models <- lapply(GASES, function(y) lapply(cands, function(r) fit_gas(y, r)))
aic_tab <- bind_rows(lapply(names(models), function(g) tibble(gas = g, model = names(cands), AIC = sapply(models[[g]], AIC)))) %>%
  group_by(gas) %>% mutate(dAIC = round(AIC - min(AIC), 1), AIC = round(AIC, 1)) %>% ungroup() %>%
  mutate(family = if_else(gas == "CO2", "Gamma(log) GLMM", "Gaussian LMM (ML)"), n = nrow(resp))
write.csv(aic_tab, "output/tables/flux_driver_model_selection.csv", row.names = FALSE)
coef_tab <- bind_rows(lapply(names(models), function(g) {
  co <- summary(models[[g]]$`T+W`)$coefficients
  tibble(gas = g, term = rownames(co), estimate = signif(co[, 1], 3), se = signif(co[, 2], 3), p = signif(co[, ncol(co)], 3))
}))
write.csv(coef_tab, "output/tables/flux_driver_coefficients_TplusW.csv", row.names = FALSE)

# prediction curves from the TxW models at fixed levels of the second driver, drawn only
# over the range observed near that level (Fig S1 d-e); N2O from the W-only model (Fig S1 f)
pred_grid <- function(m, xvar, xseq, zvar, zvals, gamma = FALSE) {
  g <- expand_grid(x = xseq, z = zvals); names(g) <- c(xvar, zvar)
  g <- g %>% mutate(Tc = Ts - 20, Wc = W - 0.15)
  X <- model.matrix(delete.response(terms(lme4::nobars(formula(m)))), g)
  g$fit <- as.vector(X %*% lme4::fixef(m)); if (gamma) g$fit <- exp(g$fit)
  zr <- diff(range(resp[[zvar]])) * 0.12
  rng <- lapply(zvals, function(z) range(resp[[xvar]][abs(resp[[zvar]] - z) <= zr]))
  keep <- mapply(function(x, z) { r <- rng[[match(z, zvals)]]; x >= r[1] & x <= r[2] }, g[[xvar]], g[[zvar]])
  g[keep, ]
}
n2o_fit <- lme4::fixef(models$N2O$W)
w_seq <- seq(quantile(resp$W, .02), quantile(resp$W, .98), length.out = 60)
bind_rows(
  pred_grid(models$CO2$TxW, "Ts", seq(quantile(resp$Ts, .02), quantile(resp$Ts, .98), length.out = 60),
            "W", c(0.08, 0.15, 0.25), gamma = TRUE) %>% mutate(gas = "CO2"),
  pred_grid(models$CH4$TxW, "W", w_seq, "Ts", c(14, 19, 25)) %>% mutate(gas = "CH4"),
  tibble(W = w_seq, Ts = NA_real_, fit = n2o_fit[1] + (W - 0.15) * n2o_fit[2], gas = "N2O")) %>%
  select(gas, Ts, W, fit) %>%
  write.csv("output/tables/flux_driver_pred_bydriver.csv", row.names = FALSE)

# --- (2) driver x treatment -----------------------------------------------------------
# CO2 is fitted as log(CO2) in a linear mixed model (log-normal); the Gamma GLMM with
# treatment interactions does not converge. Same log-scale Q10 interpretation.
med_T <- median(resp$Ts); med_W <- median(resp$W)
trt_specs <- list(
  list(gas = "CO2", y = "FCO2_DRY", x = "Ts", rhs = "Tc * treatment + Wc + Tc:Wc", rhs0 = "Tc + treatment + Wc + Tc:Wc", gamma = TRUE),
  list(gas = "CH4", y = "FCH4_DRY", x = "W", rhs = "Wc * treatment + Tc + Tc:Wc", rhs0 = "Wc + treatment + Tc + Tc:Wc", gamma = FALSE),
  list(gas = "N2O", y = "FN2O", x = "W", rhs = "Wc * treatment", rhs0 = "Wc + treatment", gamma = FALSE))
trt_tab <- list(); grids <- list()
for (sp in trt_specs) {
  yy <- if (sp$gamma) paste0("log(", sp$y, ")") else sp$y
  m1 <- suppressMessages(lme4::lmer(as.formula(paste(yy, "~", sp$rhs, "+ (1 | plot/collar)")), data = resp, REML = FALSE))
  m0 <- suppressMessages(lme4::lmer(as.formula(paste(yy, "~", sp$rhs0, "+ (1 | plot/collar)")), data = resp, REML = FALSE))
  p_int <- anova(m0, m1)$`Pr(>Chisq)`[2]
  xs <- seq(quantile(resp[[sp$x]], .02), quantile(resp[[sp$x]], .98), length.out = 60)
  grid <- expand_grid(treatment = as_trt(TRT_LEVELS), xx = xs) %>%
    mutate(Ts = if (sp$x == "Ts") xx else med_T, W = if (sp$x == "W") xx else med_W, Tc = Ts - 20, Wc = W - 0.15)
  X <- model.matrix(delete.response(terms(lme4::nobars(formula(m1)))), grid)
  b <- lme4::fixef(m1); V <- as.matrix(vcov(m1))
  grid <- grid %>% mutate(fit = as.vector(X %*% b), se = sqrt(rowSums((X %*% V) * X)), lo = fit - 1.96 * se, hi = fit + 1.96 * se)
  if (sp$gamma) grid <- grid %>% mutate(across(c(fit, lo, hi), exp))
  grids[[sp$gas]] <- grid %>% transmute(gas = sp$gas, driver = sp$x, treatment, Ts, W, fit, lo, hi)
  xc <- if (sp$x == "Ts") "Tc" else "Wc"
  sl <- c(control = unname(b[xc]), compost = unname(b[xc] + b[paste0(xc, ":treatmentcompost")]),
          slurry = unname(b[xc] + b[paste0(xc, ":treatmentslurry")]))
  trt_tab[[sp$gas]] <- tibble(gas = sp$gas, driver = sp$x, treatment = TRT_LEVELS, slope = signif(sl, 3),
                              q10 = if (sp$gamma) round(exp(10 * sl), 2) else NA, p_driver_x_treatment = signif(p_int, 3), n = nrow(resp))
}
write.csv(bind_rows(trt_tab), "output/tables/flux_driver_slopes_by_treatment.csv", row.names = FALSE)
write.csv(bind_rows(grids), "output/tables/flux_driver_pred_bytreatment.csv", row.names = FALSE)
cat(sprintf("  n = %d closures; r(T, VWC) = %.2f; Q10 (T+W model) = %.2f\n", nrow(resp), cor(resp$Ts, resp$W),
            exp(10 * lme4::fixef(models$CO2$`T+W`)["Tc"])))
