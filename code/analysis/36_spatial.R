# 36_spatial.R
# Spatial structure in plot-level responses: do position, elevation, edge location,
# neighbouring treatments or background (pre-application) conditions explain the
# plot fluxes, and are residuals spatially autocorrelated?
# Plot geometry: RTK-GPS corners (KT-CTD-Plots.csv, UTM 18N). n = 15 plots, so these
# are screening tests with low power, reported for transparency.
# Output: output/tables/spatial_covariates_by_plot.csv, spatial_tests.csv

source("code/analysis/fig_setup.R")
suppressPackageStartupMessages(library(spdep))

key <- read.csv("data/processed/treatment_key.csv")
corners <- read.csv("data/raw/field_metadata/KT-CTD-Plots.csv") %>%
  transmute(plot = as.integer(Name), E = Easting, N = Northing, z = Elevation)
geo <- corners %>% group_by(plot) %>% summarize(E = mean(E), N = mean(N), elev = mean(z), .groups = "drop") %>%
  mutate(x = E - min(E), y = N - min(N), elev = elev - min(elev)) %>% left_join(key, by = "plot")

# grid position from the 5 x 3 layout (rows run NE-SW; order plots by position along the long axis)
pc <- prcomp(geo[, c("x", "y")])
geo <- geo %>% mutate(along = pc$x[, 1], across = pc$x[, 2],
                      row = as.integer(cut(rank(along), 5)), col = ave(across, row, FUN = rank),
                      edge = row %in% c(1, 5) | col %in% c(1, 3))
# neighbours within 6 m centre-to-centre (adjacent plots in the grid, incl. diagonals)
d <- as.matrix(dist(geo[, c("x", "y")]))
nb_any <- function(tr) sapply(seq_len(nrow(geo)), function(i) sum(d[i, ] > 0 & d[i, ] < 6.5 & geo$treatment == tr))
geo <- geo %>% mutate(n_neigh = sapply(seq_len(n()), function(i) sum(d[i, ] > 0 & d[i, ] < 6.5)),
                      slurry_neigh = nb_any("slurry"), compost_neigh = nb_any("compost"))

# responses and background conditions
tot <- read.csv("output/tables/ghg_totals_by_plot.csv")
flux <- read.csv("data/processed/flux_estimates.csv") %>% mutate(date = as.Date(date))
pre <- flux %>% filter(date < APPLICATION_DATE) %>% group_by(plot) %>%
  summarize(pre_CO2 = mean(FCO2_DRY, na.rm = TRUE), pre_CH4 = mean(FCH4_DRY, na.rm = TRUE), pre_N2O = mean(FN2O, na.rm = TRUE))
hand <- read.csv("data/processed/field_metadata.csv") %>% mutate(date = as.Date(date))
vwc <- hand %>% group_by(plot) %>%
  summarize(vwc_pre = mean(mean_vwc[date < APPLICATION_DATE], na.rm = TRUE),
            vwc_season = mean(mean_vwc[date > APPLICATION_DATE], na.rm = TRUE))
dat <- geo %>%
  left_join(tot %>% filter(period == "season") %>% select(plot, CO2 = CO2_C_g_m2, CH4 = CH4_C_mg_m2, N2O = N2O_N_mg_m2), by = "plot") %>%
  left_join(tot %>% filter(period == "first_week") %>% select(plot, CO2_wk = CO2_C_g_m2, CH4_wk = CH4_C_mg_m2, N2O_wk = N2O_N_mg_m2), by = "plot") %>%
  left_join(pre, by = "plot") %>% left_join(vwc, by = "plot") %>%
  mutate(treatment = as_trt(treatment), logN2O = log(N2O))
write.csv(dat %>% mutate(across(where(is.numeric), ~ signif(.x, 4))), "output/tables/spatial_covariates_by_plot.csv", row.names = FALSE)

# 1) are covariates balanced among treatments? (a spatial confound would show here)
bal <- bind_rows(lapply(c("elev", "along", "across", "vwc_pre", "vwc_season", "pre_CO2", "pre_CH4", "pre_N2O"), function(v)
  tibble(test = "covariate ~ treatment (ANOVA)", response = v, covariate = "treatment",
         estimate = NA_real_, p = summary(aov(dat[[v]] ~ dat$treatment))[[1]][1, "Pr(>F)"])))
edge_tab <- table(dat$treatment, dat$edge)
bal <- bind_rows(bal, tibble(test = "edge x treatment (Fisher)", response = "edge", covariate = "treatment",
                             estimate = NA_real_, p = fisher.test(edge_tab)$p.value))

# 2) do covariates explain responses once treatment is accounted for? (partial slope, lm with treatment)
resp <- c("CO2", "CH4", "logN2O", "CO2_wk", "CH4_wk", "N2O_wk")
covs <- c("elev", "along", "across", "vwc_pre", "vwc_season", "edge", "slurry_neigh")
pre_map <- c(CO2 = "pre_CO2", CH4 = "pre_CH4", logN2O = "pre_N2O", CO2_wk = "pre_CO2", CH4_wk = "pre_CH4", N2O_wk = "pre_N2O")
cov_tests <- bind_rows(lapply(resp, function(r) bind_rows(lapply(c(covs, "background flux"), function(cv) {
  cvn <- if (cv == "background flux") pre_map[[r]] else cv
  m <- lm(as.formula(paste(r, "~ treatment +", cvn)), data = dat)
  co <- summary(m)$coefficients; rn <- grep(cvn, rownames(co), value = TRUE)[1]
  tibble(test = "response ~ treatment + covariate", response = r, covariate = cv,
         estimate = unname(co[rn, 1]), p = unname(co[rn, 4]))
}))))

# 3) spatial autocorrelation of treatment-model residuals (Moran's I, inverse-distance weights)
lw <- mat2listw({ w <- 1 / d; diag(w) <- 0; w }, style = "W")
moran <- bind_rows(lapply(c(resp, "vwc_season", "vwc_pre"), function(r) {
  res <- residuals(lm(as.formula(paste(r, "~ treatment")), data = dat))
  mt <- moran.test(res, lw, randomisation = TRUE)
  tibble(test = "Moran's I of treatment residuals", response = r, covariate = "space",
         estimate = unname(mt$estimate[1]), p = mt$p.value)
}))
out <- bind_rows(bal, cov_tests, moran) %>% mutate(estimate = signif(estimate, 3), p = signif(p, 3))
write.csv(out, "output/tables/spatial_tests.csv", row.names = FALSE)
cat("  spatial tests (p < 0.10):\n"); print(as.data.frame(out %>% filter(p < 0.10)))
cat(sprintf("  edge plots by treatment: %s\n", paste(capture.output(print(edge_tab)), collapse = " | ")))
