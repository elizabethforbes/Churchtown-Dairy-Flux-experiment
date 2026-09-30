# compare_goflux_soilfluxpro.R
# QC: compares the goFlux closure fluxes (code/1_clean/14_ghg_fluxes_goflux.R) with the
# LI-COR SoilFluxPro export (code/4_qc/soilfluxpro_export.R), which was the original
# flux source and is kept only for this comparison.
# Output: output/qc/goflux_vs_soilfluxpro.csv
suppressPackageStartupMessages({library(dplyr)})
dir.create("output/qc", showWarnings = FALSE, recursive = TRUE)
est <- read.csv("data/intermediate/flux_estimates.csv") %>% mutate(date = as.Date(date))
sfp <- read.csv("data/intermediate/flux_estimates_soilfluxpro.csv") %>% mutate(date = as.Date(date))
cmp <- est %>% inner_join(sfp %>% select(date, plot, collar, sfp_CO2 = FCO2_DRY, sfp_CH4 = FCH4_DRY, sfp_N2O = FN2O),
                          by = c("date", "plot", "collar"))
cmp_tab <- bind_rows(lapply(c("CO2", "CH4", "N2O"), function(g) {
  a <- cmp[[c(CO2 = "FCO2_DRY", CH4 = "FCH4_DRY", N2O = "FN2O")[g]]]; b <- cmp[[paste0("sfp_", g)]]
  ok <- complete.cases(a, b)
  tibble(gas = g, n = sum(ok), r = cor(a[ok], b[ok]), median_ratio = median(a[ok] / b[ok]),
         goflux_range = paste(signif(range(a[ok]), 3), collapse = " to "),
         soilfluxpro_range = paste(signif(range(b[ok]), 3), collapse = " to "))
}))
write.csv(cmp_tab, "output/qc/goflux_vs_soilfluxpro.csv", row.names = FALSE)
cat("\ngoFlux vs SoilFluxPro:\n"); print(as.data.frame(cmp_tab))
