# compare_goflux_soilfluxpro.R
# QC: compares the goFlux closure fluxes (code/1_clean/14_ghg_fluxes_goflux.R) with the
# LI-COR SoilFluxPro export (code/4_qc/soilfluxpro_export.R), which was the original
# flux source and is kept only for this comparison.
# The two differ in the fitting method and slightly in the system volume: goFlux builds
# the loop explicitly (2 analyzers x 28 cm3 + 2 tubing branches x 33.97 cm3 = 123.94 cm3,
# see 14_ghg_fluxes_goflux.R); SoilFluxPro's VOLUME_TOTAL = chamber header TotalVolume +
# 67.94 cm3 (114.9 cm3 of loop from 30 May, 129.9 before). Volume ratio goFlux /
# SoilFluxPro ~1.0015 from 30 May, ~0.999 before.
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
