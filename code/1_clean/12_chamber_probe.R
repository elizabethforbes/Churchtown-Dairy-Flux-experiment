# 12_chamber_probe.R
# Soil temperature and moisture logged by the LI-COR smart chamber's soil probe
# (soilp_t, soilp_m) and chamber air temperature, for every flux measurement.
# Input:  data/raw/flux/json/*.json (SmartChamber exports)
# Output: data/intermediate/chamber_env.csv  one row per date x plot x collar
#
# Notes
# - The probe logs every second during a measurement; the median over the
#   measurement is used. 9999 is the logger's missing value.
# - soilp_m is volumetric water content (m3 m-3). Readings <= 0.01 are treated as
#   probe-not-in-soil (e.g. most of 23 Jul reads exactly 0).
# - Some JSON files overlap (the 2025-05-06 KT01 exports also hold 27 and 29 May);
#   measurements are de-duplicated by name + start time.
# - Split campaigns are merged as in 14_ghg_fluxes_goflux.R (11 Sep -> 10 Sep, 15 Oct -> 14 Oct).
# - Repeat measurements of a collar on one date (e.g. "13B2", "2A_2") are averaged.

library(jsonlite)
library(dplyr)

treatment_key <- read.csv("data/intermediate/treatment_key.csv")

files <- list.files("data/raw/flux/json", pattern = "\\.json$", full.names = TRUE)
med <- function(x, lo = -Inf) {
  x <- suppressWarnings(as.numeric(unlist(x)))
  x <- x[!is.na(x) & x < 9000 & x > lo]
  if (length(x)) median(x) else NA_real_
}

rows <- list()
for (f in files) {
  d <- fromJSON(f, simplifyVector = FALSE)
  for (ds in d$datasets) for (nm in names(ds)) {
    m <- regmatches(nm, regexec("^(\\d{4}-\\d{2}-\\d{2})[-_](\\d+)([A-Ca-c])", nm))[[1]]
    if (length(m) == 0) next
    for (rep in ds[[nm]]$reps) {
      rows[[length(rows) + 1]] <- tibble(
        name = nm, start = rep$header$Date,
        date = as.Date(m[2]), plot = as.integer(m[3]), collar = toupper(m[4]),
        soil_temp_c = med(rep$data$soilp_t),
        vwc = med(rep$data$soilp_m, lo = 0.01),
        air_temp_c = med(rep$data$chamber_t)
      )
    }
  }
}

env <- bind_rows(rows) %>%
  distinct(name, start, .keep_all = TRUE) %>%
  mutate(date = case_when(
    date == as.Date("2025-09-11") ~ as.Date("2025-09-10"),
    date == as.Date("2025-10-15") ~ as.Date("2025-10-14"),
    TRUE ~ date)) %>%
  filter(plot %in% 1:15) %>%
  group_by(date, plot, collar) %>%
  summarize(n_meas = n(),
            soil_temp_c = mean(soil_temp_c, na.rm = TRUE),
            vwc = mean(vwc, na.rm = TRUE),
            air_temp_c = mean(air_temp_c, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(c(soil_temp_c, vwc, air_temp_c), ~ ifelse(is.nan(.x), NA, .x))) %>%
  left_join(treatment_key, by = "plot") %>%
  select(date, plot, treatment, collar, n_meas, soil_temp_c, vwc, air_temp_c) %>%
  arrange(date, plot, collar)

write.csv(env, "data/intermediate/chamber_env.csv", row.names = FALSE)
cat("Wrote data/intermediate/chamber_env.csv\n")
cat(sprintf("  %d collar-dates, %d dates; soil T present %d, VWC present %d\n",
            nrow(env), n_distinct(env$date), sum(!is.na(env$soil_temp_c)), sum(!is.na(env$vwc))))
