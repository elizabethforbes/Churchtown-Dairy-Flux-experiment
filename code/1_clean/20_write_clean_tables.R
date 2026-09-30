# 20_write_clean_tables.R
# Writes the final data tables (data/clean/), one per data product, from the
# processed intermediates of scripts 00-14. These are the only data files the
# analysis (code/2_analysis) and figure (code/3_figures) scripts read, and they are
# the archived data release. data/clean/data_dictionary.csv describes every column.
#
# Replicates are kept at the level they were measured (closure, tube, extraction);
# soil_by_plot.csv gives the replicate-averaged plot x sampling values used in the
# statistics.

suppressPackageStartupMessages({library(dplyr); library(tidyr)})
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
INT <- "data/intermediate"
rd <- function(f) read.csv(file.path(INT, f))
out <- function(d, f) { write.csv(d, file.path("data/clean", f), row.names = FALSE)
  cat(sprintf("  data/clean/%-28s %4d rows\n", f, nrow(d))) }
SAMPLING_DATES <- c(`1` = "2025-05-29", `2` = "2025-07-21", `3` = "2025-10-14")

# --- design -------------------------------------------------------------------------
plots <- rd("treatment_key.csv") %>% arrange(plot)
out(plots, "plots.csv")
gps <- function(f) read.csv(file.path("data/raw/field_metadata", f))
out(gps("KT-CTD-Plots.csv") %>% transmute(plot = as.integer(Name), easting_m = Easting, northing_m = Northing,
                                         elevation_m = Elevation), "plot_corners.csv")
out(gps("KT-CTD-Collars.csv") %>% transmute(plot = as.integer(sub("[A-C]$", "", Name)), collar = sub("^[0-9]+", "", Name),
                                           easting_m = Easting, northing_m = Northing, elevation_m = Elevation),
    "collars.csv")

# --- amendments ---------------------------------------------------------------------
manure <- rd("dairy_one_manure.csv")
out(manure %>% select(-ash_pct), "amendment_composition.csv")
# Application: 5 gal slurry and 18 lb compost per 3 x 3 m plot (field notes); slurry
# density taken as 1 kg/L; composition = mean of the three Dairy One samples.
PLOT_AREA_M2 <- 9
APPLIED_FRESH_KG <- c(slurry = 5 * 3.785, compost = 18 * 0.4536)
app <- manure %>% group_by(amendment_type) %>%
  summarize(ts = mean(total_solids_pct) / 100, tn = mean(total_n_pct) / 100,
            nh4 = mean(ammonium_n_pct) / 100, org = mean(organic_n_pct) / 100, .groups = "drop") %>%
  mutate(fresh_kg_m2 = APPLIED_FRESH_KG[amendment_type] / PLOT_AREA_M2,
         dm_g_m2 = fresh_kg_m2 * ts * 1000,
         n_g_m2 = fresh_kg_m2 * tn * 1000, nh4_g_m2 = fresh_kg_m2 * nh4 * 1000, org_g_m2 = fresh_kg_m2 * org * 1000,
         treatment = amendment_type) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)), n_kg_ha = n_g_m2 * 10, dm_Mg_ha = dm_g_m2 / 100) %>%
  select(treatment, ts, tn, nh4, org, fresh_kg_m2, dm_g_m2, n_g_m2, nh4_g_m2, org_g_m2, n_kg_ha, dm_Mg_ha)
out(app, "amendment_application.csv")

# --- greenhouse gas fluxes and field conditions ----------------------------------------
out(rd("flux_estimates.csv"), "ghg_fluxes.csv")
out(rd("field_metadata.csv") %>%
      select(date, plot, treatment, collar, time, vwc1, vwc2, vwc3, mean_vwc, soil_temp_c,
             soil_temp_source, soil_temp_filled_c, uncertain, notes),
    "field_probe_readings.csv")

# --- soil laboratory assays -----------------------------------------------------------
gwc  <- rd("gwc.csv");  out(gwc, "soil_gwc.csv")
ph   <- rd("ph.csv");   out(ph, "soil_ph.csv")
sir  <- rd("sir.csv");  out(sir %>% select(-flag), "soil_sir.csv")
cmin_tr <- rd("cmin_timeresolved.csv")
out(cmin_tr %>% select(plot, treatment, timepoint, lab_no, replicate, method, incubation_start, date, day,
                       cmin_rate_ug_co2c_hr_g, flag), "soil_cmin_timecourse.csv")
cmin <- rd("cmin_cumulative.csv"); out(cmin, "soil_cmin_cumulative.csv")
nmin <- rd("nmin_plot.csv");       out(nmin, "soil_nmin.csv")

# plot x sampling summary: replicates averaged within plot; where a sampling was
# measured by both the LGR and IRGA analyzers, the IRGA value is used.
prefer_irga <- function(d) d %>% group_by(plot, timepoint) %>% arrange(desc(method == "IRGA")) %>%
  slice_head(n = 1) %>% ungroup()
soil_by_plot <- expand_grid(plot = 1:15, timepoint = 1:3) %>%
  left_join(plots, by = "plot") %>%
  left_join(gwc %>% group_by(plot, timepoint) %>% summarize(gwc = mean(gwc, na.rm = TRUE), .groups = "drop"),
            by = c("plot", "timepoint")) %>%
  left_join(ph %>% group_by(plot, timepoint) %>% summarize(ph = mean(ph, na.rm = TRUE), .groups = "drop"),
            by = c("plot", "timepoint")) %>%
  left_join(sir %>% group_by(plot, timepoint, method) %>%
              summarize(sir_ug_co2c_hr_g = mean(sir_ug_co2c_hr_g, na.rm = TRUE), .groups = "drop") %>%
              prefer_irga() %>% select(plot, timepoint, sir_ug_co2c_hr_g, sir_method = method),
            by = c("plot", "timepoint")) %>%
  left_join(cmin %>% group_by(plot, timepoint, method) %>%
              summarize(cumulative_ug_co2c_g = mean(cumulative_ug_co2c_g, na.rm = TRUE),
                        cmin_rate_ug_co2c_g_d = mean(mean_rate_ug_co2c_g_d, na.rm = TRUE), .groups = "drop") %>%
              prefer_irga() %>% select(plot, timepoint, cumulative_ug_co2c_g, cmin_rate_ug_co2c_g_d, cmin_method = method),
            by = c("plot", "timepoint")) %>%
  left_join(nmin %>% select(plot, timepoint = round, initial_nh4_ug_g, initial_no3_ug_g, initial_tin_ug_g,
                            incubated_nh4_ug_g, incubated_no3_ug_g, incubated_tin_ug_g, incubation_days,
                            net_min_rate_ug_g_d, net_nitr_rate_ug_g_d),
            by = c("plot", "timepoint")) %>%
  mutate(sampling_date = SAMPLING_DATES[as.character(timepoint)]) %>%
  relocate(sampling_date, .after = timepoint) %>% arrange(timepoint, plot)
out(soil_by_plot, "soil_by_plot.csv")

# --- soil chemistry and plants ----------------------------------------------------------
out(rd("dairy_one_clean.csv") %>% select(-lab_field), "soil_chemistry.csv")
out(rd("biomass.csv"), "biomass.csv")
out(rd("dairy_one_forage.csv"), "forage_quality.csv")

# --- data dictionary ------------------------------------------------------------------
source("code/1_clean/data_dictionary.R")
chk <- lapply(list.files("data/clean", pattern = "\\.csv$"), function(f) {
  if (f == "data_dictionary.csv") return(NULL)
  cols <- names(read.csv(file.path("data/clean", f), nrows = 1, check.names = FALSE))
  miss <- setdiff(cols, DICT$column[DICT$file == f])
  if (length(miss)) stop(sprintf("data_dictionary: %s lacks %s", f, paste(miss, collapse = ", ")))
})
out(DICT, "data_dictionary.csv")
