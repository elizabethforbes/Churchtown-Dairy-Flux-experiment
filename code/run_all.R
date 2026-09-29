invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))  # figure scripts contain UTF-8 symbols

# run_all.R — Run the entire Churchtown Dairy processing + analysis pipeline
# Run from the project root directory
#
# Dependency order:
#   Stage 1 (processing): raw → data/processed/*.csv
#     00 treatment_key  (no dependencies)
#     01 gwc            (depends on: treatment_key)
#     02 ph             (depends on: treatment_key)
#     03 sir            (depends on: treatment_key, gwc)
#     04 cmin           (depends on: treatment_key, gwc)
#     05 flux           (depends on: treatment_key) -> SoilFluxPro export, comparison only
#     06 field_metadata (depends on: treatment_key)
#     07 dairy_one      (depends on: treatment_key)
#     08 biomass        (depends on: treatment_key)
#     09 dairy_one_forage (depends on: treatment_key)
#     10 dairy_one_manure (no dependencies)
#     11 nmin           (depends on: treatment_key, gwc, dairy_one_manure)
#     12 chamber_env    (depends on: treatment_key)
#     13 gapfill_soil_temp (depends on: field_metadata, chamber_env)
#     14 goflux_fluxes  (depends on: treatment_key, 05 soilfluxpro export for comparison)
#                       writes flux_estimates.csv (analysis table) from raw data
#
#   Stage 2 (analysis): data/processed/*.csv → summaries + figures
#     10 lab_assays_summary (depends on: gwc, ph, sir, cmin_cumulative, nmin_plot)
#     30 main_figures       (depends on: all processed CSVs; uses fig_setup.R)
#     31 si_figures         (depends on: gwc, ph, field_metadata, cmin, nmin, forage)

cat("=== STAGE 1: PROCESSING ===\n\n")

cat("--- 00: Treatment Key ---\n")
source("code/processing/00_clean_treatment_key.R")

cat("\n--- 01: GWC ---\n")
source("code/processing/01_process_gwc.R")

cat("\n--- 02: pH ---\n")
source("code/processing/02_process_ph.R")

cat("\n--- 03: SIR ---\n")
source("code/processing/03_process_sir.R")

cat("\n--- 04: C Mineralization ---\n")
source("code/processing/04_process_cmin.R")

cat("\n--- 05: Flux ---\n")
source("code/processing/05_process_flux.R")

cat("\n--- 06: Field Metadata ---\n")
source("code/processing/06_process_field_metadata.R")

cat("\n--- 07: Dairy One ---\n")
source("code/processing/07_process_dairy_one.R")

cat("\n--- 08: Biomass ---\n")
source("code/processing/08_process_biomass.R")

cat("\n--- 09: Dairy One Forage ---\n")
source("code/processing/09_process_dairy_one_forage.R")

cat("\n--- 10: Dairy One Manure ---\n")
source("code/processing/10_process_dairy_one_manure.R")

cat("\n--- 11: N Mineralization ---\n")
source("code/processing/11_process_nmin.R")

cat("\n--- 12: Chamber soil probe / air temperature ---\n")
source("code/processing/12_process_chamber_env.R")

cat("\n--- 13: Gap-fill handheld soil temperature ---\n")
source("code/processing/13_gapfill_soil_temp.R")

cat("\n--- 14: goFlux flux reprocessing (CO2, CH4, N2O from raw 1 Hz data) ---\n")
source("code/processing/14_goflux_fluxes.R")

cat("\n=== STAGE 2: ANALYSIS ===\n\n")

cat("--- 10: Lab Assays Summary ---\n")
source("code/analysis/10_lab_assays_summary.R")

cat("\n--- 30: Main-text figures ---\n")
source("code/analysis/30_main_figures.R")

cat("\n--- 31: SI figures ---\n")
source("code/analysis/31_si_figures.R")

cat("\n--- 32: Effect-size synthesis (Fig 6) + CO2-eq budget ---\n")
source("code/analysis/32_synthesis_figure.R")

cat("\n--- 33: Non-CO2 GHG budget (Fig 7) ---\n")
source("code/analysis/33_ghg_budget.R")

cat("\n--- 34: Repeated-measures models, minimum detectable effects ---\n")
source("code/analysis/34_repeated_measures.R")

cat("\n--- 35: Storage vs field emissions per kg manure N ---\n")
source("code/analysis/35_storage_vs_field.R")

cat("\n--- 36: Spatial structure of plot responses ---\n")
source("code/analysis/36_spatial.R")

cat("\n--- QC: per-closure concentration traces ---\n")
source("code/qc/plot_closure_traces.R")

cat("\n=== PIPELINE COMPLETE ===\n")
