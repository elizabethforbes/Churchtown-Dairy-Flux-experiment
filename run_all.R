# run_all.R
# Reproduces the whole study from the raw data: run from the repository root with
#   Rscript run_all.R
# Stage 1 (code/1_clean):    raw data -> data/intermediate -> data/clean (final tables)
# Stage 2 (code/2_analysis): data/clean -> statistics (output/tables)
# Stage 3 (code/3_figures):  data/clean + output/tables -> figures (output/figures)
# Stage 4 (code/4_qc):       quality-control outputs (output/qc), not used in the paper
# Each script runs in a fresh R session, so it depends only on the files it reads.

# output folders (not all are kept in git)
for (d in c("data/intermediate", "data/clean", "output/tables", "output/figures/main", "output/figures/si",
            "output/qc/closure_traces"))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

scripts <- c(
  # 1. data cleaning: one script per data source, then the final tables
  "code/1_clean/00_plots_treatment_key.R",
  "code/1_clean/01_soil_gwc.R",
  "code/1_clean/02_soil_ph.R",
  "code/1_clean/03_soil_sir.R",
  "code/1_clean/04_soil_cmin.R",
  "code/1_clean/06_field_probe_readings.R",
  "code/1_clean/07_soil_chemistry_dairyone.R",
  "code/1_clean/08_biomass.R",
  "code/1_clean/09_forage_quality.R",
  "code/1_clean/10_amendments.R",
  "code/1_clean/11_soil_nmin.R",
  "code/1_clean/12_chamber_probe.R",
  "code/1_clean/13_gapfill_soil_temp.R",
  "code/1_clean/14_ghg_fluxes_goflux.R",
  "code/1_clean/20_write_clean_tables.R",
  # 2. analysis
  "code/2_analysis/01_plot_totals.R",
  "code/2_analysis/02_treatment_effects.R",
  "code/2_analysis/03_flux_drivers.R",
  "code/2_analysis/04_repeated_measures.R",
  "code/2_analysis/05_ghg_budget.R",
  "code/2_analysis/06_storage_vs_field.R",
  "code/2_analysis/07_spatial.R",
  "code/2_analysis/08_effect_synthesis.R",
  # 3. figures
  "code/3_figures/fig1_design.R",
  "code/3_figures/fig2_season_fluxes.R",
  "code/3_figures/fig3_application_pulse.R",
  "code/3_figures/fig4_soil_c_n.R",
  "code/3_figures/fig5_soil_chemistry_plants.R",
  "code/3_figures/fig6_ghg_budget.R",
  "code/3_figures/figS1_flux_drivers.R",
  "code/3_figures/figS2_soil_chemistry.R",
  "code/3_figures/figS3_soil_conditions.R",
  "code/3_figures/figS4_cmin_timecourses.R",
  "code/3_figures/figS5_nmin_pools.R",
  # 4. quality control
  "code/4_qc/soilfluxpro_export.R",
  "code/4_qc/compare_goflux_soilfluxpro.R",
  "code/4_qc/plot_closure_traces.R"
)
for (f in scripts) {
  cat(sprintf("\n=== %s ===\n", f))
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(f))
  if (status != 0) stop(sprintf("%s failed (exit status %d)", f, status))
}
cat("\n=== PIPELINE COMPLETE ===\n")
