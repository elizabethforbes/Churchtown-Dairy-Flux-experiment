# Churchtown Dairy manure experiment

Data and code for a one-season field experiment comparing dairy slurry and compost
applied to a hayfield at Churchtown Dairy (Livingston/Claverack, New York), 2025:
soil CO2, CH4 and N2O fluxes, soil microbial C and N processes, soil chemistry, and
forage yield and quality.

## Design

- 15 plots (3 x 3 m), 3 treatments, n = 5:
  control (plots 2, 5, 7, 10, 15), slurry (1, 6, 11, 12, 13), compost (3, 4, 8, 9, 14)
- Amendments applied 28 May 2025
- 12 flux campaigns (6 May to 14 October), three collars per plot
- Soil sampled 29 May, 21 July and 14 October; biomass harvested 24 October

## Reproducing the analysis

From the repository root:

```bash
Rscript run_all.R
```

This runs every script in order, each in a fresh R session, from the raw data to the
final tables, statistics and figures. It has been checked from a fresh clone with all generated
files deleted: every table in `data/clean/` and `output/tables/` is reproduced byte-for-byte. R 4.4 with the packages dplyr, tidyr, readxl,
jsonlite, ggplot2, patchwork, scales, colorspace, ragg, lme4, lmerTest, emmeans, vegan,
spdep and goFlux.

## How the repository is organized

The workflow runs in one direction: **raw data → clean tables → analysis → figures**.
Analysis and figure scripts read only `data/clean/` and `output/tables/`, never the raw
files.

```
run_all.R                   single entry point
data/
  raw/                      original files as received (field sheets, instrument exports,
                            lab spreadsheets, Dairy One reports, GPS survey)
  intermediate/             products of the cleaning scripts (not used directly downstream)
  clean/                    FINAL data tables, one per data product, + data_dictionary.csv
code/
  1_clean/                  raw -> intermediate -> clean; one script per data source
  2_analysis/               clean -> statistics (output/tables)
  3_figures/                clean + statistics -> figures (output/figures)
  4_qc/                     quality-control checks (output/qc); not used in the paper
  lib/                      shared helpers (clean_helpers.R; setup.R: constants,
                            statistics helpers, figure theme)
  archive/                  superseded scripts, kept for reference
output/
  tables/                   statistics reported in the paper
  figures/main, figures/si  manuscript figures (PDF and PNG)
  figures/captions.md       figure captions
  qc/                       QC tables and per-closure concentration traces
```

## Final data tables (`data/clean/`)

Each column is described (with units) in `data/clean/data_dictionary.csv`.

| File | Contents |
|------|----------|
| `plots.csv` | plot and treatment |
| `plot_corners.csv`, `collars.csv` | RTK-GPS positions (UTM 18N) of plot corners and flux collars |
| `amendment_composition.csv` | Dairy One analyses of the slurry and compost (three samples each) |
| `amendment_application.csv` | material, dry matter and N applied per treatment |
| `ghg_fluxes.csv` | CO2, CH4 and N2O flux for every collar and campaign, with detection limits and QC flags |
| `field_probe_readings.csv` | soil temperature and volumetric water content at each collar and campaign |
| `soil_gwc.csv`, `soil_ph.csv` | gravimetric water content and pH, per subsample |
| `soil_sir.csv` | substrate-induced respiration, per replicate |
| `soil_cmin_timecourse.csv`, `soil_cmin_cumulative.csv` | C-mineralization rates per measurement day, and cumulative per tube |
| `soil_nmin.csv` | extractable N and net N mineralization and nitrification, per plot and sampling |
| `soil_by_plot.csv` | plot x sampling summary of all soil laboratory variables (the values analysed) |
| `soil_chemistry.csv` | Dairy One soil tests (Mehlich-3, Morgan, pH, organic matter, CEC, base saturation) |
| `biomass.csv` | aboveground biomass at the October harvest |
| `forage_quality.csv` | Dairy One forage composition (dry-matter basis) |

## Scripts

**1_clean** (raw → clean). Data-hygiene decisions (typo fixes, sample-label conflicts,
analyzer volume calibration, clock alignment) are made and commented in these scripts.

| Script | Data |
|--------|------|
| `00_plots_treatment_key.R` | plot-treatment key |
| `01_soil_gwc.R`, `02_soil_ph.R` | soil moisture and pH |
| `03_soil_sir.R`, `04_soil_cmin.R` | SIR and C mineralization (LGR and IRGA headspace analyses) |
| `06_field_probe_readings.R` | handheld soil temperature and moisture |
| `07_soil_chemistry_dairyone.R` | Dairy One soil tests |
| `08_biomass.R`, `09_forage_quality.R` | biomass and forage quality |
| `10_amendments.R` | amendment composition |
| `11_soil_nmin.R` | KCl-extractable N and N mineralization |
| `12_chamber_probe.R`, `13_gapfill_soil_temp.R` | chamber soil probe; gap-filling of handheld soil temperature |
| `14_ghg_fluxes_goflux.R` | fluxes recomputed from the raw 1-Hz analyzer records with goFlux |
| `20_write_clean_tables.R` | writes `data/clean/` and the data dictionary |

**2_analysis** (clean → `output/tables/`)

| Script | Analysis |
|--------|----------|
| `01_plot_totals.R` | cumulative fluxes per plot (season; days 1-6), CH4 emission events, soil metrics per plot |
| `02_treatment_effects.R` | amendment - control contrasts (Welch, Hedges' g, ANOVA); forage PERMANOVA |
| `03_flux_drivers.R` | temperature and moisture models of collar fluxes |
| `04_repeated_measures.R` | repeated-measures mixed models (fluxes, soil), metabolic quotient, N supply, minimum detectable differences |
| `05_ghg_budget.R` | non-CO2 budget (CO2-eq), N2O emission factors, metric sensitivity |
| `06_storage_vs_field.R` | field-phase vs IPCC storage-phase emissions per kg manure N |
| `07_spatial.R` | spatial balance and autocorrelation checks; baseline-adjusted effects |
| `08_effect_synthesis.R` | Hedges' g for all responses; Benjamini-Hochberg within screening panels |
| `09_heterogeneity.R` | spatial heterogeneity of responses: plot-level spread and top-plot share; within-plot collar spread before vs after application |

**3_figures**: one script per figure (`fig1_design.R` ... `fig6_ghg_budget.R`,
`figS1_flux_drivers.R` ... `figS5_nmin_pools.R`).

**4_qc**: SoilFluxPro export and comparison with goFlux; per-closure concentration traces.

## Data availability

The final tables and code are archived at Zenodo (DOI to be added on publication).
