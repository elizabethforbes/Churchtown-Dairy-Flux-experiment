# Data

- `raw/`: original files as received from the instruments, labs and field sheets.
  The cleaning scripts (`code/1_clean/`) read these files and are the only scripts that do.
- `intermediate/`: products of the cleaning scripts, one or more per data source.
- `clean/`: the final data tables used in the analysis, described column by column
  (with units) in `clean/data_dictionary.csv`.

Design and treatment codes are given in the top-level `README.md`.

## Raw files

Files marked *not used* are kept as received for completeness; no script reads them.

### `raw/flux/`: chamber greenhouse gas fluxes

| File | Contents | Read by |
|------|----------|---------|
| `json/*.json` | LI-COR LI-8200 Smart Chamber exports with LI-7810 CO2, CH4 and H2O records, one per field day. `2025-05-06-KT01*.json` are the 6 May baseline exports; they overlap and duplicate records are dropped. `2025_05_06_KT01.json` is an empty export and is skipped. | `12_chamber_probe.R`, `14_ghg_fluxes_goflux.R` |
| `data/TG20-*.data` | LI-7820 N2O and H2O records (1 Hz), logged on the analyzer's own clock | `14_ghg_fluxes_goflux.R` |
| `data/TG10-*.data` | LI-7810 internal log for 6 May (the LI-7810 data used come from the Smart Chamber exports) | *not used* |
| `co2_n20_ch4_allflux2025.xlsx` | Fluxes calculated in LI-COR SoilFluxPro | `4_qc/` comparison only |
| `smartchamber_data_key.xlsx` | Description of the Smart Chamber export fields | documentation |

The scanned field datasheets (collar order, closure times, handheld readings) are
archived with the data release rather than in this repository.

### `raw/field_metadata/`: field readings and positions

| File | Contents | Read by |
|------|----------|---------|
| `In situ fluxes meta-data.xlsx` | Handheld soil temperature and VWC, 27 May to 3 June (typed in 2025) | `06_field_probe_readings.R` |
| `handheld_probe_transcribed_2026-09-28.xlsx`, `.csv` | Handheld readings for 19 June to 15 October, transcribed from the scanned field sheets; uncertain entries flagged. The script reads the CSV. | `06_field_probe_readings.R` |
| `KT-CTD-Plots.csv`, `KT-CTD-Collars.csv` | RTK-GPS positions (UTM 18N) of plot corners and collars | `20_write_clean_tables.R` |
| `GIS Cleaned Data.xlsx`, `KT-GIS Cleaned Data.csv` | Earlier versions of the GPS survey | *not used* |

### `raw/soil/`: laboratory soil assays

Three soil samplings (29 May, 21 July, 14 October 2025) are numbered 1–3 in the file names.

| Folder | Contents | Read by |
|--------|----------|---------|
| `gwc/` | Gravimetric water content | `01_soil_gwc.R` |
| `ph/` | Soil pH | `02_soil_ph.R` |
| `sir/` | Substrate-induced respiration: headspace CO2 by LGR laser analyzer (sampling 1) or infrared gas analyzer, IRGA (samplings 2–3; the LGR sheet in `SIR_2.xlsx` is empty) | `03_soil_sir.R` |
| `cmin/` | C mineralization, five or six headspace measurements per sampling: LGR (sampling 1) or IRGA (samplings 2–3). `flushing_20250807.xlsx` holds the flush and measurement times for 8 August. | `04_soil_cmin.R` |
| `cmin_mass/` | Soil masses and water content for the C- and N-mineralization tubes | `04_soil_cmin.R`, `11_soil_nmin.R` |
| `nmin/Yale_inorgN_2026.xlsx` | KCl-extractable NO3-N and NH4-N (mg N L^-1 of extract). Rows from other projects analysed in the same runs have been removed from this copy. | `11_soil_nmin.R` |
| `manure_amendment/` | Masses for KCl extractions of the slurry and compost themselves | `11_soil_nmin.R` |
| `paper_datasheets/` | Photographs of the handwritten lab datasheets, used to resolve sample-label conflicts (see comments in `11_soil_nmin.R`) | reference |
| `microbes/` | Inventory of archived DNA samples | *not used* |

The LGR and IRGA calculations, constants and data corrections are documented in the
comments of `03_soil_sir.R` and `04_soil_cmin.R`. In brief: LGR headspace CO2 is
reconstructed from the injection dilution using the loop volume measured with standards
on each run, and both methods convert to µg CO2-C with the ideal gas law at 20 °C using
the incubation-jar headspace volume.

### `raw/dairy_one/`: commercial laboratory analyses (Dairy One, Ithaca, NY)

| Folder | Contents | Read by |
|--------|----------|---------|
| `soil/` | Soil tests for all plots and samplings. `DairyOne_soil.csv` is the lab export (`DairyOne_soildata.csv`) annotated with plot numbers. | `07_soil_chemistry_dairyone.R` |
| `forage/` | Forage quality, `DairyOne_forage.CSV`; `DairyOne_forage.xlsx` is the same table with a variable key (all values % of dry matter) | `09_forage_quality.R` |
| `manure/` | Slurry and compost composition, `DC394691-26.CSV` | `10_amendments.R` |

The PDF reports and XML exports in each folder are the lab's original deliverables and
are *not used* by the scripts.

### Other

| File | Contents | Read by |
|------|----------|---------|
| `treatment_list.xlsx` | Plot-to-treatment key | `00_plots_treatment_key.R` |
| `vegetation/vegetation biomass.xlsx` | Aboveground biomass, October harvest | `08_biomass.R` |
