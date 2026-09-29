# Figure captions (draft)

Panels carry only axis labels, facet labels and necessary markers; statistics and explanations are in the captions below.

Treatment encoding is the same in every figure: control is a grey circle, compost a brown triangle and slurry a blue square. Unless noted, plots are the experimental unit (n = 5 per treatment). Subsamples, such as chamber collars and incubation tubes, were averaged within each plot before summarising. Small translucent points are individual plots. Large symbols are treatment means with 95% confidence intervals.

## Main text

**Figure 1. Study design.**
(a) Plot layout from the RTK-GPS survey (UTM 18N, north up): 3 × 3 m plots shaded by treatment, flux collars as dots, plot numbers, and plot-mean elevation above the lowest corner (m; total relief 0.85 m). Treatments are balanced for elevation, position, edge location and pre-application moisture and fluxes (36_spatial.R).
(b) Timeline: manure application (28 May 2025), flux campaigns (open before application, filled after), soil samplings and the October biomass harvest.

**Figure 2. Soil greenhouse-gas fluxes through the season.**
(a) Soil temperature and (b) volumetric water content at each campaign, from the handheld probes at 10 cm (mean ± SE). Triangles mark the soil samplings (S1–S3) and the biomass harvest (H). The open symbol is 22 Aug, where temperature was gap-filled from the chamber probe (see Methods).
(c, e, g) CO2, CH4 and N2O fluxes. Small points are plot means of three collars; large symbols are treatment means ± SE. Grey shading marks the period before application.
(d, f, h) Season totals from 29 May to 14 Oct, integrated per plot by the trapezoid rule. One-way ANOVA p = 0.74 (CO2), 0.72 (CH4) and 0.57 (N2O). The right-hand column is labelled "Season total".

**Figure 3. The application pulse.**
(a, c, e) Fluxes from the day before application to day 22. Open circles are individual collars and large symbols treatment means ± SE. CO2 is shown on a log axis; CH4 and N2O on pseudo-log axes, so that hotspot collars stay visible; the time axis is compressed between days 6 and 22 (//).
(b, d, f) Cumulative flux over days 1–6 (29 May–3 Jun) per plot. Amendment minus control (Welch 95% CI): CO2 compost −1.8 (−14 to 11), slurry 11.5 (−1.9 to 25) g C m−2; CH4 compost 0.15 (−0.88 to 1.2), slurry 1.1 (−0.65 to 2.9) mg C m−2; N2O compost 0.36 (−1.2 to 1.9), slurry 4.6 (−0.23 to 9.4) mg N m−2.
CH4 is hotspot-driven, so the event count in panel c is the more appropriate test than the plot means in panel d. An emission event is a collar measurement with net CH4 emission (flux > 0). Events occurred in 4 of 45 slurry collar measurements in days 1–6, against 6 of 489 at all other times and in all other treatments (Fisher exact p = 0.006). Control plots had no events at any time. The largest events of the season were all at slurry collar 13B in days 1–6 (5.2, 1.8 and 0.52 nmol m−2 s−1); all others were below 0.5.
The strongest hotspot (plot 13, collar B) was already a weak source before application (0.26 nmol m−2 s−1 on 27 May). Slurry amplified it about 20-fold on day 1, and it returned to near zero by day 22. Event counts by treatment and period are in ch4_emission_events.csv.

**Figure 4. Soil C and N cycling across the season.**
Soils were sampled on 29 May, 21 Jul and 14 Oct. Panels are grouped in three columns:
- Microbial biomass and C mineralization: (a) substrate-induced respiration and (b) mean C mineralization rate
- N transformations over 28 days at 20 °C and 65% water-holding capacity: (c) net N mineralization and (d) net nitrification
- Extractable N: (e) KCl-extractable NH4+-N and (f) NO3−-N.

Asterisks mark one-way ANOVA p < 0.05 at that sampling (nitrate on 21 Jul and 14 Oct).

**Figure 5. Soil chemistry and plant response.**
(a) Soil pH, organic matter, CEC, base saturation and Mehlich-3 P, K, Ca and Mg at each sampling, as Hedges' g (amendment minus control, divided by the pooled plot-level SD, small-sample corrected) with 95% CI. The three points in each row are the samplings: 14 Oct, 21 Jul and 29 May from top to bottom, lighter = earlier.
(b) Aboveground dry biomass at the October harvest. Small points are plots; large symbols are means with 95% CI.
(c) Forage composition at the October harvest, as Hedges' g.

The grey band marks |g| < 0.8, the conventional "large" effect threshold. With n = 5 per treatment, only effects beyond about ±1.3–1.5 reach p < 0.05. Filled symbols are Welch p < 0.05, uncorrected.
- Biomass did not differ (ANOVA p = 0.99).
- Across the 86 soil chemistry and forage contrasts (the two screening panels), 3 had p < 0.05, fewer than the ~4 expected by chance, and none remained significant after Benjamini–Hochberg correction (effect_synthesis.csv).
- Forage PERMANOVA: p = 0.40.

**Figure 6. Non-CO2 greenhouse-gas budget.**
All values are in CO2-equivalents (GWP100, IPCC AR6: CH4 27.0, N2O 273), from plot-level trapezoid totals for 29 May–14 Oct.
(a) Season budget per treatment. Bars are component means: CH4 uptake, N2O in days 1–6, and N2O over the rest of the season. Small points are net CH4 + N2O per plot; large symbols are the treatment mean ± 95% CI.
(b) N2O emission factor: amendment-attributable N2O-N (plot minus control mean) as % of the N applied (slurry 2.75, compost 3.87 g N m−2), for days 1–6 and the season. Reference lines are the IPCC 2019 EF1 aggregate default (1%) and the value for organic N inputs in wet climates (0.6%).
(c) The non-CO2 budget in the context of the system's carbon fluxes over the same season, all in g CO2(-eq) m−2 on a log scale. These are gross fluxes shown for scale; they are not terms of a single net balance.
- **Soil respiration:** from the clipped collars, so roots plus microbes. It is integrated from midday closures (10:00–17:00), so it is likely biased high compared with a 24-h total.
- **Aboveground NPP:** the October harvest of the 0.5 m² subplot left uncut since the pre-experiment mow, assuming C = 45% of dry mass.
- **Amendment C input:** not measured. The range assumes C = 25–40% of dry matter for slurry and 15–30% for compost.
- **Result:** the non-CO2 net is 0.35–0.7% of season soil respiration and 6–11% of aboveground NPP. For the amendment effect to be offset, 10–17% of the slurry C, or 2–4% of the compost C, would have to stay in the soil through the season (point estimates).
- **Metric choice barely matters** (ghg_co2eq_metrics.csv): season net non-CO2 for the slurry plots is 42 g CO2-eq m−2 under GWP100, 38 under GWP20, 38 under GTP100 and 44 under GWP* for a sustained practice. N2O dominates, and its GWP20 equals its GWP100.

Soil CO2 is not included in panels a–b. Chamber CO2 is soil respiration (roots plus microbes), not net ecosystem exchange, so it is not a term in a GHG balance. The amendments' carbon inputs were not measured.

Results:
- N2O dominates the non-CO2 budget in every treatment: 23 (control), 33 (compost) and 44 (slurry) g CO2-eq m−2. CH4 uptake offsets about 2 g CO2-eq m−2.
- The first week is 3–7% of season N2O.
- Net budgets did not differ (ANOVA p = 0.57).
- The slurry emission factor was 0.17% (−0.01 to 0.34) over days 1–6 and 1.8% (−3.2 to 6.8) over the season. Compost was 0.01% and 0.6%.

## Supplementary

- **S1. Amendment composition.** Per-mass N forms, total solids and N on a dry-mass basis. Samples were taken 28 May 2025 and analysed January 2026. Applied rates: slurry 2.75 g N m−2 (27.5 kg N ha−1) and 143 g dry matter m−2; compost 3.87 g N m−2 (38.7 kg N ha−1) and 438 g dry matter m−2 (Table 1).
- **S2. Soil temperature and moisture as flux drivers.**
  - (a–c) Each point is one collar measurement after application (n = 445). Lines are fixed-effect fits for each treatment with 95% CI, from mixed models with random intercepts for plot and collar within plot:
    - (a) log(CO2) ~ temperature × treatment + moisture + temperature:moisture, shown at median moisture.
    - (b) CH4 ~ moisture × treatment + temperature + temperature:moisture, shown at median temperature.
    - (c) N2O ~ moisture × treatment.
    
    Drivers are handheld soil temperature (22 Aug gap-filled) and VWC. Model selection among null, T, W, T+W, T+W+W² and T×W is in the table flux_driver_model_selection.csv. The best models were T×W for CO2 and CH4. For N2O, T+W+W² was best, but the evidence for any driver was weak (null ΔAIC 5.3). The slope × treatment p-values test whether amendments changed the response. The y-axes show the 1st–99th percentile of fluxes; the models use all data.
  - (d–f) The same data coloured by the second driver, with model curves at fixed levels of that driver.
- **S3. Soil pH, organic matter, CEC, base saturation and Mehlich-3 P, K, Ca and Mg for all plots at each soil sampling.** No treatment differences.
- **S4. Soil conditions.**
  - (a) Soil moisture and (b) pH at sampling.
  - (c) Covariation of soil temperature and moisture from the handheld probe (r ≈ −0.3 across collars), which is why the two drivers could be separated in Fig. S2.
- **S5. C-mineralization time courses.** Thin lines are individual plots. The 16 Dec reading of round 3 is excluded: tube identity was uncertain and the standards were anomalous.
- **S6. Extractable N pools at day 0 and day 28 of each incubation.**

## Methods notes
- **Flux calculation.** Fluxes were recomputed from the raw 1 Hz concentration records with goFlux 0.4.0 (best.flux, Hüppi criteria) through fluxqc 0.2.3.
  - CO2 and CH4 come from the smart chamber's LI-7810 record; N2O comes from the LI-7820.
  - The window runs from the chamber's 25 s deadband to chamber opening.
  - The LI-7820 clock was aligned to the chamber per closure, by cross-correlating the H2O traces of the two instruments (offsets −3 to 105 s; details in goflux_clock_offsets_by_closure.csv).
  - The minimum detectable flux is 1.96·σ/t, with σ the MAD of first differences per instrument and day. Values below it are retained and flagged: 52% of N2O and 0.2% of CH4 closures.
  - Early seal break: in one closure (29 May 6C) the chamber opened or lost its seal about 106 s after closure, which shows as a CO2 drop of 240 ppm in 3 s. Its window was cut 1 s before the drop, giving 80 s of data. The corrected values (CO2 21.0 µmol, N2O 1.17 nmol m−2 s−1) match the same collar on 30 May (19.9 and 1.20). The rule is general: a drop of more than 15 ppm CO2 within 3 s inside the window. No other closure triggered it.
  - Clocks: the smart chamber's clock matches its GPS timestamps within 0–3 s. The LI-7820 has no time sync; its clock runs about 1 s per day fast and was reset occasionally, and the 30 May 14:12 reset is visible as duplicated seconds in its log.
  - Per-closure concentration traces for all 546 closures are in output/qc/closure_traces/.
  - Repeat closures: the last one is kept, except the 30 May 13B confirmation measurement, which is averaged with the first (3.6 and 3.9 nmol N2O m−2 s−1).
- **Handheld probe data.** Readings for 19 Jun–15 Oct were transcribed from scanned field sheets in September 2026. The file handheld_probe_transcribed_TO_VERIFY.xlsx lists them, with uncertain cells highlighted. The 10–11 Sep pages were transcribed twice independently and agreed on 45/45 VWC values and 44/45 temperatures.
- **Temperature gap-fill.**
  - Within-campaign gaps (23 Jul) used handheld ~ chamber probe + (1 | date).
  - The missing 22 Aug campaign used handheld ~ chamber probe + day of year + day of year². Its leave-one-campaign-out error on campaign means is RMSE 2.0 °C (maximum 3.9 °C).
- **Colour palette.** Colours are colorblind-safe (CIEDE2000 ≥ 24 between treatments under simulated deuteranopia, protanopia and tritanopia), and treatments also differ in marker shape.
- **Export.** Figures are vector PDF plus 300 dpi PNG at 180 mm width.
