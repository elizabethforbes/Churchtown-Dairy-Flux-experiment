# Figure captions (draft)

Treatment encoding is the same in every figure: control is a grey circle, compost a brown triangle and slurry a blue square. Unless noted, plots are the experimental unit (n = 5 per treatment). Subsamples, such as chamber collars and incubation tubes, were averaged within each plot before summarising. Small translucent points are individual plots. Large symbols are treatment means with 95% confidence intervals.

## Main text

**Figure 1. Study timeline and amendment inputs.**
(a) Manure application (28 May 2025), chamber GHG flux campaigns (open symbols before application, filled after), soil sampling (rounds 1–3) and the October biomass harvest.
(b) Fresh mass, (c) dry matter and (d) nitrogen applied per m2.
- The amounts assume the planned rates from the field notes: 5 gal slurry and 18 lb compost per 3 × 3 m plot, with slurry density taken as 1 kg L−1.
- Composition is the mean of three Dairy One analyses per amendment. In panel d, light shading is ammonium N and dark shading organic N.
- Totals: slurry 2.75 g N m−2 (27.5 kg N ha−1) and 143 g dry matter m−2; compost 3.87 g N m−2 (38.7 kg N ha−1) and 438 g dry matter m−2.

**Figure 2. Soil greenhouse-gas fluxes through the season.**
(a) Soil temperature and (b) volumetric water content at each campaign, from the handheld probes at 10 cm (mean ± SE). The open symbol is 22 Aug, where temperature was gap-filled from the chamber probe (see Methods).
(c, e, g) CO2, CH4 and N2O fluxes. Small points are plot means of three collars; large symbols are treatment means ± SE. Grey shading marks the period before application.
(d, f, h) Season totals from 29 May to 14 Oct, integrated per plot by the trapezoid rule. p-values are from one-way ANOVA.

**Figure 3. The application pulse.**
(a, c, e) Fluxes from the day before application to day 22. Open circles are individual collars and large symbols treatment means ± SE. CO2 is shown on a log axis; CH4 and N2O on pseudo-log axes, so that hotspot collars stay visible.
(b, d, f) Cumulative flux over days 1–6 (29 May–3 Jun) per plot. Text gives each amendment minus control with its Welch 95% CI.
CH4 is hotspot-driven, so the event count in panel c is the more appropriate test than the plot means in panel d. An emission event is a collar measurement with net CH4 emission (flux > 0). Events occurred in 4 of 45 slurry collar measurements in days 1–6, against 6 of 489 at all other times and in all other treatments (Fisher exact p = 0.006). Control plots had no events at any time. The largest events of the season were all at slurry collar 13B in days 1–6 (5.2, 1.8 and 0.52 nmol m−2 s−1); all others were below 0.5.
The strongest hotspot (plot 13, collar B) was already a weak source before application (0.26 nmol m−2 s−1 on 27 May). Slurry amplified it about 20-fold on day 1, and it returned to near zero by day 22. Event counts by treatment and period are in ch4_emission_events.csv.

**Figure 4. Soil temperature and moisture as flux drivers.**
Each point is one collar measurement after application (n = 445). Lines are fixed-effect fits for each treatment with 95% CI, from mixed models with random intercepts for plot and collar within plot:
- (a) log(CO2) ~ temperature × treatment + moisture + temperature:moisture, shown at median moisture.
- (b) CH4 ~ moisture × treatment + temperature + temperature:moisture, shown at median temperature.
- (c) N2O ~ moisture × treatment.

Drivers are handheld soil temperature (22 Aug gap-filled) and VWC. Model selection among null, T, W, T+W, T+W+W² and T×W is in the table flux_driver_model_selection.csv. The best models were T×W for CO2 and CH4. For N2O, T+W+W² was best, but the evidence for any driver was weak (null ΔAIC 5.4). The slope × treatment p-values test whether amendments changed the response. The y-axes show the 1st–99th percentile of fluxes; the models use all data.

**Figure 5. Soil N and C cycling across the season.**
Soils were sampled on 29 May, 21 Jul and 14 Oct. Panels show:
- (a) KCl-extractable NH4+-N and (b) NO3−-N
- (c) net N mineralization and (d) net nitrification over 28 days at 20 °C and 65% water-holding capacity
- (e) substrate-induced respiration
- (f) mean C mineralization rate.

p-values are shown where a one-way ANOVA gave p < 0.05.

**Figure 6. Every amendment effect in the study, on one scale.**
- **Scale:** Hedges' g (amendment minus control, divided by the pooled plot-level SD, with a small-sample correction) with 95% CI. Plots are the experimental unit (n = 5 per treatment). The grey band marks |g| < 0.8, the conventional threshold for a "large" effect; with n = 5 per group, only effects beyond about ±1.3–1.5 reach p < 0.05.
- **Panels:**
  - (a) GHG plot totals for days 1–6 and for the season (29 May–14 Oct).
  - (b) Soil N and C cycling.
  - (c) Dairy One soil tests. In panels b and c, the three points in each row are the three samplings (14 Oct, 21 Jul, 29 May from top to bottom; lighter = earlier).
  - (d) October biomass and forage composition.
- **Symbols:** filled = Welch p < 0.05, uncorrected.
- **Result:**
  - 5 of 136 effects had p < 0.05, fewer than the ~7 expected by chance, and none survived Benjamini–Hochberg FDR correction (all q > 0.9). Values are in effect_synthesis.csv.
  - The first-week slurry N2O and CO2 pulses (Fig. 3) are the largest GHG effects (N2O g = 1.49, CO2 g = 1.14). Their Welch tests narrowly miss p < 0.05 (0.057 and 0.083), but the one-way ANOVA across all three treatments gives p = 0.018 and 0.056.

## Supplementary

- **S1. Amendment composition (Dairy One).** Per-mass N forms, total solids and N on a dry-mass basis. Samples were taken 28 May 2025 and analysed January 2026.
- **S2. GHG fluxes minus control, by campaign.** Mean difference of plot means with Welch 95% CI.
- **S3. Soil metrics of Fig. 5 minus control.** Welch 95% CI.
- **S4. Soil moisture and pH at sampling, and instrument agreement.** Handheld vs chamber probe on the same collars across all campaigns. The chamber probe temperature tracks air temperature and reads 5–10 °C warmer than soil in summer; its moisture agrees poorly with the handheld probe (r ≈ 0.26).
- **S5. C-mineralization time courses.** Thin lines are individual plots. The 16 Dec reading of round 3 is excluded: tube identity was uncertain and the standards were anomalous.
- **S6. Extractable N pools at day 0 and day 28 of each incubation.**
- **S7. Plant response.**
  - (a) Aboveground biomass.
  - (b) Forage composition as the percent difference from the control mean; points are amendment plots and symbols mean ± 95% CI. The starch axis is truncated.
  - PERMANOVA on all 19 standardized variables: p = 0.40. No variable differs after Benjamini–Hochberg FDR correction.
- **S8. Fig. 4 data coloured by the second driver.** Model curves are shown at fixed levels of that driver.
- **S9. Dairy One soil tests (Mehlich-3 and Morgan) for all plots at each soil sampling.** No treatment differences.
- **S10. Covariation of temperature and moisture.** Measured with the handheld soil probe (r ≈ −0.3) and the chamber probe (r ≈ −0.7). This is why the chamber probe could not separate the two drivers.

## Methods notes
- **Flux calculation.** Fluxes were recomputed from the raw 1 Hz concentration records with goFlux 0.4.0 (best.flux, Hüppi criteria) through fluxqc 0.2.3.
  - CO2 and CH4 come from the smart chamber's LI-7810 record; N2O comes from the LI-7820.
  - The window runs from the chamber's 25 s deadband to chamber opening.
  - The LI-7820 clock was aligned to the chamber per closure, by cross-correlating the H2O traces of the two instruments (offsets −3 to 105 s; details in goflux_clock_offsets_by_closure.csv).
  - The minimum detectable flux is 1.96·σ/t, with σ the MAD of first differences per instrument and day. Values below it are retained and flagged: 52% of N2O and 0.4% of CH4 closures.
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
