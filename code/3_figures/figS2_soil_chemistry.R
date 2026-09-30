# figS2_soil_chemistry.R
# Fig S2. Soil pH, organic matter, CEC, base saturation and Mehlich-3 P, K, Ca and Mg for
# all plots at each sampling (plot values; treatment means with 95% CI; * ANOVA p < 0.05).
# Input: data/clean/soil_chemistry.csv

source("code/lib/setup.R")
ev <- function(x) eval(parse(text = x))
d1 <- clean_csv("soil_chemistry.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(ROUND_LABELS[timepoint], levels = ROUND_LABELS))
d1_vars <- tribble(
  ~col,                  ~title,                 ~ylab,
  "ph",                  "pH (Dairy One)",       "'pH'",
  "om_pct",              "Organic matter",       "'Organic matter (%, LOI)'",
  "cec_meq100g",         "CEC",                  "expression(CEC~(meq~100~g^{-1}))",
  "base_sat_total_pct",  "Base saturation",      "'Base saturation (%)'",
  "p_ppm",               "Phosphorus",           "'Mehlich-3 P (ppm)'",
  "k_ppm",               "Potassium",            "'Mehlich-3 K (ppm)'",
  "ca_ppm",              "Calcium",              "'Mehlich-3 Ca (ppm)'",
  "mg_ppm",              "Magnesium",            "'Mehlich-3 Mg (ppm)'"
)
s9 <- lapply(seq_len(nrow(d1_vars)), function(i) {
  m <- d1_vars[i, ]; v <- sym(m$col)
  pv <- anova_p(d1, !!v, round_lab)
  ggplot() + dot_ci_layers(d1, trt_summary(d1, !!v, round_lab), round_lab, !!v) +
    geom_text(data = pv, aes(round_lab, Inf, label = ifelse(p < 0.05, "*", "")), vjust = 1.1, size = 3.2) +
    scale_x_discrete(labels = function(x) sub(" ", "\n", x)) +
    labs(x = NULL, y = ev(m$ylab))
})
figs9 <- wrap_plots(s9, ncol = 4) + plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(figs9, "figS2_soil_chemistry", 180, 105, "si")

