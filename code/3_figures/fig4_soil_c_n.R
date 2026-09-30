# fig4_soil_c_n.R
# Fig 4. Soil C and N cycling at each sampling: (a) substrate-induced respiration,
# (b) C mineralization, (c) net N mineralization, (d) KCl-extractable mineral N at the
# start of the incubation. Plot values, treatment means with 95% CI; * one-way ANOVA
# p < 0.05 at that sampling.
# Input: output/tables/soil_metrics_by_plot.csv

source("code/lib/setup.R")
ev <- function(x) eval(parse(text = x))

soil <- read.csv("output/tables/soil_metrics_by_plot.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(round_lab, levels = ROUND_LABELS))
soil_metrics <- tribble(
  ~col,                    ~ylab,
  "sir_ug_co2c_hr_g",      "expression(atop(SIR, (mu*g~CO[2]*'-C'~g^{-1}~h^{-1})))",
  "cmin_rate_ug_co2c_g_d", "expression(atop(C~mineralization, (mu*g~CO[2]*'-C'~g^{-1}~d^{-1})))",
  "net_min_rate_ug_g_d",   "expression(atop(Net~N~mineralization, (mu*g~N~g^{-1}~d^{-1})))",
  "tin_ug_g",              "expression(atop(Extractable~mineral~N, (mu*g~N~g^{-1})))"
)
panels <- lapply(seq_len(nrow(soil_metrics)), function(i) {
  m <- soil_metrics[i, ]; v <- sym(m$col)
  s <- trt_summary(soil, !!v, round_lab); pv <- anova_p(soil, !!v, round_lab)
  ggplot() + { if (grepl("net_", m$col)) zero_line() } +
    dot_ci_layers(soil %>% filter(!is.na(!!v)), s, round_lab, !!v) +
    geom_text(data = pv, aes(round_lab, Inf, label = ifelse(p < 0.05, "*", "")),
              vjust = 1.1, size = 3.2, colour = INK) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.14))) +
    labs(x = NULL, y = ev(m$ylab))
})
fig4 <- wrap_plots(panels, ncol = 2, byrow = FALSE) +
  plot_layout(guides = "collect") + tags_pub() & theme(legend.position = "bottom")
save_fig(fig4, "fig4_soil_c_n", 125, 115)
