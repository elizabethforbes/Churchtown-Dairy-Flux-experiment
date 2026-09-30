# fig5_soil_chemistry_plants.R
# Fig 5. (a) Dairy One soil tests at each sampling and (c) forage composition at the
# October harvest, as Hedges' g (amendment - control; 95% CI; filled = Welch p < 0.05,
# uncorrected); (b) aboveground biomass (plot values, mean and 95% CI).
# Input: output/tables/effect_synthesis.csv (08_effect_synthesis.R); data/clean/biomass.csv

source("code/lib/setup.R")

syn <- read.csv("output/tables/effect_synthesis.csv") %>%
  mutate(sampling = factor(sampling, levels = c("29 May", "21 Jul", "14 Oct", "single")),
         treatment = as_trt(treatment), sig = welch_p < 0.05)
GLIM <- 4.5   # clip CIs for display
panel_plot <- function(pn) {
  d <- syn %>% filter(panel == pn)
  lv <- d %>% distinct(label, label_order) %>% arrange(label_order) %>% pull(label)
  d <- d %>% mutate(label = factor(label, levels = rev(lv)),
                    lo_c = pmax(g_lo, -GLIM), hi_c = pmin(g_hi, GLIM), g_c = pmax(pmin(hedges_g, GLIM), -GLIM))
  multi <- any(d$sampling != "single")
  pd <- position_dodge(width = if (multi) 0.7 else 0)
  ggplot(d, aes(y = label, group = sampling)) +
    annotate("rect", xmin = -0.8, xmax = 0.8, ymin = -Inf, ymax = Inf, fill = "grey95") +
    geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.3) +
    geom_linerange(aes(xmin = lo_c, xmax = hi_c, colour = treatment), position = pd, linewidth = 0.35) +
    geom_point(aes(x = g_c, colour = treatment, shape = treatment,
                   fill = ifelse(sig, as.character(treatment), "white"),
                   alpha = sampling), position = pd, size = 1.4, stroke = 0.4) +
    facet_grid(~ treatment, labeller = labeller(treatment = TRT_LABELS)) +
    scale_colour_trt(guide = "none") + scale_shape_trt(guide = "none") +
    scale_fill_manual(values = c(TRT_COLS, white = "white"), guide = "none") +
    scale_alpha_manual(values = c("29 May" = 0.45, "21 Jul" = 0.7, "14 Oct" = 1, single = 1), guide = "none") +
    scale_x_continuous(limits = c(-GLIM, GLIM), breaks = c(-4, -2, 0, 2, 4), oob = scales::squish) +
    labs(x = "Hedges' g (amendment − control)", y = NULL) +
    theme(panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.2),
          plot.title.position = "plot",
          panel.spacing.x = unit(6, "pt"), strip.text = element_text(hjust = 0.5),
          axis.text.y = element_text(size = rel(0.85)))
}
pa <- panel_plot("Soil tests")
pc <- panel_plot("Plant")
biomass <- clean_csv("biomass.csv") %>% group_by(plot, treatment) %>%
  summarize(biomass = mean(dry_matter_g_m2), .groups = "drop") %>% mutate(treatment = as_trt(treatment))
pb <- ggplot() + dot_ci_layers(biomass %>% mutate(x = treatment), trt_summary(biomass, biomass) %>% mutate(x = treatment),
                               x, biomass, pt_size = 1.3) +
  trt_axis() + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = expression(Aboveground~biomass~(g~m^{-2}))) +
  theme(plot.title.position = "plot", legend.position = "bottom")
fig5 <- (((pa / pb) + plot_layout(heights = c(1.35, 1))) | pc) + plot_layout(widths = c(1, 1), guides = "collect") +
  tags_pub() & theme(legend.position = "bottom")
save_fig(fig5, "fig5_soil_chemistry_plants", 180, 140)
