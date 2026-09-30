# figS5_nmin_pools.R
# Fig S5. (a) Extractable NH4+ and NO3- at the start and end of each incubation and
# (b) net nitrification (plot values; treatment means with 95% CI).
# Input: data/clean/soil_nmin.csv; output/tables/soil_metrics_by_plot.csv

source("code/lib/setup.R")
sampled_fac <- function(r) factor(paste0("Sampled ", ROUND_LABELS[as.character(r)]),
                                  levels = paste0("Sampled ", ROUND_LABELS))
nmin <- clean_csv("soil_nmin.csv") %>% mutate(treatment = as_trt(treatment))
pools <- nmin %>%
  select(plot, treatment, round, initial_nh4_ug_g, initial_no3_ug_g, incubated_nh4_ug_g, incubated_no3_ug_g) %>%
  pivot_longer(-c(plot, treatment, round), names_to = c("stage", "form"),
               names_pattern = "(initial|incubated)_(nh4|no3)_ug_g") %>%
  mutate(stage = factor(stage, levels = c("initial", "incubated"), labels = c("Day 0", "Day 28")),
         form = factor(form, levels = c("nh4", "no3"),
                       labels = c("NH[4]^'+'*'-N'~(mu*g~N~g^{-1})", "NO[3]^'-'*'-N'~(mu*g~N~g^{-1})")),
         round_lab = sampled_fac(round))
figs6 <- ggplot() + zero_line() +
  dot_ci_layers(pools, trt_summary(pools, value, form, round_lab, stage), stage, value) +
  facet_grid(form ~ round_lab, scales = "free_y", switch = "y", labeller = labeller(form = label_parsed)) +
  labs(x = NULL, y = NULL) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "plain", hjust = 0.5))
sm <- read.csv("output/tables/soil_metrics_by_plot.csv") %>%
  mutate(treatment = as_trt(treatment), round_lab = factor(round_lab, levels = ROUND_LABELS))
s6b <- ggplot() + zero_line() +
  dot_ci_layers(sm, trt_summary(sm, net_nitr_rate_ug_g_d, round_lab), round_lab, net_nitr_rate_ug_g_d) +
  facet_wrap(~ "Net nitrification") +
  labs(x = NULL, y = expression(mu*g~N~g^{-1}~d^{-1}))
figs6 <- (figs6 | free(s6b, type = "panel", side = "b")) + plot_layout(widths = c(3, 1.1), guides = "collect") + tags_pub() &
  theme(legend.position = "bottom")
save_fig(figs6, "figS5_nmin_pools", 180, 95, "si")

