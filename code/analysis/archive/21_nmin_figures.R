# 21_nmin_figures.R
# N mineralization summary figure and treatment-means table
# Inputs:  data/processed/nmin_plot.csv
# Outputs: output/figures/summary_nmin.png
#          output/tables/nmin_treatment_means.csv

library(dplyr)
library(tidyr)
library(ggplot2)

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

treat_colors <- c("control" = "#377eb8", "compost" = "#4daf4a", "slurry" = "#e41a1c")
treat_shapes <- c("control" = 16, "compost" = 17, "slurry" = 15)
round_labels <- c("1" = "Round 1 (May 29)", "2" = "Round 2 (Jul 21)", "3" = "Round 3 (Oct)")

nmin <- read.csv("data/processed/nmin_plot.csv") %>%
  mutate(treatment = factor(treatment, levels = c("control", "compost", "slurry")))

metrics <- c(
  initial_no3_ug_g = "Initial NO3-N (ug/g)",
  initial_nh4_ug_g = "Initial NH4-N (ug/g)",
  net_min_ug_g     = "Net N mineralization, 28 d (ug N/g)",
  net_nitr_ug_g    = "Net nitrification, 28 d (ug N/g)"
)

long <- nmin %>%
  select(round, plot, treatment, all_of(names(metrics))) %>%
  pivot_longer(all_of(names(metrics)), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metrics[metric], levels = metrics),
         round = factor(round, levels = 1:3, labels = round_labels))

summ <- long %>%
  group_by(metric, round, treatment) %>%
  summarize(mean = mean(value), se = sd(value) / sqrt(n()), n = n(), .groups = "drop")

# Treatment test per metric x round (one-way ANOVA on plot means, n = 5)
tests <- long %>%
  group_by(metric, round) %>%
  summarize(p = summary(aov(value ~ treatment))[[1]][1, "Pr(>F)"], .groups = "drop") %>%
  mutate(label = sprintf("ANOVA p = %.2f", p))

pd <- position_dodge(width = 0.6)
p <- ggplot(long, aes(x = treatment, y = value, color = treatment, shape = treatment)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 1),
             alpha = 0.45, size = 1.8) +
  geom_errorbar(data = summ, aes(y = mean, ymin = mean - se, ymax = mean + se),
                width = 0.15, linewidth = 0.6) +
  geom_point(data = summ, aes(y = mean), size = 3.2) +
  geom_text(data = tests, aes(x = 2, y = Inf, label = label), inherit.aes = FALSE,
            vjust = 1.4, size = 3, color = "grey35") +
  facet_grid(metric ~ round, scales = "free_y", switch = "y",
             labeller = labeller(metric = label_wrap_gen(22))) +
  scale_color_manual(values = treat_colors) +
  scale_shape_manual(values = treat_shapes) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(x = NULL, y = NULL, color = "Treatment", shape = "Treatment",
       title = "28-day soil N mineralization",
       subtitle = "Small points = plot means (n = 5 plots per treatment); large points = treatment mean +/- SE") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey85", fill = NA))

ggsave("output/figures/summary_nmin.png", p, width = 10, height = 10, dpi = 150)
cat("Saved output/figures/summary_nmin.png\n")

table_out <- summ %>%
  left_join(tests %>% select(metric, round, p), by = c("metric", "round")) %>%
  mutate(across(c(mean, se), ~ round(.x, 2)), p = round(p, 3)) %>%
  arrange(metric, round, treatment)
write.csv(table_out, "output/tables/nmin_treatment_means.csv", row.names = FALSE)
cat("Wrote output/tables/nmin_treatment_means.csv\n")
