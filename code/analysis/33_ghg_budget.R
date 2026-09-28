# 33_ghg_budget.R
# Main Fig 7: non-CO2 greenhouse-gas budget in CO2-equivalents (GWP100, IPCC AR6:
# CH4 non-fossil 27.0, N2O 273), from the plot-level totals of Figs 2-3.
#   (a) season budget per treatment (29 May-14 Oct): CH4 uptake, N2O in days 1-6,
#       N2O over the rest of the season; net per plot and treatment mean +/- 95% CI
#   (b) the part attributable to each amendment (amendment minus control mean,
#       Welch 95% CI), by component
#   (c) N2O emission factor: amendment-attributable N2O-N as % of applied N,
#       for days 1-6 and the season, against IPCC 2019 EF1 (1.0% aggregate;
#       0.6% for organic N inputs in wet climates)
# Soil CO2 is not included: chamber CO2 is soil respiration (roots + microbes),
# not a net ecosystem exchange, so it is not a GHG balance term.
# Input:  output/tables/ghg_totals_by_plot.csv (30_main_figures.R),
#         output/tables/application_inputs.csv
# Output: output/figures/main/fig7_ghg_budget.{pdf,png}; output/tables/ghg_co2eq_budget.csv

source("code/analysis/fig_setup.R")

GWP_CH4 <- 27.0; GWP_N2O <- 273
tot <- read.csv("output/tables/ghg_totals_by_plot.csv")
n_app <- read.csv("output/tables/application_inputs.csv") %>% transmute(treatment, n_mg_m2 = n_g_m2 * 1000)

ch4_eq <- function(mgC) mgC * 16 / 12 * GWP_CH4 / 1000     # g CO2-eq m-2
n2o_eq <- function(mgN) mgN * 44 / 28 * GWP_N2O / 1000
wide <- tot %>% select(plot, treatment, period, CH4_C_mg_m2, N2O_N_mg_m2) %>%
  pivot_wider(names_from = period, values_from = c(CH4_C_mg_m2, N2O_N_mg_m2)) %>%
  transmute(plot, treatment = as_trt(treatment),
            ch4 = ch4_eq(CH4_C_mg_m2_season),
            n2o_wk = n2o_eq(N2O_N_mg_m2_first_week),
            n2o_rest = n2o_eq(N2O_N_mg_m2_season - N2O_N_mg_m2_first_week),
            n2o = n2o_wk + n2o_rest, net = ch4 + n2o,
            n2o_n_wk = N2O_N_mg_m2_first_week, n2o_n_season = N2O_N_mg_m2_season)

# --- (a) season budget -----------------------------------------------------------
comp_lab <- c(ch4 = "CH₄ (season)", n2o_rest = "N₂O, rest of season", n2o_wk = "N₂O, days 1–6")
comp_fill <- c(ch4 = "grey70", n2o_rest = "#D9C7A8", n2o_wk = "#7A5C2E")
stack <- wide %>% group_by(treatment) %>% summarize(across(c(ch4, n2o_wk, n2o_rest), mean), .groups = "drop") %>%
  pivot_longer(-treatment, names_to = "comp", values_to = "val") %>%
  mutate(comp = factor(comp, levels = c("ch4", "n2o_rest", "n2o_wk")))
net_s <- trt_summary(wide, net)
p_net <- anova_p(wide, net)$p
pa <- ggplot() +
  geom_col(data = stack, aes(treatment, val, fill = comp), width = 0.6, colour = NA) +
  geom_hline(yintercept = 0, colour = INK, linewidth = 0.3) +
  geom_point(data = wide, aes(treatment, net, colour = treatment), shape = 16, size = 1.1, alpha = 0.55,
             position = position_nudge(x = 0.42), show.legend = FALSE) +
  geom_linerange(data = net_s, aes(treatment, ymin = lo, ymax = hi), colour = INK, linewidth = 0.4,
                 position = position_nudge(x = 0.42)) +
  geom_point(data = net_s, aes(treatment, mean, shape = treatment, fill = treatment), colour = INK,
             size = 2, stroke = 0.4, position = position_nudge(x = 0.42), show.legend = FALSE) +
  scale_fill_manual(values = c(comp_fill, TRT_COLS), breaks = names(comp_fill)[c(3, 2, 1)],
                    labels = comp_lab[c(3, 2, 1)], name = NULL) +
  scale_colour_trt(guide = "none") + scale_shape_trt(guide = "none") +
  scale_x_discrete(labels = TRT_LABELS) +
  labs(x = NULL, y = expression(g~CO[2]*"-eq"~m^{-2}), title = "Season non-CO₂ budget",
       subtitle = sprintf("Net: ANOVA p = %.2f", p_net)) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(plot.title.position = "plot", legend.position = "inside", legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1), legend.key.size = unit(6, "pt"))

# --- (b) attributable to the amendment -------------------------------------------
att_vars <- c(ch4 = "CH₄, season", n2o_wk = "N₂O, days 1–6", n2o = "N₂O, season", net = "Net, season")
att <- bind_rows(lapply(names(att_vars), function(v) diff_vs_control(wide, !!sym(v)) %>% mutate(comp = v))) %>%
  mutate(comp = factor(comp, levels = rev(names(att_vars))))
pdg <- position_dodge(width = 0.55)
pb <- ggplot(att, aes(y = comp, x = diff, colour = treatment)) +
  geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.3) +
  geom_linerange(aes(xmin = lo, xmax = hi), position = pdg, linewidth = 0.4) +
  geom_point(aes(shape = treatment, fill = treatment), position = pdg, size = 1.8, stroke = 0.4) +
  scale_colour_trt(guide = "none") + scale_fill_trt(guide = "none") + scale_shape_trt(guide = "none") +
  scale_y_discrete(labels = att_vars) +
  labs(y = NULL, x = expression(Amendment~minus~control~(g~CO[2]*"-eq"~m^{-2})),
       title = "Attributable to the amendment") +
  theme(plot.title.position = "plot", panel.grid.major.y = element_line(colour = "grey94", linewidth = 0.2))

# --- (c) N2O emission factor ------------------------------------------------------
ctl <- wide %>% filter(treatment == "control") %>% summarize(wk = mean(n2o_n_wk), season = mean(n2o_n_season))
ef_plot <- wide %>% filter(treatment != "control") %>% left_join(n_app, by = "treatment") %>%
  transmute(plot, treatment, `Days 1–6` = 100 * (n2o_n_wk - ctl$wk) / n_mg_m2,
            Season = 100 * (n2o_n_season - ctl$season) / n_mg_m2) %>%
  pivot_longer(-c(plot, treatment), names_to = "period", values_to = "ef") %>%
  mutate(period = factor(period, levels = c("Days 1–6", "Season")), treatment = as_trt(treatment))
ef_ci <- bind_rows(lapply(c("n2o_n_wk", "n2o_n_season"), function(v) diff_vs_control(wide, !!sym(v)) %>%
  mutate(period = ifelse(v == "n2o_n_wk", "Days 1–6", "Season")))) %>%
  left_join(n_app %>% mutate(treatment = as_trt(treatment)), by = "treatment") %>%
  mutate(mean = 100 * diff / n_mg_m2, lo = 100 * lo / n_mg_m2, hi = 100 * hi / n_mg_m2,
         period = factor(period, levels = c("Days 1–6", "Season")))
pc <- ggplot() + scale_x_discrete() +
  geom_hline(yintercept = 0, colour = MUTED, linewidth = 0.3) +
  geom_hline(yintercept = c(1, 0.6), colour = INK, linewidth = 0.3, linetype = c("22", "12")) +
  annotate("text", x = 0.45, y = c(1, 0.6), label = c("IPCC 1%", "0.6%"),
           hjust = 0, vjust = -0.4, size = 1.9, colour = INK) +
  geom_point(data = ef_plot, aes(period, ef, colour = treatment), shape = 16, size = 1, alpha = 0.45,
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6, seed = 1), show.legend = FALSE) +
  geom_linerange(data = ef_ci, aes(period, ymin = lo, ymax = hi, colour = treatment),
                 position = position_dodge(0.6), linewidth = 0.4, show.legend = FALSE) +
  geom_point(data = ef_ci, aes(period, mean, colour = treatment, shape = treatment, fill = treatment),
             position = position_dodge(0.6), size = 2, stroke = 0.4) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  labs(x = NULL, y = "% of applied N as N₂O-N", title = "N₂O emission factor") +
  theme(legend.position = "bottom") +
  theme(plot.title.position = "plot")

fig7 <- ((pa | pc) / pb) + plot_layout(heights = c(1.3, 1)) + tags_pub()

save_fig(fig7, "fig7_ghg_budget", 180, 130)

# --- table -------------------------------------------------------------------------
bud <- wide %>% group_by(treatment) %>%
  summarize(across(c(ch4, n2o_wk, n2o_rest, n2o, net), list(mean = mean, se = ~ sd(.x) / sqrt(n()))), .groups = "drop") %>%
  left_join(ef_ci %>% select(treatment, period, mean, lo, hi) %>%
              pivot_wider(names_from = period, values_from = c(mean, lo, hi), names_glue = "EF_{period}_{.value}"),
            by = "treatment") %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)))
names(bud) <- gsub("–", "-", gsub(" ", "_", names(bud)))
write.csv(bud, "output/tables/ghg_co2eq_budget.csv", row.names = FALSE)
cat(sprintf("  wrote ghg_co2eq_budget.csv; net season CO2-eq ANOVA p = %.2f\n", p_net))
print(as.data.frame(bud))
print(as.data.frame(att %>% select(comp, treatment, diff, lo, hi, p) %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))))
