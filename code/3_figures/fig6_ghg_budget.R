# fig6_ghg_budget.R
# Fig 6. Non-CO2 GHG budget (GWP100): (a) season budget per treatment (component means;
# net per plot and treatment mean +/- 95% CI); (b) N2O emission factor with the IPCC 2019
# EF1 reference values; (c) season totals for scale on a signed, symmetric log axis
# (+ to the atmosphere, - from the atmosphere or into soil), grouped so nothing is double
# counted; the shaded row (manure effect) is already contained in the CH4 and N2O rows.
# Input: output/tables/ghg_co2eq_by_plot.csv, n2o_ef_by_plot.csv, n2o_ef_ci.csv,
#        ghg_context_by_plot.csv, ghg_context_means.csv (05_ghg_budget.R)

source("code/lib/setup.R")

wide <- read.csv("output/tables/ghg_co2eq_by_plot.csv") %>% mutate(treatment = as_trt(treatment))
net_s <- trt_summary(wide, net)
comp_lab <- expression(ch4 = CH[4]*","~season, n2o_rest = N[2]*O*","~rest~of~season,
                       n2o_wk = N[2]*O*","~days~1*"–"*6)
comp_fill <- c(ch4 = "grey70", n2o_rest = "#D9C7A8", n2o_wk = "#7A5C2E")
stack <- wide %>% group_by(treatment) %>% summarize(across(c(ch4, n2o_wk, n2o_rest), mean), .groups = "drop") %>%
  pivot_longer(-treatment, names_to = "comp", values_to = "val") %>%
  mutate(comp = factor(comp, levels = c("ch4", "n2o_rest", "n2o_wk")))
net_s <- trt_summary(wide, net)
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
  labs(x = NULL, y = expression(g~CO[2]*"-eq"~m^{-2})) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(plot.title.position = "plot", legend.position = "inside", legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1), legend.key.size = unit(6, "pt"))

ef_plot <- read.csv("output/tables/n2o_ef_by_plot.csv") %>%
  mutate(period = factor(period, levels = c("Days 1–6", "Season")), treatment = as_trt(treatment))
ef_ci <- read.csv("output/tables/n2o_ef_ci.csv") %>%
  mutate(period = factor(period, levels = c("Days 1–6", "Season")), treatment = as_trt(treatment))
pc <- ggplot() + scale_x_discrete() +
  geom_hline(yintercept = 0, colour = MUTED, linewidth = 0.3) +
  geom_hline(yintercept = c(1, 0.6), colour = INK, linewidth = 0.3, linetype = c("22", "12")) +
  geom_point(data = ef_plot, aes(period, ef, colour = treatment), shape = 16, size = 1, alpha = 0.45,
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6, seed = 1), show.legend = FALSE) +
  geom_linerange(data = ef_ci, aes(period, ymin = lo, ymax = hi, colour = treatment),
                 position = position_dodge(0.6), linewidth = 0.4, show.legend = FALSE) +
  geom_point(data = ef_ci, aes(period, mean, colour = treatment, shape = treatment, fill = treatment),
             position = position_dodge(0.6), size = 2, stroke = 0.4) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 0.1, base = 10),
                     breaks = c(-1, 0, 0.1, 1, 5), labels = c("\u22121", "0", "0.1", "1", "5")) +
  annotate("text", x = 2.62, y = c(1, 0.6), label = c("IPCC 1%", "0.6% (organic N, wet)"),
           vjust = c(-0.3, 1.3), hjust = 0, size = 2.2, colour = INK) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = expression(N[2]*O*"-N"~("%"~of~applied~N))) +
  theme(legend.position = "bottom", plot.margin = margin(5.5, 62, 5.5, 5.5)) +
  theme(plot.title.position = "plot")

pts <- read.csv("output/tables/ghg_context_by_plot.csv") %>% mutate(treatment = as_trt(treatment))
mns <- read.csv("output/tables/ghg_context_means.csv") %>% mutate(treatment = as_trt(treatment))
grp_of <- c(rs = "CO2", anpp = "CO2", amend = "Manure C", n2o = "CH4, N2O", ch4 = "CH4, N2O", att = "Effect")
grp_lvl <- c("CO2", "Manure C", "CH4, N2O", "Effect")
grp_labs <- c("CO2" = "CO[2]", "Manure C" = "Manure~C", "CH4, N2O" = "CH[4]*','~N[2]*O", "Effect" = "Manure~effect")
row_lvl <- rev(c("rs", "anpp", "amend", "n2o", "ch4", "att"))
row_labs <- expression(att = Manure~effect~on~CH[4]+N[2]*O, ch4 = CH[4], n2o = N[2]*O,
                       amend = "Manure C added", anpp = "Aboveground production", rs = "Soil respiration")
grp_fix <- function(d) d %>% mutate(grp = factor(grp_of[row], levels = grp_lvl), row = factor(row, levels = row_lvl))
pts <- grp_fix(pts); mns <- grp_fix(mns)
shade <- tibble(grp = factor("Effect", levels = grp_lvl))
pdd <- position_dodge(width = 0.6)
pd_ <- ggplot() +
  geom_rect(data = shade, aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "grey93") +
  geom_vline(xintercept = 0, colour = INK, linewidth = 0.3) +
  geom_point(data = pts, aes(val, row, colour = treatment), shape = 16, size = 0.8, alpha = 0.35,
             position = position_jitterdodge(jitter.width = 0, jitter.height = 0.08, dodge.width = 0.6, seed = 1)) +
  geom_point(data = mns, aes(val, row, colour = treatment, shape = treatment, fill = treatment),
             position = pdd, size = 1.8, stroke = 0.4) +
  facet_grid(grp ~ ., scales = "free_y", space = "free_y", labeller = labeller(grp = as_labeller(grp_labs, label_parsed))) +
  scale_y_discrete(labels = row_labs) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 1, base = 10),
                     breaks = c(-1000, -100, -10, 0, 10, 100, 1000, 10000),
                     labels = c("\u22121k", "\u2212100", "\u221210", "0", "10", "100", "1k", "10k"), limits = c(-2000, 12000)) +
  scale_colour_trt(guide = "none") + scale_fill_trt(guide = "none") + scale_shape_trt(guide = "none") +
  labs(x = expression(Season~total~(g~CO[2]~or~CO[2]*"-eq"~m^{-2}*","~symmetric~log~scale)), y = NULL) +
  geom_text(data = tibble(grp = factor("CO2", levels = grp_lvl), x = c(-4, 4), hj = c(1, 0),
                          lab = c("\u2190 from atmosphere or into soil", "to atmosphere \u2192")),
            aes(x = x, y = Inf, label = lab, hjust = hj), vjust = 1.4, size = 2.2, colour = MUTED) +
  theme(plot.title.position = "plot", panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.25),
        panel.grid.major.y = element_blank(), strip.background = element_blank(),
        strip.text.y = element_blank(), panel.spacing.y = unit(3, "pt"))
fig7 <- ((free(pa) | pc) + plot_layout(widths = c(1, 1))) / pd_ + plot_layout(heights = c(1.15, 1)) + tags_pub()
save_fig(fig7, "fig6_ghg_budget", 180, 140)

