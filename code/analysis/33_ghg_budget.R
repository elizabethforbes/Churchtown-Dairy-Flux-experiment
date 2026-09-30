# 33_ghg_budget.R
# Main Fig 6: non-CO2 greenhouse-gas budget in CO2-equivalents (GWP100, IPCC AR6:
# CH4 non-fossil 27.0, N2O 273), from the plot-level totals of Figs 2-3.
#   (a) season budget per treatment (29 May-14 Oct): CH4 uptake, N2O in days 1-6,
#       N2O over the rest of the season; net per plot and treatment mean +/- 95% CI
#   (b) N2O emission factor: amendment-attributable N2O-N as % of applied N,
#       for days 1-6 and the season, against IPCC 2019 EF1 (1.0% aggregate;
#       0.6% for organic N inputs in wet climates)
#   (c) context: the non-CO2 budget against soil respiration, aboveground NPP and
#       the (assumed) amendment C input, all in CO2 units
# The attributable (amendment minus control) components are computed below for the
# tables and the context panel but are no longer drawn as a separate panel.
# Soil CO2 is not included in the budget: chamber CO2 is soil respiration (roots + microbes),
# not a net ecosystem exchange, so it is not a GHG balance term.
# Input:  output/tables/ghg_totals_by_plot.csv (30_main_figures.R),
#         output/tables/application_inputs.csv
# Output: output/figures/main/fig6_ghg_budget.{pdf,png}; output/tables/ghg_co2eq_budget.csv,
#         ghg_co2eq_metrics.csv

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
comp_lab <- expression(ch4 = CH[4]*","~season, n2o_rest = N[2]*O*","~rest~of~season,
                       n2o_wk = N[2]*O*","~days~1*"–"*6)
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
  labs(x = NULL, y = expression(g~CO[2]*"-eq"~m^{-2})) +
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
  geom_point(data = ef_plot, aes(period, ef, colour = treatment), shape = 16, size = 1, alpha = 0.45,
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6, seed = 1), show.legend = FALSE) +
  geom_linerange(data = ef_ci, aes(period, ymin = lo, ymax = hi, colour = treatment),
                 position = position_dodge(0.6), linewidth = 0.4, show.legend = FALSE) +
  geom_point(data = ef_ci, aes(period, mean, colour = treatment, shape = treatment, fill = treatment),
             position = position_dodge(0.6), size = 2, stroke = 0.4) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  labs(x = NULL, y = expression(N[2]*O*"-N"~("%"~of~applied~N))) +
  theme(legend.position = "bottom") +
  theme(plot.title.position = "plot")

# --- (d) context: non-CO2 budget against the system's CO2-C fluxes -----------------
# All in g CO2(-eq) m-2 over the same season (log axis). These are gross fluxes for
# scale, not terms of one net balance.
#   Soil respiration: clipped collars (roots + microbes), season trapezoid total of
#     midday closures (10:00-17:00), so likely biased high vs a 24-h integral.
#   ANPP: Oct harvest of a 0.5 m2 subplot left uncut since the pre-experiment mow;
#     C = 47% of dry mass (IPCC 2006 Vol. 4 Ch. 6 default for herbaceous biomass).
#   Amendment C: not measured. C = 50% of volatile solids, using the same assumed
#     VS shares of dry matter as 35_storage_vs_field.R (slurry 0.80, after ASAE D384.2
#     VS/TS = 0.85 for lactating dairy manure as excreted; compost 0.55), i.e.
#     40% (slurry) and 27.5% (compost) of dry matter.
VS_FRAC <- c(slurry = 0.80, compost = 0.55)
C_FRAC <- 0.5 * VS_FRAC
co2 <- function(gC) gC * 44 / 12
rs <- tot %>% filter(period == "season") %>% transmute(plot, treatment = as_trt(treatment), val = co2(CO2_C_g_m2))
anpp <- read.csv("data/processed/biomass.csv") %>% group_by(plot, treatment) %>%
  summarize(val = co2(mean(dry_matter_g_m2) * 0.47), .groups = "drop") %>% mutate(treatment = as_trt(treatment))
dm <- read.csv("output/tables/application_inputs.csv")
amend_c <- bind_rows(lapply(c("slurry", "compost"), function(tr) {
  d <- dm$dm_g_m2[dm$treatment == tr]
  tibble(treatment = as_trt(tr), val = co2(d * C_FRAC[[tr]]))
}))
att_net <- att %>% filter(comp == "net") %>% select(treatment, diff)
ctx_rows <- c(rs = "Soil respiration", anpp = "Aboveground production",
              amend = "Manure C added", net = "CH4 + N2O budget",
              att = "CH4 + N2O, manure effect", ch4 = "CH4 uptake")
ctx_labs <- expression(rs = "Soil respiration", anpp = "Aboveground production", amend = "Manure C added",
                       net = CH[4]+N[2]*O~budget, att = CH[4]+N[2]*O*","~manure~effect, ch4 = CH[4]~uptake)
lvl <- rev(names(ctx_rows))
pts <- bind_rows(rs %>% mutate(row = "rs"), anpp %>% mutate(row = "anpp"),
                 wide %>% transmute(plot, treatment, val = net, row = "net"),
                 wide %>% transmute(plot, treatment, val = -ch4, row = "ch4")) %>%
  mutate(row = factor(row, levels = lvl))
mns <- pts %>% group_by(row, treatment) %>% summarize(val = mean(val), .groups = "drop") %>%
  bind_rows(att_net %>% transmute(row = factor("att", levels = lvl), treatment, val = diff),
            amend_c %>% mutate(row = factor("amend", levels = lvl)))
pdd <- position_dodge(width = 0.6)
pd_ <- ggplot() +
  geom_point(data = pts, aes(val, row, colour = treatment), shape = 16, size = 0.8, alpha = 0.35,
             position = position_jitterdodge(jitter.width = 0, jitter.height = 0.08, dodge.width = 0.6, seed = 1)) +
  geom_point(data = mns, aes(val, row, colour = treatment, shape = treatment, fill = treatment),
             position = pdd, size = 1.8, stroke = 0.4) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000), labels = c("1", "10", "100", "1k", "10k"), limits = c(0.5, 12000)) +
  annotation_logticks(sides = "b", linewidth = 0.2, short = unit(1, "pt"), mid = unit(2, "pt"), long = unit(3, "pt")) +
  scale_y_discrete(labels = ctx_labs, drop = FALSE) +
  scale_colour_trt(guide = "none") + scale_fill_trt(guide = "none") + scale_shape_trt(guide = "none") +
  labs(x = expression(Season~total~(g~CO[2]~or~CO[2]*"-eq"~m^{-2}*","~log~scale)), y = NULL) +
  theme(plot.title.position = "plot", panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.25),
        panel.grid.major.y = element_blank())

fig7 <- ((free(pa) | pc) / pd_) + plot_layout(heights = c(1.15, 1)) + tags_pub()
save_fig(fig7, "fig6_ghg_budget", 180, 140)

# context numbers
ctx <- mns %>% mutate(val = signif(val, 3)) %>% pivot_wider(names_from = treatment, values_from = val)
brk <- att_net %>% left_join(amend_c, by = "treatment") %>% mutate(c_input_over_att = val / diff)
cat("  context (g CO2-eq m-2):\n"); print(as.data.frame(ctx))
cat("  amendment C input (as CO2) relative to attributable non-CO2:\n")
print(as.data.frame(brk %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))))
cat(sprintf("  non-CO2 net as %% of soil respiration: %s\n",
            paste(sprintf("%s %.2f", levels(wide$treatment),
                          100 * tapply(wide$net, wide$treatment, mean) / tapply(rs$val, rs$treatment, mean)), collapse = ", ")))
cat(sprintf("  non-CO2 net as %% of ANPP C uptake: %s\n",
            paste(sprintf("%s %.1f", levels(wide$treatment),
                          100 * tapply(wide$net, wide$treatment, mean) / tapply(anpp$val, anpp$treatment, mean)), collapse = ", ")))

# --- metric sensitivity (table) ----------------------------------------------------
# IPCC AR6 WG1 Table 7.15: CH4 non-fossil GWP20 79.7, GWP100 27.0, GTP100 4.7;
# N2O GWP20 273, GWP100 273, GTP100 233. GWP* (Smith et al. 2021) applies only to
# the CH4 term and depends on how emissions change over time: a new, sustained
# change in CH4 flux (e.g. applying slurry every year) is weighted 4.53 x GWP100
# for its first 20 years; a flux that has been constant for >20 years (the
# background soil CH4 sink) is weighted 0.28 x GWP100. N2O is treated like CO2
# (GWP* = GWP100).
metrics <- tribble(~metric, ~f_ch4_total, ~f_ch4_att, ~f_n2o,
                   "GWP100", 27.0, 27.0, 273,
                   "GWP20", 79.7, 79.7, 273,
                   "GTP100", 4.7, 4.7, 233,
                   "GWP* (sustained practice)", 0.28 * 27.0, 4.53 * 27.0, 273)
mass <- wide %>% transmute(plot, treatment, ch4_kg = ch4 / GWP_CH4, n2o_kg = n2o / GWP_N2O)  # g CH4, g N2O per m2
ctl_mass <- mass %>% filter(treatment == "control") %>% summarize(ch4 = mean(ch4_kg), n2o = mean(n2o_kg))
met_tab <- bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
  m <- metrics[i, ]
  mass %>% group_by(treatment) %>% summarize(ch4 = mean(ch4_kg), n2o = mean(n2o_kg), .groups = "drop") %>%
    transmute(metric = m$metric, treatment,
              ch4_co2eq = ch4 * m$f_ch4_total, n2o_co2eq = n2o * m$f_n2o, net_co2eq = ch4_co2eq + n2o_co2eq,
              attributable_co2eq = ifelse(treatment == "control", NA,
                                          (ch4 - ctl_mass$ch4) * m$f_ch4_att + (n2o - ctl_mass$n2o) * m$f_n2o))
})) %>% mutate(across(where(is.numeric), ~ signif(.x, 3)))
write.csv(met_tab, "output/tables/ghg_co2eq_metrics.csv", row.names = FALSE)
cat("  wrote ghg_co2eq_metrics.csv\n"); print(as.data.frame(met_tab))

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
