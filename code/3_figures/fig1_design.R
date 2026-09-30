# fig1_design.R
# Fig 1. Study design: (a) plot map from the RTK-GPS survey, (b) timeline of flux
# campaigns, soil samplings and harvest, (c) dry matter and N applied per hectare.
# Input: data/clean/plots.csv, plot_corners.csv, collars.csv, ghg_fluxes.csv, biomass.csv,
#        amendment_application.csv

source("code/lib/setup.R")

# plot map from RTK-GPS corners and collars (UTM 18N), relative to the south-west corner
key <- clean_csv("plots.csv")
corners <- clean_csv("plot_corners.csv") %>% transmute(plot, E = easting_m, N = northing_m, z = elevation_m)
collars <- clean_csv("collars.csv") %>% transmute(plot, E = easting_m, N = northing_m)
E0 <- min(corners$E); N0 <- min(corners$N); zmin <- min(corners$z)
poly <- corners %>% group_by(plot) %>% mutate(ang = atan2(N - mean(N), E - mean(E))) %>% arrange(plot, ang) %>% ungroup() %>%
  left_join(key, by = "plot") %>% mutate(x = E - E0, y = N - N0, treatment = as_trt(treatment))
cent <- poly %>% group_by(plot, treatment) %>% summarize(x = mean(x), y = mean(y), dz = mean(z) - zmin, .groups = "drop")
s1map <- ggplot() +
  geom_polygon(data = poly, aes(x, y, group = plot, fill = treatment), colour = INK, linewidth = 0.25, alpha = 0.35) +
  geom_point(data = collars %>% mutate(x = E - E0, y = N - N0), aes(x, y), shape = 16, size = 0.5, colour = INK) +
  geom_text(data = cent, aes(x - 2.4, y + 0.6, label = plot), size = 2.3, fontface = "bold", colour = INK, hjust = 1) +
  geom_text(data = cent, aes(x - 2.4, y - 0.7, label = sprintf("%.2f", dz)), size = 1.7, colour = MUTED, hjust = 1) +
  annotate("segment", x = max(poly$x) + 2, xend = max(poly$x) + 2, y = 0, yend = 4,
           arrow = arrow(length = unit(3, "pt")), linewidth = 0.3, colour = INK) +
  annotate("text", x = max(poly$x) + 2, y = 5, label = "N", size = 2.2, colour = INK) +
  annotate("segment", x = 0, xend = 5, y = -2, yend = -2, linewidth = 0.5, colour = INK) +
  annotate("text", x = 2.5, y = -3.2, label = "5 m", size = 2, colour = INK) +
  coord_equal(clip = "off") + scale_fill_trt() +
  labs(x = NULL, y = NULL) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid.major.y = element_blank())

# timeline of manure application, flux campaigns, soil sampling and harvest
flux_dates <- sort(unique(as.Date(clean_csv("ghg_fluxes.csv")$date)))
harvest <- unique(as.Date(clean_csv("biomass.csv")$sampling_date))
rows <- c("Manure applied", "GHG flux", "Soil sampling", "Biomass harvest")
ev_points <- bind_rows(
  tibble(row = "Manure applied", date = APPLICATION_DATE, kind = "event"),
  tibble(row = "GHG flux", date = flux_dates) %>% mutate(kind = if_else(date < APPLICATION_DATE, "pre", "post")),
  tibble(row = "Soil sampling", date = as.Date(ROUND_DATES), kind = "event"),
  tibble(row = "Biomass harvest", date = harvest, kind = "event"))
s1time <- ggplot(ev_points %>% filter(row != "Manure applied"), aes(date, row)) +
  geom_vline(xintercept = APPLICATION_DATE, colour = MUTED, linewidth = 0.35, linetype = "22") +
  annotate("text", x = APPLICATION_DATE + 3, y = Inf, label = "Manure applied, 28 May", hjust = 0, vjust = 1.2,
           size = 2.2, colour = INK) +
  geom_point(size = 1.5, colour = INK) +
  scale_y_discrete(limits = rev(setdiff(rows, "Manure applied"))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = as.Date(c("2025-05-01", "2025-10-31")), expand = expansion(0)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank())

# (c) what went on each plot: dry matter and N (organic filled, NH4+ open), per ha
ai <- clean_csv("amendment_application.csv")
inp <- bind_rows(
  ai %>% transmute(treatment, panel = "dm", seg = "Organic", ymin = 0, ymax = dm_Mg_ha),
  ai %>% transmute(treatment, panel = "n", seg = "Organic", ymin = 0, ymax = org_g_m2 * 10),
  ai %>% transmute(treatment, panel = "n", seg = "Ammonium", ymin = org_g_m2 * 10, ymax = (org_g_m2 + nh4_g_m2) * 10)) %>%
  mutate(x = match(treatment, c("compost", "slurry")),
         panel = factor(panel, levels = c("dm", "n"), labels = c("Dry~matter~(Mg~ha^{-1})", "Total~N~(kg~ha^{-1})")))
key <- tibble(form = factor(c("Organic N", "NH4"), levels = c("Organic N", "NH4")), x = 1, y = 0,
              panel = factor("Total~N~(kg~ha^{-1})", levels = levels(inp$panel)))
s1n <- ggplot(inp) +
  geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = ymin, ymax = ymax, colour = treatment,
                fill = ifelse(seg == "Organic", treatment, "white")), linewidth = 0.35) +
  geom_point(data = key, aes(x, y, shape = form), size = 0, colour = NA) +
  scale_fill_manual(values = c(TRT_COLS, white = "white"), guide = "none") +
  scale_colour_manual(values = TRT_COLS, guide = "none") +
  scale_shape_manual(values = c(22, 22), name = NULL, labels = c("Organic N", expression(NH[4]^'+'*'-N')),
                     guide = guide_legend(override.aes = list(size = 3.2, colour = INK, fill = c("grey45", "white"), stroke = 0.35))) +
  facet_wrap(~ panel, scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(breaks = 1:2, labels = c("Compost", "Slurry"), expand = expansion(add = 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "right", strip.text = element_text(face = "plain", size = 6.5),
        axis.text.x = element_text(size = 6.5), legend.text = element_text(size = 6.5), legend.key.size = unit(7, "pt"),
        panel.spacing = unit(8, "pt"))
fig1 <- ((s1map + theme(legend.position = "bottom")) | (s1time / s1n + plot_layout(heights = c(1.2, 1)))) +
  plot_layout(widths = c(1, 1.15)) + tags_pub()
save_fig(fig1, "fig1_design", 180, 110)
