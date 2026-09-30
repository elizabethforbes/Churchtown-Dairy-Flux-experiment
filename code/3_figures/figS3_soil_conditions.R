# figS3_soil_conditions.R
# Fig S3. (a) Gravimetric water content at each soil sampling (plot values; treatment
# means with 95% CI); (b) soil temperature vs volumetric water content at 10 cm from the
# handheld probe after application (grey: collars; black: campaign means).
# Input: data/clean/soil_gwc.csv, field_probe_readings.csv

source("code/lib/setup.R")
round_fac <- function(r) factor(ROUND_LABELS[as.character(r)], levels = ROUND_LABELS)
gwc <- clean_csv("soil_gwc.csv") %>% group_by(plot, treatment, timepoint) %>%
  summarize(gwc = mean(gwc), .groups = "drop") %>% mutate(treatment = as_trt(treatment), round_lab = round_fac(timepoint))
s4a <- ggplot() + dot_ci_layers(gwc, trt_summary(gwc, gwc, round_lab), round_lab, gwc) +
  labs(x = "Soil sampling", y = expression(Gravimetric~water~(g~g^{-1})))
hand <- clean_csv("field_probe_readings.csv") %>% mutate(date = as.Date(date))
cov_panel <- function(d, x, y, title) {
  cm <- d %>% group_by(date) %>% summarize(x = mean(.data[[x]], na.rm = TRUE), y = mean(.data[[y]], na.rm = TRUE)) %>% filter(!is.nan(x), !is.nan(y))
  ok <- complete.cases(d[[x]], d[[y]])
  ggplot(d, aes(.data[[x]], .data[[y]])) +
    geom_point(colour = "grey70", shape = 16, size = 0.6, alpha = 0.6) +
    geom_point(data = cm, aes(x, y), colour = INK, size = 1.6) +
    labs(x = "Soil temperature, 10 cm (°C)", y = expression(VWC~(m^3~m^{-3})))
}
s4e <- cov_panel(hand %>% filter(date > APPLICATION_DATE) %>% mutate(W = mean_vwc / 100), "soil_temp_c", "W", "Handheld soil probe")
figs4 <- ((s4a + theme(legend.position = "bottom")) | s4e) + tags_pub()
save_fig(figs4, "figS3_soil_conditions", 180, 75, "si")

