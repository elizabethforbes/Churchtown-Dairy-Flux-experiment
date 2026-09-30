# figS4_cmin_timecourses.R
# Fig S4. C-mineralization time courses (20 degC, 65% WHC): plot means (thin lines) and
# treatment means +/- SE, by sampling.
# Input: data/clean/soil_cmin_timecourse.csv

source("code/lib/setup.R")
sampled_fac <- function(r) factor(paste0("Sampled ", ROUND_LABELS[as.character(r)]),
                                  levels = paste0("Sampled ", ROUND_LABELS))
ctr <- clean_csv("soil_cmin_timecourse.csv", colClasses = c(lab_no = "character")) %>%
  filter(is.na(flag), !is.na(cmin_rate_ug_co2c_hr_g)) %>%
  group_by(plot, treatment, timepoint, day) %>%
  summarize(rate = mean(cmin_rate_ug_co2c_hr_g), .groups = "drop") %>%
  mutate(treatment = as_trt(treatment), round_lab = sampled_fac(timepoint))
cts <- trt_summary(ctr, rate, round_lab, day)
figs5 <- ggplot() +
  geom_line(data = ctr, aes(day, rate, colour = treatment, group = plot), linewidth = 0.25, alpha = 0.35,
            position = position_dodge(0.8), show.legend = FALSE) +
  geom_point(data = ctr, aes(day, rate, colour = treatment), shape = 16, size = 0.7, alpha = 0.45,
             position = position_dodge(0.8), show.legend = FALSE) +
  geom_linerange(data = cts, aes(day, ymin = mean - se, ymax = mean + se, colour = treatment),
                 position = position_dodge(0.8), linewidth = 0.4, show.legend = FALSE) +
  geom_line(data = cts, aes(day, mean, colour = treatment), position = position_dodge(0.8), linewidth = 0.6, show.legend = FALSE) +
  geom_point(data = cts, aes(day, mean, colour = treatment, fill = treatment, shape = treatment),
             position = position_dodge(0.8), size = 1.6, stroke = 0.35) +
  facet_wrap(~ round_lab, nrow = 1) +
  scale_colour_trt() + scale_fill_trt() + scale_shape_trt() +
  scale_x_continuous(breaks = c(0, 7, 14, 21, 28)) +
  labs(x = "Day of incubation", y = expression(CO[2]*'-C'~(mu*g~g^{-1}~h^{-1})))
save_fig(figs5, "figS4_cmin_timecourses", 180, 70, "si")

