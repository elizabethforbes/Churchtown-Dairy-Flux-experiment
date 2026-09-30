# figS1_flux_drivers.R
# Fig S1. Soil temperature and moisture as flux drivers (collar measurements after
# application). (a-c) fixed-effect fits per treatment (95% CI) with the driver x treatment
# test; (d-f) the same data coloured by the second driver, with model curves at fixed
# levels of it.
# Input: output/tables/flux_driver_data.csv, flux_driver_pred_bytreatment.csv,
#        flux_driver_pred_bydriver.csv (03_flux_drivers.R)

source("code/lib/setup.R")

resp <- read.csv("output/tables/flux_driver_data.csv") %>% mutate(treatment = as_trt(treatment))
by_trt <- read.csv("output/tables/flux_driver_pred_bytreatment.csv") %>% mutate(treatment = as_trt(treatment))
by_drv <- read.csv("output/tables/flux_driver_pred_bydriver.csv")
YLAB <- list(CO2 = expression(CO[2]~(mu*mol~m^{-2}~s^{-1})), CH4 = expression(CH[4]~(nmol~m^{-2}~s^{-1})),
             N2O = expression(N[2]*O~(nmol~m^{-2}~s^{-1})))
XLAB <- list(Ts = "Soil temperature (°C)", W = expression(Soil~VWC~(m^3~m^{-3})))
YCOL <- c(CO2 = "FCO2_DRY", CH4 = "FCH4_DRY", N2O = "FN2O")

# --- (a-c) one fitted line per treatment ---------------------------------------------
top <- lapply(c("CO2", "CH4", "N2O"), function(g) {
  grid <- by_trt %>% filter(gas == g); x <- grid$driver[1]; y <- YCOL[[g]]; logged <- g == "CO2"
  ggplot(resp, aes(.data[[x]], .data[[y]], colour = treatment)) +
    { if (!logged) zero_line() } +
    geom_point(shape = 16, size = 0.7, alpha = 0.35, show.legend = FALSE) +
    geom_ribbon(data = grid, aes(x = .data[[x]], ymin = lo, ymax = hi, fill = treatment), inherit.aes = FALSE,
                alpha = 0.15, show.legend = FALSE) +
    geom_line(data = grid, aes(.data[[x]], fit, colour = treatment), linewidth = 0.7, show.legend = FALSE) +
    geom_point(data = grid[0, ], aes(.data[[x]], fit, fill = treatment, shape = treatment), size = 2) +
    coord_cartesian(ylim = if (logged) c(0, quantile(resp[[y]], 0.99)) else quantile(resp[[y]], c(0.01, 0.99))) +
    scale_colour_trt(drop = FALSE) + scale_fill_trt(drop = FALSE) + scale_shape_trt(drop = FALSE) +
    labs(x = XLAB[[x]], y = YLAB[[g]])
})

# --- (d-f) coloured by the second driver -------------------------------------------------
TEMP_PAL <- colorspace::sequential_hcl(5, "Heat 2", rev = TRUE)
VWC_PAL  <- colorspace::sequential_hcl(5, "Teal", rev = TRUE)
vwc_scale  <- scale_colour_gradientn(colours = VWC_PAL, name = expression(VWC~(m^3~m^{-3})), limits = c(0, 0.4), oob = scales::squish)
temp_scale <- scale_colour_gradientn(colours = TEMP_PAL, name = "Soil T (°C)", limits = c(10, 30), oob = scales::squish)
g_co2 <- by_drv %>% filter(gas == "CO2"); g_ch4 <- by_drv %>% filter(gas == "CH4"); g_n2o <- by_drv %>% filter(gas == "N2O")
n2o_slope <- (g_n2o$fit[nrow(g_n2o)] - g_n2o$fit[1]) / (g_n2o$W[nrow(g_n2o)] - g_n2o$W[1])   # W-only model is linear
n2o_int <- g_n2o$fit[1] - n2o_slope * g_n2o$W[1]
f_a <- ggplot(resp, aes(Ts, FCO2_DRY, colour = W)) +
  geom_point(shape = 16, size = 0.8, alpha = 0.75) +
  geom_line(data = g_co2, aes(Ts, fit, group = W, colour = W), linewidth = 0.8) +
  vwc_scale + coord_cartesian(ylim = c(0, quantile(resp$FCO2_DRY, 0.99))) +
  labs(x = "Soil temperature (°C)", y = YLAB$CO2)
f_b <- ggplot(resp, aes(W, FCH4_DRY, colour = Ts)) +
  zero_line() +
  geom_point(shape = 16, size = 0.8, alpha = 0.75) +
  geom_line(data = g_ch4, aes(W, fit, group = Ts, colour = Ts), linewidth = 0.8) +
  temp_scale + coord_cartesian(ylim = quantile(resp$FCH4_DRY, c(0.01, 0.99))) +
  labs(x = XLAB$W, y = YLAB$CH4)
f_c <- ggplot(resp, aes(W, FN2O, colour = Ts)) +
  zero_line() +
  geom_point(shape = 16, size = 0.8, alpha = 0.75) +
  geom_abline(intercept = n2o_int, slope = n2o_slope, colour = INK, linewidth = 0.7) +
  temp_scale + coord_cartesian(ylim = quantile(resp$FN2O, c(0.01, 0.99))) +
  labs(x = XLAB$W, y = YLAB$N2O)

s_top <- wrap_plots(top, nrow = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
s_bot <- (f_a | f_b | f_c) &
  theme(legend.position = "bottom", legend.key.width = unit(14, "pt"), legend.key.height = unit(5, "pt"),
        legend.title = element_text(size = 6.5, vjust = 0.8), legend.text = element_text(size = 6))
save_fig((s_top / s_bot) + tags_pub(), "figS1_flux_drivers", 180, 160, "si")
