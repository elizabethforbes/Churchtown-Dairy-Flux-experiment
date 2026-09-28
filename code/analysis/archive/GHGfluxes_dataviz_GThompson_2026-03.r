# initial data visualization for Smartchamber-collected survey fluxes in the 2025 manure fertilizer experiment

# load libraries:
library(tidyverse)
library(readxl)
library(lubridate) # useful for datetime manipulation

ghg <- read_xlsx("co2_n20_ch4_allflux2025.xlsx")

# convert DOY to date/time, add date and time cols:
ghg <- ghg %>% 
  mutate(
    day = floor(DOY_initial_value), # gives integer day
    fraction = DOY_initial_value - day, # gets the fraction part of day, aka time
    datetime = make_date(2025, 1, 1) + days(day - 1) + seconds(fraction*86400), # gets base date, offsets by a day, and converts fraction to sections
  ) %>% 
  select(-day, -fraction) %>% 
  mutate(date = as_date(datetime),
         week = isoweek(date)) # gives us the week that a measurement was taken, for averaging purposes

ghg_avg <- ghg %>% 
  group_by(plot_rep, week) %>% 
  summarize(
    datetime = first(datetime),  # keep first obs of datetime
    date = first(date),          # keep first obs of date
    across(where(is.numeric), function(x) mean(x,na.rm = TRUE))) %>% 
  ungroup()

# add plot ID to treatment:
ghg_avg <- ghg_avg %>% 
  mutate(trtmnt = case_when(
    plot_rep %in% c(1,6,11,12,13) ~ "slurry manure",
    plot_rep %in% c(2,5,7,10,15) ~ "no fertilizer",
    TRUE ~ "compost manure"
  ))

# relevel your primary analysis factor so control is first
ghg_avg$trtmnt <- factor(
  ghg_avg$trtmnt,
  levels = c("no fertilizer", "compost manure", "slurry manure"))

# plot CO2:
ghg_avg %>% 
  ggplot(aes(x=week, y = FCO2_DRY, colour = trtmnt))+
  ggplot(aes(x = week, y = FCO2_DRY, colour = trtmnt))+
  geom_point()+
  geom_smooth(method = "loess", # Locally Estimated Scatterplot Smoothing regression: useful for visualzing complex, non-linear relationships in data where no specific mathematical relationship is assumed
              alpha = 0.15)+
  labs(x = "week", y = "CO2 flux rate, umol/m2/sec")+
  theme_bw()

# plot CH4:
ghg_avg %>% 
  ggplot(aes(x=week, y = FCH4_DRY, colour = trtmnt))+
  geom_point()+
  geom_smooth(method = "loess",
              alpha = 0.15)+
  labs(x = "week", y = "CH4 flux rate, umol/m2/sec")+
  theme_bw()

# plot N20:
ghg_avg %>% 
  ggplot(aes(x=week, y = FN2O, colour = trtmnt))+
  geom_point()+
  geom_smooth(method = "loess",
              alpha = 0.15)+
  labs(x = "week", y = "N20 flux rate, umol/m2/sec")+
  theme_bw()

## instead of averaging by plot_rep, average by treatment so that each point represents the averaged flux per treatment type for that week and the associated standard error
ghg_trt <- ghg_avg %>%
  group_by(trtmnt, week) %>%
  summarize(
    CO2_mean = mean(FCO2_DRY, na.rm = TRUE),
    CO2_se = sd(FCO2_DRY, na.rm = TRUE)/sqrt(n()),
    CH4_mean = mean(FCH4_DRY, na.rm = TRUE),
    CH4_se = sd(FCH4_DRY, na.rm = TRUE)/sqrt(n()),
    N2O_mean = mean(FN2O, na.rm = TRUE),
    N2O_se = sd(FN2O, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  )

# plot CO2:
ggplot(ghg_trt,
       aes(x = week,
           y = CO2_mean,
           color = trtmnt,
           group = trtmnt)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = CO2_mean - CO2_se,
        ymax = CO2_mean + CO2_se),
    width = 0.2) +
  labs(
    x = "Week",
    y = expression(CO[2]~flux~(mu*mol~m^-2~s^-1)),
    color = "Treatment") +
  theme_bw()

# plot CH4:
ggplot(ghg_trt,
       aes(x = week,
           y = CH4_mean,
           color = trtmnt,
           group = trtmnt)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = CH4_mean - CH4_se,
        ymax = CH4_mean + CH4_se),
    width = 0.2) +
  labs(
    x = "Week",
    y = expression(CH[4]~flux~(mu*mol~m^-2~s^-1)),
    color = "Treatment") +
  theme_bw()

# plot N2O:
ggplot(ghg_trt,
       aes(x = week,
           y = N2O_mean,
           color = trtmnt,
           group = trtmnt)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = N2O_mean - N2O_se,
        ymax = N2O_mean + N2O_se),
    width = 0.2) +
  labs(
    x = "Week",
    y = expression(N[2]*O~flux~(mu*mol~m^-2~s^-1)),
    color = "Treatment") +
  theme_bw()

# boxplot plot for CO2: 
ggplot(ghg_avg,
       aes(x = factor(week),y = FCO2_DRY, colour = trtmnt)) + # had to make week a factor
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA) +
  geom_point(
    aes(colour = trtmnt),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8),
    size = 1.5,
    alpha = 0.7) +
  labs(
    x = "week",
    y = expression(CO[2]~flux~(mu*mol~m^-2~s^-1)), 
    color = "treatment") +
  theme_bw()

# boxplot CH4: 
ggplot(ghg_avg,
       aes(x = factor(week),y = FCH4_DRY, colour = trtmnt)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA) +
  geom_point(
    aes(colour = trtmnt),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8),
    size = 1.5,
    alpha = 0.7) +
  labs(
    x = "week",
    y = expression(CH[4]~flux~(mu*mol~m^-2~s^-1)), 
    color = "treatment") +
  theme_bw()

# boxplot for N2O: 
ggplot(ghg_avg,
       aes(x = factor(week),y = FN2O, colour = trtmnt)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = NA) +
  geom_point(
    aes(colour = trtmnt),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8),
    size = 1.5,
    alpha = 0.7) +
  labs(
    x = "week",
    y = expression(N[2]*O~flux~(mu*mol~m^-2~s^-1)), 
    color = "treatment") +
  theme_bw()

## plotting individual plots, showing plot variability, outliers, chamber problems, spatial heterogeneity
# plot CO2:
ggplot(ghg_avg,
       aes(week,
           FCO2_DRY,
           color = trtmnt,
           group = plot_rep)) + # plot_rep is the average of three measurements taken during the sampling time
  geom_line(alpha = 0.4) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "black") + 
  theme_bw()

# plot CH4: 
ggplot(ghg_avg,
       aes(week,
           FCH4_DRY,
           color = trtmnt,
           group = plot_rep)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "black") +
  theme_bw()

# plot N2O:
ggplot(ghg_avg,
       aes(week,
           FN2O,
           color = trtmnt,
           group = plot_rep)) +
  geom_line(alpha = 0.4) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "black") + 
  theme_bw()

## Attempt at cumulative models, I am doing everything chatGPT tells me to do
ghg_cumulative <- ghg_avg %>%
  arrange(plot_rep, datetime) %>%
  group_by(plot_rep) %>%
  mutate(dt = as.numeric(difftime(datetime,
                                  lag(datetime),
                                  units = "secs")))

ghg_cumulative <- ghg_cumulative %>%
  group_by(plot_rep) %>%
  mutate(trap_CO2 = (FCO2_DRY + lag(FCO2_DRY)) / 2 * dt, # lag() finds the "previous" values in a vector, making it easy to compare the current row's value with a previous row's value
         trap_CH4 = (FCH4_DRY + lag(FCH4_DRY)) / 2 * dt,
         trap_N2O = (FN2O + lag(FN2O)) / 2 * dt)

ghg_cumulative <- ghg_cumulative %>%
  group_by(plot_rep) %>%
  mutate(
    trap_CO2 = replace_na(trap_CO2, 0),
    trap_CH4 = replace_na(trap_CH4, 0),
    trap_N2O = replace_na(trap_N2O, 0))


ghg_cumulative <- ghg_cumulative %>%
  group_by(plot_rep) %>%
  mutate(
    cum_CO2 = cumsum(trap_CO2), # cumsum() 
    cum_CH4 = cumsum(trap_CH4),
    cum_N2O = cumsum(trap_N2O))

# plot CO2:
ggplot(ghg_cumulative,
       aes(week,
           cum_CO2,
           color = trtmnt,
           group = plot_rep)) +
  geom_line() +
  geom_point() +
  labs(
    x = "week",
    y = expression(CO[2]~cumulative~flux~(mu*mol~m^-2))) +
  theme_bw()

# plot CH4: 
ggplot(ghg_cumulative,
       aes(week,
           cum_CH4,
           color = trtmnt,
           group = plot_rep)) +
  geom_line() +
  geom_point() +
  labs(
    x = "week",
    y = expression(CH[4]~cumulative~flux~(mu*mol~m^-2))) + 
  theme_bw()

# plot N2O: 
ggplot(ghg_cumulative,
       aes(week,
           cum_N2O,
           color = trtmnt,
           group = plot_rep)) +
  geom_line() +
  geom_point() +
  labs(
    x = "week",
    y = expression(N[2]*O~cumulative~flux~(mu*mol~m^-2))) +
  theme_bw()

# Adding vertical lines to define pre and post treatment periods
# plot CO2:
ggplot(ghg_trt,
       aes(x = week,
           y = CO2_mean,
           color = trtmnt,
           group = trtmnt)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = CO2_mean - CO2_se,
        ymax = CO2_mean + CO2_se),
    width = 0.2) +
  labs(
    x = "week",
    y = expression(CO[2]~flux~(mu*mol~m^-2~s^-1)),
    color = "treatment") +
  geom_vline(xintercept = 23,
             linetype = "dashed",
             color = "black") +
  theme_bw()

# plot CH4:
ggplot(ghg_trt,
       aes(x = week,
           y = CH4_mean,
           color = trtmnt,
           group = trtmnt)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = CH4_mean - CH4_se,
        ymax = CH4_mean + CH4_se),
    width = 0.2) +
  labs(
    x = "week",
    y = expression(CH[4]~flux~(mu*mol~m^-2~s^-1)),
    color = "treatment") +
  geom_vline(xintercept = 23,
             linetype = "dashed",
             color = "black") +
  theme_bw()

# plot N2O:
ggplot(ghg_trt,
       aes(x = week,
           y = N2O_mean,
           color = trtmnt,
           group = trtmnt)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = N2O_mean - N2O_se,
        ymax = N2O_mean + N2O_se),
    width = 0.2) +
  labs(
    x = "week",
    y = expression(N[2]*O~flux~(mu*mol~m^-2~s^-1)),
    color = "treatment") +
  geom_vline(xintercept = 23,
             linetype = "dashed",
             color = "black") +
  theme_bw()






