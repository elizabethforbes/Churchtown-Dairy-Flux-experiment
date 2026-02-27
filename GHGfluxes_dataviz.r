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
  geom_point()+
  geom_smooth(method = "loess",
              alpha = 0.15)+
  labs(x = "", y = "CO2 flux rate, umol/m2/sec")+
  theme_bw()

# plot CH4:
ghg_avg %>% 
  ggplot(aes(x=week, y = FCH4_DRY, colour = trtmnt))+
  geom_point()+
  geom_smooth(method = "loess",
              alpha = 0.15)+
  labs(x = "", y = "CH4 flux rate, umol/m2/sec")+
  theme_bw()

# plot N20:
ghg_avg %>% 
  ggplot(aes(x=week, y = FN2O, colour = trtmnt))+
  geom_point()+
  geom_smooth(method = "loess",
              alpha = 0.15)+
  labs(x = "", y = "N20 flux rate, umol/m2/sec")+
  theme_bw()
