# 13_gapfill_soil_temp.R
# Gap-fill missing handheld soil temperatures using the chamber probe.
# Input:  data/intermediate/field_metadata.csv (06), data/intermediate/chamber_env.csv (12)
# Output: data/intermediate/field_metadata.csv with added columns
#           soil_temp_filled_c  measured value, or the gap-filled estimate
#           soil_temp_source    "measured" | "filled_within_campaign" | "filled_whole_campaign"
#
# Two cases:
#  1. Collars missing within a campaign that has other handheld readings
#     (23 Jul: probe broken for plots 3, 6 and 9C). Mixed model
#     handheld ~ probe + (1 | date): the campaign's own offset plus the collar's
#     probe deviation. Residual SD ~0.8 C.
#  2. Whole campaign missing (22 Aug: temperature not recorded). Linear model
#     handheld ~ probe + day-of-year + day-of-year^2 on collar-level data from all
#     other campaigns. Leave-one-campaign-out error on campaign means: RMSE 2.0 C,
#     max 3.9 C (probe alone: RMSE 2.2, max 5.0).

library(dplyr)

meta <- read.csv("data/intermediate/field_metadata.csv") %>% mutate(date = as.Date(date))
env  <- read.csv("data/intermediate/chamber_env.csv") %>% mutate(date = as.Date(date))

d <- meta %>%
  left_join(env %>% select(date, plot, collar, probe_t = soil_temp_c), by = c("date", "plot", "collar")) %>%
  mutate(doy = as.numeric(format(date, "%j")))
cal <- d %>% filter(!is.na(soil_temp_c), !is.na(probe_t))

m_within <- lme4::lmer(soil_temp_c ~ probe_t + (1 | date), data = cal)
m_whole  <- lm(soil_temp_c ~ probe_t + poly(doy, 2, raw = TRUE), data = cal)

has_campaign <- d %>% group_by(date) %>% summarize(n_meas = sum(!is.na(soil_temp_c)), .groups = "drop")
d <- d %>% left_join(has_campaign, by = "date") %>%
  mutate(
    pred_within = ifelse(n_meas > 0 & !is.na(probe_t),
                         predict(m_within, newdata = d %>% mutate(date = date), allow.new.levels = TRUE), NA),
    pred_whole  = ifelse(!is.na(probe_t), predict(m_whole, newdata = d), NA),
    soil_temp_source = case_when(
      !is.na(soil_temp_c) ~ "measured",
      n_meas > 0 & !is.na(pred_within) ~ "filled_within_campaign",
      n_meas == 0 & !is.na(pred_whole) ~ "filled_whole_campaign",
      TRUE ~ NA_character_),
    soil_temp_filled_c = case_when(
      soil_temp_source == "measured" ~ soil_temp_c,
      soil_temp_source == "filled_within_campaign" ~ pred_within,
      soil_temp_source == "filled_whole_campaign" ~ pred_whole)
  )

out <- d %>% select(-probe_t, -doy, -n_meas, -pred_within, -pred_whole) %>%
  mutate(soil_temp_filled_c = round(soil_temp_filled_c, 2))
write.csv(out, "data/intermediate/field_metadata.csv", row.names = FALSE)
cat("Gap-filled soil temperature in data/intermediate/field_metadata.csv\n")
print(table(out$soil_temp_source, useNA = "ifany"))
cat(sprintf("  22 Aug filled campaign mean: %.1f C\n",
            mean(out$soil_temp_filled_c[out$date == as.Date("2025-08-22")], na.rm = TRUE)))
