# 06_field_probe_readings.R
# Handheld probe readings at each chamber collar: volumetric water content (VWC,
# %; two-rod probe listed as 10 cm in project notes) and soil temperature (10 cm),
# for every GHG flux campaign.
# Inputs:
#   data/raw/field_metadata/In situ fluxes meta-data.xlsx           27 May - 3 Jun (typed in 2025)
#   data/raw/field_metadata/handheld_probe_transcribed_2026-09-28.csv 19 Jun - 15 Oct
#     (transcribed from the scanned field sheets; see
#      handheld_probe_transcribed_TO_VERIFY.xlsx; 'uncertain' rows flagged)
# Output: data/intermediate/field_metadata.csv
# Split campaigns are merged to the flux dates (11 Sep -> 10 Sep, 15 Oct -> 14 Oct).
# No handheld sheet exists for 6 May. 22 Aug has no soil temperature (not recorded);
# on 23 Jul the temperature probe was broken for plots 3, 6 and collar 9C.

library(readxl)
library(dplyr)

treatment_key <- read.csv("data/intermediate/treatment_key.csv")
num <- function(x) suppressWarnings(as.numeric(x))

typed <- read_excel("data/raw/field_metadata/In situ fluxes meta-data.xlsx", sheet = "Sheet1") %>%
  transmute(plot = as.integer(Plot), collar = as.character(Collar), date = as.Date(Date),
            time = format(as.POSIXct(Time, format = "%H:%M"), "%H:%M"),
            vwc1 = num(VWC1), vwc2 = num(VWC2), vwc3 = num(VWC3),
            soil_temp_c = num(soil_temp_C), mean_depth = num(mean_depth),
            notes = as.character(notes), uncertain = FALSE, source = "typed_xlsx") %>%
  filter(!is.na(plot))

trans <- read.csv("data/raw/field_metadata/handheld_probe_transcribed_2026-09-28.csv", colClasses = "character") %>%
  transmute(plot = as.integer(plot), collar = collar, date = as.Date(sheet_date), time = time,
            vwc1 = num(vwc1), vwc2 = num(vwc2), vwc3 = num(vwc3),
            soil_temp_c = num(soil_temp_c), mean_depth = num(mean_depth_cm),
            notes = notes, uncertain = tolower(uncertain) == "yes", source = "transcribed_2026-09-28")

meta <- bind_rows(typed, trans) %>%
  mutate(sheet_date = date,
         date = case_when(date == as.Date("2025-09-11") ~ as.Date("2025-09-10"),
                          date == as.Date("2025-10-15") ~ as.Date("2025-10-14"),
                          TRUE ~ date),
         mean_vwc = rowMeans(cbind(vwc1, vwc2, vwc3), na.rm = TRUE),
         mean_vwc = ifelse(is.nan(mean_vwc), NA, mean_vwc)) %>%
  left_join(treatment_key, by = "plot") %>%
  select(plot, treatment, date, sheet_date, collar, time, vwc1, vwc2, vwc3,
         mean_vwc, soil_temp_c, mean_depth, uncertain, source, notes) %>%
  arrange(date, plot, collar)

stopifnot(!any(duplicated(meta[, c("date", "plot", "collar")])))
write.csv(meta, "data/intermediate/field_metadata.csv", row.names = FALSE)
cat("Wrote data/intermediate/field_metadata.csv\n")
cat(sprintf("  %d rows, %d campaigns; VWC present %d, soil T present %d\n", nrow(meta),
            n_distinct(meta$date), sum(!is.na(meta$mean_vwc)), sum(!is.na(meta$soil_temp_c))))
