# 10_amendments.R
# Process DairyOne manure amendment nutrient analysis
# Samples taken 2025-05-28, received by Dairy One 2026-01-19. All % are as received (fresh basis).
# Output: data/intermediate/dairy_one_manure.csv

library(dplyr)

manure_raw <- read.csv("data/raw/dairy_one/manure/DC394691-26.CSV")

manure <- manure_raw %>%
  transmute(
    sample_id = trimws(Sample.Number),
    description = trimws(Description.1),
    amendment_type = case_when(
      grepl("SLURRY", Description.1, ignore.case = TRUE) ~ "slurry",
      grepl("COMPOST", Description.1, ignore.case = TRUE) ~ "compost",
      TRUE ~ NA_character_
    ),
    replicate = as.integer(gsub(".*\\s(\\d+)$", "\\1", trimws(Description.1))),
    total_n_pct = as.numeric(Nitrogen..N.),
    ammonium_n_pct = as.numeric(Ammonium.Nitrogen..NH3.),  # as received; nitrate not requested
    organic_n_pct = as.numeric(Organic.Nitrogen),
    p_pct = as.numeric(Phosphorus..P.),
    k_pct = as.numeric(Potassium..K.),
    total_solids_pct = as.numeric(Total.Solids),
    ash_pct = as.numeric(Ash)
  ) %>%
  filter(!is.na(amendment_type)) %>%
  arrange(amendment_type, replicate)

write.csv(manure, "data/intermediate/dairy_one_manure.csv", row.names = FALSE)
cat("Wrote data/intermediate/dairy_one_manure.csv\n")
cat(sprintf("  %d rows (%s)\n", nrow(manure),
            paste(table(manure$amendment_type), names(table(manure$amendment_type)),
                  collapse = ", ")))
