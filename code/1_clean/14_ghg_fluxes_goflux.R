# 14_ghg_fluxes_goflux.R
# Recompute CO2, CH4 and N2O chamber fluxes from the raw 1 Hz concentration
# records with goFlux (Rheault et al. 2024) version 0.5.0.9002 with additions
# (Gewirtzman 2026, doi:10.5281/zenodo.23256675; github.com/jgewirtzman/goFlux,
# commit 006f625), pinned in the project library r-lib/ (see .Rprofile):
#   - Flux = goFlux::best.flux (LM or HM, Hüppi et al. 2018 criteria), via
#     goFlux::process.fluxes().
#   - MDF = z * sigma / t * flux.term with z = 1.96 (conf = 0.95; a benchmark
#     multiplier, not a calibrated 95 % test; Cowan et al. 2025). sigma = the
#     per-closure second-difference (Hadamard) precision, 1.4826 * MAD(diff2) / sqrt(6)
#     within the fitted window, median per analyzer x field day (and logging
#     interval) (goFlux::empirical.prec); t = goFlux::closure.time. Below-MDF fluxes
#     are kept signed. Retain-and-flag: nothing is deleted.
#   - goFlux::qc.flags physical QC screens flag closures for review (not removed).
#
# Inputs
#   data/raw/flux/json/*.json        LI-8200 smart chamber + LI-7810 (CO2, CH4, H2O)
#   data/raw/flux/data/TG20-*.data   LI-7820 (N2O, H2O), separate logger clock
# Windows
#   From the smart chamber: start = closure + 25 s deadband (as logged by the
#   chamber), end = chamber opening (~95 s). N2O uses the same windows after
#   shifting the LI-7820 clock onto the chamber clock: the per-day offset is
#   the difference between the H2O rise onsets in the LI-7820 record and in the
#   chamber's own (LI-7810) record, both found with goFlux::find.clock.offset()
#   against the logged closure times. Offsets drift ~1 s/day and reset twice
#   (3 Jun, 30 Sep). The day offset is then refined per closure by
#   cross-correlating the chamber H2O trace with the LI-7820 H2O trace
#   (search +-100 s around the day offset, excluding the neighbouring-closure
#   aliases at ~+-200 s), smoothed with a running median of 5 within each day.
#   This catches a ~70 s clock step at ~14:10 on 30 May (plots 10-15) that a
#   single day offset missed.
# Geometry
#   Vtot = chamber (4244.1 cm3) + collar offset x area (318 cm2) + the closed
#   loop outside the chamber, built explicitly rather than taken from the header.
#   Lab convention: 28 cm3 of analyzer internal volume for each analyzer in the
#   loop (LI-COR's LI-78xx analyzer volume; not the 6.4 cm3 optical cavity quoted
#   by goFlux), plus the tubing. With the LI-7810 and LI-7820 on LI-COR's
#   two-analyzer setup (T-split at the chamber, one 2 m assembly to each
#   analyzer; 33.97 cm3 of tubing per branch) the loop is
#   2 x 28 + 2 x 33.97 = 123.94 cm3 on every date.
#   The chamber header's TotalVolume is not used as is, because its IrgaVolume
#   was a single-analyzer setting that changed with the chamber configuration,
#   not with the plumbing: 61.97 = 28 + 33.97 on 6, 27 and 29 May (KT01 files),
#   46.96 = 28 + 18.96 (1.2 m bundle default) from 30 May. The header is used
#   only for chamber + collar (TotalVolume - IrgaVolume).
#   SoilFluxPro's export used header TotalVolume + 67.94 cm3 (114.9 or 129.9 cm3
#   of loop), so goFlux Vtot / SoilFluxPro VOLUME_TOTAL is ~1.0015 from 30 May and
#   ~0.999 before. Pcham/Tcham from the chamber sensors.
# Repeat closures of a collar on one date: the last one is kept (field-sheet
#   notes record re-measurements after leaks/restarts; same rule as the team's
#   "redo supersedes" convention). Dropped closures are listed in the report.
# Outputs
#   data/intermediate/flux_goflux.csv               one row per collar x date, all goFlux fields
#   data/intermediate/flux_estimates.csv            analysis table (same columns as before + MDF flags)
#   output/qc/goflux_clock_offsets.csv, goflux_qc_review.csv (QC)

`%||%` <- function(a, b) if (is.null(a)) b else a
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(goFlux); library(jsonlite)
})
stopifnot(packageVersion("goFlux") == "0.5.0.9002")

TZ <- "America/New_York"
SHOULDER_S <- 30
PREC_7810 <- c(CO2 = 3.5, CH4 = 0.6, H2O = 45)      # datasheet 1-s precision (goFlux default)
PREC_7820 <- c(N2O = 0.4, H2O = 45)
# Closed loop outside the chamber (see Geometry above). Lab convention: 28 cm3 of
# analyzer internal volume per analyzer in the loop; tubing is LI-COR's value for
# one 2 m assembly + T-split leg, one branch per analyzer.
ANALYZER_VOL_L <- 0.028
TUBING_BRANCH_VOL_L <- 0.03397
dir.create("output/qc", showWarnings = FALSE, recursive = TRUE)
treatment_key <- read.csv("data/intermediate/treatment_key.csv")

# --- 1. Smart chamber records (CO2, CH4) --------------------------------------
json_files <- list.files("data/raw/flux/json", pattern = "\\.json$", full.names = TRUE)
json_files <- json_files[!grepl("^2025_05_06_KT01", basename(json_files))]   # empty exports
ch <- bind_rows(lapply(json_files, function(f) {
  x <- tryCatch(as.data.frame(import.LI8200(f, timezone = TZ)), error = function(e) NULL)
  if (!is.null(x)) x$json_file <- basename(f)
  x
}))
ch <- ch[!duplicated(paste(ch$chamID, ch$Etime)), ]      # the KT01 exports overlap
ch <- ch[!is.na(ch$rep) & !is.na(ch$Etime), ]              # empty datasets (aborted closures, no reps)

# per-measurement header: TotalVolume (cm3), Offset, start time
hdr <- bind_rows(lapply(json_files, function(f) {
  d <- fromJSON(f, simplifyVector = FALSE)
  bind_rows(lapply(d$datasets, function(ds) bind_rows(lapply(names(ds), function(nm)
    bind_rows(lapply(names(ds[[nm]]$reps), function(rp) {
      h <- ds[[nm]]$reps[[rp]]$header
      tibble(chamID = paste0(nm, "_", sub("REP_", "", rp)), TotalVolume_cm3 = h$TotalVolume,
             IrgaVolume_cm3 = h$IrgaVolume, Offset_cm = h$Offset, header_time = h$Date)
    }))))))
})) %>% distinct(chamID, .keep_all = TRUE)

m <- regmatches(ch$chamID, regexec("^(\\d{4}-\\d{2}-\\d{2})[-_](\\d+)([A-Ca-c])", ch$chamID))
ch$date_name <- as.Date(sapply(m, `[`, 2)); ch$plot <- as.integer(sapply(m, `[`, 3))
ch$collar <- toupper(sapply(m, `[`, 4))
ch <- ch %>% filter(!is.na(plot), plot %in% 1:15) %>% left_join(hdr, by = "chamID")

closures <- ch %>% group_by(chamID) %>%
  summarize(date_name = first(date_name), plot = first(plot), collar = first(collar),
            start.time = first(start.time), end.time = first(cham.open), cham.close = first(cham.close),
            Vtot_header = first(TotalVolume_cm3) / 1000, IrgaVolume = first(IrgaVolume_cm3) / 1000, Area = first(Area),
            Pcham = mean(Pcham[flag == 1], na.rm = TRUE), Tcham = mean(Tcham[flag == 1], na.rm = TRUE),
            json_file = first(json_file), .groups = "drop") %>%
  mutate(bad_clock = is.na(start.time) | format(start.time, "%Y") != "2025",
         date = case_when(date_name == as.Date("2025-09-11") ~ as.Date("2025-09-10"),
                          date_name == as.Date("2025-10-15") ~ as.Date("2025-10-14"),
                          TRUE ~ date_name)) %>%
  arrange(date, plot, collar, start.time)
# repeat closures of a collar on one sheet date. Field sheets give the reason for
# most repeats (leak, restart, collar height/offset corrected, "#2 is correct"):
# the last closure supersedes. The only chamber remark marks a confirmation
# measurement (30 May 13B2: "SECOND MEASUREMENT TO CONFIRM POSITIVE METHANE FLUX"),
# so when any closure in a group carries a CONFIRM remark, all are kept and averaged.
remarks <- bind_rows(lapply(json_files, function(f) {
  d <- fromJSON(f, simplifyVector = FALSE)
  bind_rows(lapply(d$datasets, function(ds) bind_rows(lapply(names(ds), function(nm)
    bind_rows(lapply(names(ds[[nm]]$reps), function(rp)
      tibble(chamID = paste0(nm, "_", sub("REP_", "", rp)), remark = ds[[nm]]$remark %||% "")))))))
})) %>% distinct(chamID, .keep_all = TRUE)
closures <- closures %>% left_join(remarks, by = "chamID") %>%
  group_by(date_name, plot, collar) %>%
  mutate(n_rep = n(), confirm = any(grepl("CONFIRM", toupper(remark))),
         keep = confirm | row_number() == n(),
         repeat_rule = case_when(n_rep == 1 ~ "single", confirm ~ "confirmation: averaged",
                                 keep ~ "repeat: last kept", TRUE ~ "repeat: superseded")) %>% ungroup()
write.csv(closures %>% filter(n_rep > 1) %>% select(date_name, plot, collar, chamID, start.time, remark, repeat_rule),
          "output/qc/goflux_repeat_closures.csv", row.names = FALSE)
dropped <- closures %>% filter(!keep)
closures <- closures %>% filter(keep)

# early seal break: a sharp CO2 drop (>15 ppm within 3 s) inside the fit window
# means the chamber opened or lost its seal before the end of the record (only
# 29 May 6C, at ~106 s after closure). The window for all gases is cut 1 s before
# the drop. (Rows with Etime corrupted by +2^32 s lie outside the window.)
BREAK_PPM_3S <- -15
break_at <- sapply(closures$chamID, function(id) {
  w <- ch[ch$chamID == id & ch$Etime >= 0 & ch$Etime <= 95, c("Etime", "CO2dry_ppm")]
  w <- w[order(w$Etime), ]
  if (nrow(w) < 10) return(NA_real_)
  d3 <- w$CO2dry_ppm[-(1:3)] - head(w$CO2dry_ppm, -3)
  i <- which(d3 < BREAK_PPM_3S)
  if (length(i)) w$Etime[i[1]] - 1 else NA_real_
})
closures <- closures %>%
  mutate(window_truncated_s = unname(break_at),
         end.time = if_else(!is.na(window_truncated_s), start.time + window_truncated_s, end.time))
cat(sprintf("Early seal breaks (window truncated): %s\n",
            paste(closures$chamID[!is.na(closures$window_truncated_s)], collapse = ", ")))
cat(sprintf("Chamber closures: %d kept, %d superseded repeats dropped; %d with a corrupted chamber clock\n",
            nrow(closures), nrow(dropped), sum(closures$bad_clock)))

# --- 2. LI-7820 N2O records + clock alignment ---------------------------------
read_7820 <- function(f) {           # skip malformed rows (e.g. one truncated line on 11 Sep)
  L <- readLines(f, warn = FALSE); nf <- lengths(strsplit(L, "\t"))
  nh <- nf[startsWith(L, "DATAH")][1]; keep <- !startsWith(L, "DATA\t") | nf == nh
  tf <- tempfile(fileext = ".data"); writeLines(L[keep], tf)
  x <- as.data.frame(import.LI7820(tf, timezone = TZ)); x$n_malformed <- sum(!keep); x
}
n2 <- bind_rows(lapply(list.files("data/raw/flux/data", pattern = "^TG20.*\\.data$", full.names = TRUE), read_7820))
n2 <- n2[!is.na(n2$POSIX.time) & !duplicated(n2$POSIX.time), ]
n2$day <- as.Date(n2$POSIX.time, tz = TZ)

ch_ok <- ch %>% filter(format(POSIX.time, "%Y") == "2025")
ch_ok$day <- as.Date(ch_ok$POSIX.time, tz = TZ)
days <- sort(unique(ch_ok$day))
offsets <- bind_rows(lapply(days, function(d) {
  starts <- closures %>% filter(!bad_clock, as.Date(start.time, tz = TZ) == d) %>% pull(cham.close)
  n1 <- n2[n2$day == d, ]
  if (length(starts) < 5 || nrow(n1) < 300) return(tibble(day = d, n_closures = length(starts)))
  a <- find.clock.offset(n1, starts, gastype = "H2O_ppm", search = c(-150, 150), window = 30, plot = FALSE)
  b <- find.clock.offset(ch_ok[ch_ok$day == d, ], starts, gastype = "H2O_ppm", search = c(-120, 120), window = 30, plot = FALSE)
  tibble(day = d, n_closures = length(starts), onset_7820_s = a$offset, score_7820 = round(a$score, 1),
         onset_chamber_s = b$offset, clock_offset_s = a$offset - b$offset)
}))

# per-closure refinement: lag maximizing cor(chamber H2O, shifted LI-7820 H2O)
day_off <- setNames(offsets$clock_offset_s, as.character(offsets$day))
closure_lag <- function(id) {
  tr <- ch[ch$chamID == id, ]; cc <- tr$cham.close[1]
  if (is.na(cc) || format(cc, "%Y") != "2025") return(NULL)
  o0 <- day_off[as.character(as.Date(cc, tz = TZ))]; if (is.na(o0)) return(NULL)
  tc <- as.numeric(tr$POSIX.time - cc, units = "secs")
  x <- n2[n2$POSIX.time >= cc + o0 - 150 & n2$POSIX.time <= cc + o0 + 300, ]
  if (nrow(x) < 100) return(NULL)
  tx <- as.numeric(x$POSIX.time - cc, units = "secs")
  lags <- (o0 - 100):(o0 + 100)
  r <- vapply(lags, function(L) {
    y <- approx(tx - L, x$H2O_ppm, xout = tc)$y; ok <- !is.na(y) & !is.na(tr$H2O_ppm)
    if (sum(ok) < 60) NA_real_ else cor(y[ok], tr$H2O_ppm[ok])
  }, numeric(1))
  if (all(is.na(r))) return(NULL)
  tibble(chamID = id, cham.close = cc, day_offset_s = unname(o0), lag_s = lags[which.max(r)], lag_r = max(r, na.rm = TRUE))
}
lags <- bind_rows(lapply(closures$chamID[!closures$bad_clock], closure_lag)) %>%
  mutate(day = as.Date(cham.close, tz = TZ)) %>% arrange(cham.close) %>% group_by(day) %>%
  mutate(run_med = as.numeric(stats::runmed(lag_s, k = min(5, 2 * ((n() - 1) %/% 2) + 1), endrule = "median")),
         clock_offset_s = ifelse(abs(lag_s - run_med) <= 5 & lag_r >= 0.9, lag_s, run_med)) %>% ungroup()
write.csv(lags, "output/qc/goflux_clock_offsets_by_closure.csv", row.names = FALSE)

# LI-7820 in the loop: the closure has an aligned LI-7820 trace (or, lacking one,
# another closure of the same sheet date does). Vtot = chamber + collar + one
# analyzer and one tubing branch per analyzer in the loop.
closures <- closures %>% group_by(date_name) %>%
  mutate(li7820_in_loop = chamID %in% lags$chamID | any(chamID %in% lags$chamID)) %>% ungroup() %>%
  mutate(Vloop = (1 + li7820_in_loop) * (ANALYZER_VOL_L + TUBING_BRANCH_VOL_L),
         Vtot = Vtot_header - IrgaVolume + Vloop)
cat(sprintf("LI-7820 in the loop for %d of %d closures; loop volume %s L; Vtot median %.3f L (header %.3f L, x%.4f)\n",
            sum(closures$li7820_in_loop), nrow(closures), paste(unique(round(closures$Vloop, 5)), collapse = "/"),
            median(closures$Vtot), median(closures$Vtot_header), median(closures$Vtot / closures$Vtot_header)))
saveRDS(list(ch = ch, n2 = n2, closures = closures, dropped = dropped, lags = lags),
        "data/intermediate/flux_raw_traces.rds")   # for code/qc/plot_closure_traces.R
offsets <- offsets %>% left_join(lags %>% group_by(day) %>%
  summarise(closure_offset_min = min(clock_offset_s), closure_offset_max = max(clock_offset_s),
            n_differ_gt5s = sum(abs(clock_offset_s - day_offset_s) > 5)), by = "day")
write.csv(offsets, "output/qc/goflux_clock_offsets.csv", row.names = FALSE)
cat("LI-7820 clock offsets (s, analyzer minus chamber):\n"); print(as.data.frame(offsets))

# --- 3. Trace segments -----------------------------------------------------------
seg_one <- function(tr, cl, gas_cols, prec, instrument) {
  s <- tr[tr$POSIX.time >= cl$start.time - SHOULDER_S & tr$POSIX.time <= cl$end.time + SHOULDER_S, ]
  if (nrow(s) < 10) return(NULL)
  s <- s[, c("POSIX.time", gas_cols, "H2O_ppm")]
  s$UniqueID <- cl$chamID
  s$flag <- as.numeric(s$POSIX.time >= cl$start.time & s$POSIX.time <= cl$end.time)
  if (sum(s$flag) < 30) return(NULL)
  s$start.time <- cl$start.time; s$end.time <- cl$end.time
  s$Etime <- as.numeric(s$POSIX.time - cl$start.time, units = "secs")
  s$obs.length <- as.numeric(cl$end.time - cl$start.time, units = "secs")
  s$obs.length_corr <- s$obs.length; s$start.time_corr <- s$start.time; s$end.time_corr <- s$end.time
  s$Vtot <- cl$Vtot; s$Area <- cl$Area; s$Pcham <- cl$Pcham; s$Tcham <- cl$Tcham
  s$DATE <- format(cl$start.time, "%Y-%m-%d")
  for (g in names(prec)) s[[paste0(g, "_prec")]] <- prec[[g]]
  s$H2O_ppm[is.na(s$H2O_ppm)] <- 0
  s$instrument <- instrument
  s$instrument_day <- paste(instrument, cl$date_name)
  s
}
# CO2/CH4: the chamber's own record, per closure (Etime-based, so works even with a bad clock)
man_ch <- bind_rows(lapply(split(ch, ch$chamID), function(tr) {
  cl <- closures[closures$chamID == tr$chamID[1], ]
  if (!nrow(cl)) return(NULL)
  if (cl$bad_clock) {   # rebuild a notional time axis; only Etime matters for the fit
    cl$start.time <- as.POSIXct(paste(cl$date_name, "12:00:00"), tz = TZ)
    cl$end.time <- cl$start.time + 95
  }
  tr$POSIX.time <- cl$start.time + tr$Etime            # rebuild times from Etime (robust to clock faults)
  seg_one(tr, cl, c("CO2dry_ppm", "CH4dry_ppb"), PREC_7810, "LI-7810")
}))
# N2O: LI-7820 record shifted onto the chamber clock
off_map <- setNames(lags$clock_offset_s, lags$chamID)
man_n2o <- bind_rows(lapply(seq_len(nrow(closures)), function(i) {
  cl <- closures[i, ]; if (cl$bad_clock) return(NULL)
  off <- off_map[cl$chamID]
  if (is.na(off)) return(NULL)
  tr <- n2[n2$POSIX.time >= cl$start.time + off - 60 & n2$POSIX.time <= cl$end.time + off + 60, ]
  tr$POSIX.time <- tr$POSIX.time - off
  seg_one(tr, cl, "N2Odry_ppb", PREC_7820, "LI-7820")
}))
cat(sprintf("Segments: CO2/CH4 %d closures; N2O %d closures\n",
            n_distinct(man_ch$UniqueID), n_distinct(man_n2o$UniqueID)))

# --- 4. goFlux: fluxes, detection and QC flags ---------------------------------
# Precision for the detection limit: goFlux::empirical.prec (default of process.fluxes),
# median per analyzer x field day. The datasheet precision in the *_prec columns still
# sets goFlux's kappa-max for the HM fit, as before.
aux_ch  <- man_ch %>% distinct(UniqueID, instrument_day)
aux_n2o <- man_n2o %>% distinct(UniqueID, instrument_day)
qc_list <- list(min.secs = 60, min.obs = NULL,   # closures shorter than 60 s
                ambient.sigma = NULL)              # windows start after the 25 s deadband
res_co2 <- process.fluxes(man_ch, "CO2dry_ppm", auxfile = aux_ch, by = "instrument_day", conf = 0.95, qc = FALSE)
res_ch4 <- process.fluxes(man_ch, "CH4dry_ppb", auxfile = aux_ch, by = "instrument_day", conf = 0.95,
                          qc = qc_list, co2.flux.result = res_co2$fluxes)
res_n2o <- process.fluxes(man_n2o, "N2Odry_ppb", auxfile = aux_n2o, by = "instrument_day", conf = 0.95,
                          qc = qc_list, co2.flux.result = res_co2$fluxes)
saveRDS(list(CO2 = res_co2$settings, CH4 = res_ch4$settings, N2O = res_n2o$settings),
        "output/qc/goflux_settings.rds")   # every option used, goFlux version and commit

pick <- function(f, gas) {
  se <- ifelse(f$model == "HM" & !is.na(f$HM.SE), f$HM.SE, f$LM.SE)
  flag_cols <- grep("^qc\\.(c0|convex|min\\.secs|min\\.obs|noisy|ambient|clock)$", names(f), value = TRUE)
  # a CH4/N2O closure whose CO2 did not rise clearly is flagged too (co2.tracer FALSE)
  no_co2 <- if ("co2.tracer" %in% names(f)) f$co2.tracer %in% FALSE else rep(FALSE, nrow(f))
  note <- vapply(seq_len(nrow(f)), function(i) {
    fired <- flag_cols[vapply(flag_cols, function(cn) isTRUE(f[[cn]][i]), logical(1))]
    paste(c(sub("^qc\\.", "", fired), if (no_co2[i]) "no CO2 rise"), collapse = ", ")
  }, character(1))
  qc_any <- if ("qc.any" %in% names(f)) (f$qc.any %in% TRUE) | no_co2 else NA
  out <- tibble(chamID = f$UniqueID, flux = f$best.flux, model = f$model, LM = f$LM.flux, HM = f$HM.flux,
                SE = se, LM_r2 = f$LM.r2, g_fact = f$g.fact, sigma_prec = f$det.prec, MDF = f$det.MDF,
                below_MDF = f$det.class == "below MDF", det_class = f$det.class, quality_check = f$quality.check,
                qc_any = qc_any, qc_note = if ("qc.any" %in% names(f)) note else NA)
  names(out)[-1] <- paste0(gas, "_", names(out)[-1]); out
}
gf <- closures %>%
  left_join(pick(res_co2$fluxes, "CO2"), by = "chamID") %>%
  left_join(pick(res_ch4$fluxes, "CH4"), by = "chamID") %>%
  left_join(pick(res_n2o$fluxes, "N2O"), by = "chamID") %>%
  left_join(treatment_key, by = "plot")
write.csv(gf, "data/intermediate/flux_goflux.csv", row.names = FALSE)

# analysis table in the previous schema (+ detection fields); confirmation repeats averaged
gf_collar <- gf %>% group_by(date, plot, collar, treatment) %>%
  summarize(n_closures = n(),
            across(where(is.numeric) & !any_of(c("n_rep")), ~ mean(.x, na.rm = TRUE)),
            across(where(is.logical), ~ any(.x, na.rm = TRUE)),
            across(where(is.character) & !any_of(c("treatment", "collar")), ~ paste(unique(.x), collapse = "|")),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)),
         # detection re-evaluated on the averaged flux
         CH4_below_MDF = abs(CH4_flux) < CH4_MDF, N2O_below_MDF = abs(N2O_flux) < N2O_MDF,
         CH4_det_class = ifelse(CH4_below_MDF, "below detection", ifelse(CH4_flux > 0, "emission", "uptake")),
         N2O_det_class = ifelse(N2O_below_MDF, "below detection", ifelse(N2O_flux > 0, "emission", "uptake")))
est <- gf_collar %>% transmute(plot, treatment, date, collar,
                        FCO2_DRY = CO2_flux, FCH4_DRY = CH4_flux, FN2O = N2O_flux,
                        FCO2_R2 = CO2_LM_r2, FCH4_R2 = CH4_LM_r2, FN2O_R2 = N2O_LM_r2,
                        CO2_model, CH4_model, N2O_model,
                        CH4_MDF, CH4_below_MDF, CH4_det_class, N2O_MDF, N2O_below_MDF, N2O_det_class,
                        CH4_qc_any, N2O_qc_any) %>%
  arrange(date, plot, collar)
write.csv(est, "data/intermediate/flux_estimates.csv", row.names = FALSE)

# --- 5. Reports ------------------------------------------------------------------
qc_rev <- gf %>% filter(CH4_qc_any %in% TRUE | N2O_qc_any %in% TRUE) %>%
  select(chamID, date, plot, collar, CH4_flux, CH4_qc_note, N2O_flux, N2O_qc_note)
write.csv(qc_rev, "output/qc/goflux_qc_review.csv", row.names = FALSE)
det <- gf %>% summarize(across(c(CH4_below_MDF, N2O_below_MDF), ~ round(100 * mean(.x, na.rm = TRUE), 1)),
                        HM_CO2 = round(100 * mean(CO2_model == "HM", na.rm = TRUE), 1),
                        HM_CH4 = round(100 * mean(CH4_model == "HM", na.rm = TRUE), 1),
                        HM_N2O = round(100 * mean(N2O_model == "HM", na.rm = TRUE), 1))
cat("\n% below MDF and % HM model:\n"); print(as.data.frame(det))
cat(sprintf("QC screens flagged %d closures for review (not removed)\n", nrow(qc_rev)))
