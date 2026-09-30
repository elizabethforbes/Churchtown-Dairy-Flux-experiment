# plot_closure_traces.R
# QC plots of every chamber closure: raw concentration traces for CO2, CH4 and
# N2O, plus H2O from both analyzers, from 2 min before chamber closure to 2 min
# after the chamber record ends.
#   - x axis (bottom): chamber clock (GPS-synced), s from closure and time of day
#   - x axis (top):    LI-7820 clock time for the same instant (adjusted alignment)
#   - N2O / LI-7820 H2O are drawn twice: at the adjusted times used in the flux
#     fits (black) and at the verbatim LI-7820 timestamps (orange)
#   - grey band = fit window (after the 25 s deadband to chamber opening, or to
#     just before an early seal break)
# CO2/CH4/chamber H2O exist only during the chamber record (JSON), so those rows
# are blank outside the closure.
# Input:  data/intermediate/flux_raw_traces.rds (written by code/1_clean/14_ghg_fluxes_goflux.R),
#         data/intermediate/flux_goflux.csv
# Output: output/qc/closure_traces/closure_traces_<date>.pdf (one per field day)
#         and closure_traces_all.pdf; 3 closures per page.

Sys.setlocale("LC_ALL", "en_US.UTF-8")
suppressMessages({library(dplyr); library(ggplot2); library(patchwork)})
TZ <- "America/New_York"
PRE <- 120; POST <- 120    # s before closure / after end of chamber record

raw <- readRDS("data/intermediate/flux_raw_traces.rds")
ch <- raw$ch; n2 <- raw$n2; lags <- raw$lags
gf <- read.csv("data/intermediate/flux_goflux.csv")
tk <- read.csv("data/intermediate/treatment_key.csv")

# one row per closure (kept + superseded repeats); superseded ones get the
# offset of the nearest kept closure in time
cl_all <- bind_rows(raw$closures %>% mutate(status = repeat_rule),
                    raw$dropped %>% mutate(status = "repeat: superseded (not used)")) %>%
  select(chamID, date_name, plot, collar, cham.close, start.time, end.time, status, remark) %>%
  left_join(lags %>% select(chamID, clock_offset_s, lag_r), by = "chamID") %>%
  arrange(cham.close)
miss <- which(is.na(cl_all$clock_offset_s))
for (i in miss) {
  j <- which.min(abs(as.numeric(lags$cham.close - cl_all$cham.close[i], units = "secs")))
  cl_all$clock_offset_s[i] <- lags$clock_offset_s[j]
}
cl_all <- cl_all %>% left_join(tk %>% select(plot, treatment), by = "plot") %>%
  left_join(gf %>% select(chamID, CO2_flux, CO2_model, CH4_flux, CH4_model, CH4_below_MDF,
                          N2O_flux, N2O_model, N2O_MDF, N2O_below_MDF, N2O_qc_note, CH4_qc_note), by = "chamID")

trt_col <- c(control = "#8C8C8C", compost = "#B35806", slurry = "#2166AC")
gas_lab <- c(CO2 = "CO[2]~(ppm)", CH4 = "CH[4]~(ppb)", N2O = "N[2]*O~(ppb)", H2O = "H[2]*O~(ppt)")
src_col <- c("chamber (LI-7810)" = "#2166AC", "LI-7820 adjusted" = "black", "LI-7820 verbatim" = "#E08214")

plot_closure <- function(k) {
  cl <- cl_all[k, ]; cc <- cl$cham.close; off <- cl$clock_offset_s
  tr <- ch[ch$chamID == cl$chamID, ]
  tr <- tr[!is.na(tr$Etime) & tr$Etime < 3600, ]   # drop the +2^32 s overflow rows (outside the fit windows)
  # chamber times from Etime (as in the flux fits); one row each in 6 May 7B and
  # 22 Aug 1A has POSIX.time/Etime corrupted by +2^32 s and is dropped above
  win_s <- as.numeric(cl$start.time - cc, units = "secs")
  tc <- win_s + tr$Etime
  rec_end <- max(tc, na.rm = TRUE)
  xr <- c(-PRE, rec_end + POST)
  d_ch <- bind_rows(
    tibble(t = tc, val = tr$CO2dry_ppm, gas = "CO2"),
    tibble(t = tc, val = tr$CH4dry_ppb, gas = "CH4"),
    tibble(t = tc, val = tr$H2O_ppm / 1000, gas = "H2O")) %>% mutate(src = "chamber (LI-7810)")
  # LI-7820: pull a range wide enough for both alignments
  x <- n2[n2$POSIX.time >= cc + min(0, off) - PRE - 5 & n2$POSIX.time <= cc + max(0, off) + rec_end + POST + 5, ]
  tv <- as.numeric(x$POSIX.time - cc, units = "secs")          # verbatim
  d_n2 <- bind_rows(
    tibble(t = tv - off, val = x$N2Odry_ppb, gas = "N2O", src = "LI-7820 adjusted"),
    tibble(t = tv,       val = x$N2Odry_ppb, gas = "N2O", src = "LI-7820 verbatim"),
    tibble(t = tv - off, val = x$H2O_ppm / 1000, gas = "H2O", src = "LI-7820 adjusted"),
    tibble(t = tv,       val = x$H2O_ppm / 1000, gas = "H2O", src = "LI-7820 verbatim"))
  d <- bind_rows(d_ch, d_n2) %>% filter(t >= xr[1], t <= xr[2], !is.na(val)) %>%
    mutate(gas = factor(gas, levels = names(gas_lab), labels = gas_lab),
           src = factor(src, levels = names(src_col)))
  brk <- seq(-120, ceiling(xr[2] / 60) * 60, by = 60)
  lab_bottom <- function(b) paste0(b, "\n", format(cc + b, "%H:%M:%S", tz = TZ))
  lab_top <- function(b) format(cc + off + b, "%H:%M:%S", tz = TZ)
  f <- function(v, dg = 2) ifelse(is.na(v), "NA", formatC(v, format = "f", digits = dg))
  sub <- sprintf("CO2 %s µmol (%s) | CH4 %s nmol (%s)%s | N2O %s nmol (%s)%s, MDF %s",
                 f(cl$CO2_flux), cl$CO2_model, f(cl$CH4_flux, 3), cl$CH4_model,
                 ifelse(isTRUE(cl$CH4_below_MDF), " <MDF", ""), f(cl$N2O_flux, 3), cl$N2O_model,
                 ifelse(isTRUE(cl$N2O_below_MDF), " <MDF", ""), f(cl$N2O_MDF, 3))
  notes <- paste(na.omit(c(if (!is.na(cl$N2O_qc_note) && nzchar(cl$N2O_qc_note)) paste("N2O QC:", cl$N2O_qc_note),
                           if (!is.na(cl$remark) && nzchar(cl$remark)) paste("remark:", cl$remark))), collapse = " | ")
  ggplot(d, aes(t, val, colour = src)) +
    annotate("rect", xmin = win_s, xmax = as.numeric(cl$end.time - cc, units = "secs"),
             ymin = -Inf, ymax = Inf, fill = "grey88") +
    geom_vline(xintercept = c(0, rec_end), linetype = 2, linewidth = 0.25, colour = "grey40") +
    geom_line(linewidth = 0.3) +
    facet_grid(gas ~ ., scales = "free_y", labeller = label_parsed) +
    scale_colour_manual(values = src_col, drop = FALSE, name = NULL) +
    scale_x_continuous(limits = xr, breaks = brk, labels = lab_bottom, expand = c(0, 0),
                       sec.axis = dup_axis(labels = lab_top, name = sprintf("LI-7820 clock (offset %+d s)", off))) +
    labs(x = "chamber clock (GPS): s from closure / time", y = NULL,
         title = sprintf("%s   plot %d%s  %s   [%s]", cl$chamID, cl$plot, cl$collar, cl$treatment, cl$status),
         subtitle = paste0(sub, if (nzchar(notes)) paste0("\n", notes) else "")) +
    theme_bw(base_size = 7) +
    theme(plot.title = element_text(size = 7.5, face = "bold", colour = trt_col[cl$treatment]),
          plot.subtitle = element_text(size = 6), legend.position = "bottom",
          legend.text = element_text(size = 7), strip.text.y = element_text(size = 7),
          panel.grid.minor = element_blank(), axis.text = element_text(size = 6))
}

out_dir <- "output/qc/closure_traces"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
tmp_dir <- tempfile("traces"); dir.create(tmp_dir)   # render locally (writing into Google Drive page by page is very slow)
per_page <- 3
render <- function(idx, file) {
  cairo_pdf(file, width = 11, height = 8.5, onefile = TRUE)
  for (p in split(idx, ceiling(seq_along(idx) / per_page))) {
    pl <- lapply(p, function(k) tryCatch(plot_closure(k), error = function(e) {
      message("  failed ", cl_all$chamID[k], ": ", conditionMessage(e)); NULL }))
    pl <- Filter(Negate(is.null), pl)
    while (length(pl) < per_page) pl <- c(pl, list(plot_spacer()))
    print(wrap_plots(pl, nrow = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom"))
  }
  invisible(dev.off())
}
days <- as.character(sort(unique(cl_all$date_name)))
files <- parallel::mclapply(days, function(d) {
  f <- file.path(tmp_dir, sprintf("closure_traces_%s.pdf", d))
  render(which(as.character(cl_all$date_name) == d), f); f
}, mc.cores = max(1, min(length(days), parallel::detectCores() - 1)))
files <- unlist(files)
# combined file: concatenate the per-day PDFs if qpdf is available, else render again
all_f <- file.path(tmp_dir, "closure_traces_all.pdf")
if (requireNamespace("qpdf", quietly = TRUE)) qpdf::pdf_combine(files, all_f) else render(seq_len(nrow(cl_all)), all_f)
invisible(file.copy(c(files, all_f), out_dir, overwrite = TRUE))
cat(sprintf("Wrote %d closure plots (%d field days) to %s/\n", nrow(cl_all), length(days), out_dir))
