# fig_setup.R
# Shared palette, theme, export and summary helpers for all manuscript figures.
# Sourced by 30_main_figures.R and 31_si_figures.R.

invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))  # Rscript defaults to "C", which mangles ± − etc.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# --- Treatment encoding ------------------------------------------------------
# Colour carries meaning: control = neutral grey (no amendment), compost =
# earthy brown-orange (solid organic amendment), slurry = blue (liquid).
# Checked with colorspace CVD simulations: min CIEDE2000 between any pair is
# >= 24 under deuteranopia, protanopia and tritanopia. The three share similar
# lightness, so every treatment also gets its own marker shape (grayscale/print).
TRT_LEVELS <- c("control", "compost", "slurry")
TRT_LABELS <- c(control = "Control", compost = "Compost", slurry = "Slurry")
TRT_COLS   <- c(control = "#8C8C8C", compost = "#B35806", slurry = "#2166AC")
TRT_SHAPES <- c(control = 21, compost = 24, slurry = 22)   # filled-able outlines

APPLICATION_DATE <- as.Date("2025-05-28")
ROUND_DATES  <- c(`1` = "2025-05-29", `2` = "2025-07-21", `3` = "2025-10-14")
ROUND_LABELS <- c(`1` = "29 May", `2` = "21 Jul", `3` = "14 Oct")

INK   <- "grey15"
MUTED <- "grey45"
RULE  <- "grey80"

# One fixed legend key for every panel, so patchwork collects a single legend
trt_guide <- function() guide_legend(override.aes = list(size = 2, stroke = 0.4, alpha = 1))
scale_colour_trt <- function(guide = trt_guide(), ...) scale_colour_manual(values = TRT_COLS, labels = TRT_LABELS, name = NULL, guide = guide, ...)
scale_fill_trt   <- function(guide = trt_guide(), ...) scale_fill_manual(values = TRT_COLS, labels = TRT_LABELS, name = NULL, guide = guide, ...)
scale_shape_trt  <- function(guide = trt_guide(), ...) scale_shape_manual(values = TRT_SHAPES, labels = TRT_LABELS, name = NULL, guide = guide, ...)

as_trt <- function(x) factor(x, levels = TRT_LEVELS)

# --- Theme -------------------------------------------------------------------
# Sized for print: 7-8 pt text at final size (180 mm double / 88 mm single column).
theme_pub <- function(base_size = 7) {
  theme_classic(base_size = base_size, base_family = "Helvetica") %+replace%
    theme(
      text = element_text(colour = INK),
      axis.text = element_text(colour = INK, size = rel(0.9)),
      axis.title = element_text(size = rel(1)),
      axis.line = element_line(colour = INK, linewidth = 0.3),
      axis.ticks = element_line(colour = INK, linewidth = 0.3),
      axis.ticks.length = unit(1.5, "pt"),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.25),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = rel(1), hjust = 0,
                                margin = margin(0, 0, 3, 0)),
      legend.position = "bottom",
      legend.key.size = unit(8, "pt"),
      legend.text = element_text(size = rel(1)),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(3, "pt"),
      plot.title = element_text(face = "bold", size = rel(1), hjust = 0,
                                margin = margin(0, 0, 4, 0)),
      plot.subtitle = element_text(colour = MUTED, size = rel(0.93), hjust = 0,
                                   margin = margin(0, 0, 4, 0)),
      plot.tag = element_text(face = "bold", size = rel(1.3), hjust = 0, vjust = 1),
      plot.margin = margin(4, 6, 4, 4)
    )
}
theme_set(theme_pub())

# Panel tags a, b, c... (tag styling comes from theme_pub's plot.tag)
tags_pub <- function() plot_annotation(tag_levels = "a")

# --- Export ------------------------------------------------------------------
FIG_DIR <- "output/figures"
save_fig <- function(p, name, width_mm, height_mm, subdir = "main") {
  d <- file.path(FIG_DIR, subdir)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(d, paste0(name, ".pdf")), p, width = width_mm, height = height_mm,
         units = "mm", device = cairo_pdf)
  ggsave(file.path(d, paste0(name, ".png")), p, width = width_mm, height = height_mm,
         units = "mm", dpi = 300, device = ragg::agg_png, bg = "white")
  cat(sprintf("  saved %s/%s (.pdf, .png) %d x %d mm\n", subdir, name, width_mm, height_mm))
}

# --- Summaries ---------------------------------------------------------------
# Plots are the experimental unit (n = 5 per treatment). Always average
# subsamples (collars, tubes) within plot before summarising.
trt_summary <- function(df, value, ...) {
  df %>%
    group_by(treatment, ...) %>%
    summarize(n = sum(!is.na({{ value }})),
              mean = mean({{ value }}, na.rm = TRUE),
              se = sd({{ value }}, na.rm = TRUE) / sqrt(n),
              lo = mean - qt(0.975, pmax(n - 1, 1)) * se,
              hi = mean + qt(0.975, pmax(n - 1, 1)) * se,
              .groups = "drop") %>%
    mutate(treatment = as_trt(treatment))
}

# Effect of each amendment relative to control within each group:
# difference in means with Welch 95% CI, the same as % of the control mean, and
# Hedges' g (pooled-SD standardized difference, small-sample corrected) with 95% CI.
diff_vs_control <- function(df, value, ...) {
  df %>%
    group_by(...) %>%
    group_modify(function(d, k) {
      ctl <- d %>% filter(treatment == "control") %>% pull({{ value }}) %>% na.omit()
      bind_rows(lapply(c("compost", "slurry"), function(tr) {
        x <- d %>% filter(treatment == tr) %>% pull({{ value }}) %>% na.omit()
        if (length(x) < 2 || length(ctl) < 2) return(NULL)
        tt <- t.test(x, ctl)
        n1 <- length(x); n2 <- length(ctl)
        sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(ctl)) / (n1 + n2 - 2))
        g <- (mean(x) - mean(ctl)) / sp * (1 - 3 / (4 * (n1 + n2) - 9))   # Hedges' g
        se_g <- sqrt((n1 + n2) / (n1 * n2) + g^2 / (2 * (n1 + n2)))
        tibble(treatment = tr, diff = mean(x) - mean(ctl),
               lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value,
               control_mean = mean(ctl),
               hedges_g = g, g_lo = g - 1.96 * se_g, g_hi = g + 1.96 * se_g)
      }))
    }) %>%
    ungroup() %>%
    mutate(pct = 100 * diff / control_mean, pct_lo = 100 * lo / control_mean,
           pct_hi = 100 * hi / control_mean, treatment = as_trt(treatment))
}

anova_p <- function(df, value, ...) {
  df %>% group_by(...) %>%
    summarize(p = tryCatch(summary(aov({{ value }} ~ treatment))[[1]][1, "Pr(>F)"],
                           error = function(e) NA_real_), .groups = "drop")
}

# --- Reusable layers -----------------------------------------------------------
# Plot-level points + treatment mean with 95% CI, dodged by treatment.
dot_ci_layers <- function(plot_df, summ_df, x, y, dodge = 0.55, pt_size = 1.1, mean_size = 2.2) {
  pd <- position_dodge(width = dodge)
  pj <- position_jitterdodge(jitter.width = 0.12, dodge.width = dodge, seed = 1)
  list(
    geom_point(data = plot_df, aes(x = {{ x }}, y = {{ y }}, colour = treatment),
               position = pj, size = pt_size, alpha = 0.45, shape = 16, show.legend = FALSE),
    geom_linerange(data = summ_df, aes(x = {{ x }}, ymin = lo, ymax = hi, colour = treatment),
                   position = pd, linewidth = 0.45, show.legend = FALSE),
    geom_point(data = summ_df, aes(x = {{ x }}, y = mean, colour = treatment,
                                   fill = treatment, shape = treatment),
               position = pd, size = mean_size, stroke = 0.4),
    scale_colour_trt(), scale_fill_trt(), scale_shape_trt()
  )
}

zero_line <- function() geom_hline(yintercept = 0, colour = MUTED, linewidth = 0.3, linetype = "22")
application_line <- function() geom_vline(xintercept = APPLICATION_DATE, colour = MUTED,
                                          linewidth = 0.3, linetype = "22")

# Category axis for treatment-only panels: colour + shape + legend identify the
# treatment, so the crowded x labels are dropped.
trt_axis <- function() list(
  scale_x_discrete(labels = TRT_LABELS),
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
)
