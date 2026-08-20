suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(patchwork)
  library(metafor)
  library(scales)
  library(grid)
})

root_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
dir_create <- function(...) dir.create(file.path(root_dir, ...), recursive = TRUE, showWarnings = FALSE)
invisible(lapply(
  list(
    c("data", "processed"), c("data", "source_data"),
    c("figures", "main"), c("figures", "extended"),
    c("analysis_logs")
  ),
  function(x) do.call(dir_create, as.list(x))
))

stage_levels <- c("EMBRYO", "LARVA", "SETTLEMENT/METAMORPHOSIS", "JUVENILE/ADULT")
stage_labels <- c(
  "EMBRYO" = "Embryo",
  "LARVA" = "Larva",
  "SETTLEMENT/METAMORPHOSIS" = "Settlement\n/metamorphosis",
  "JUVENILE/ADULT" = "Juvenile\n/adult"
)
# Single-line variants for axes/legends where vertical space is tight.
stage_labels_1l <- c(
  "EMBRYO" = "Embryo",
  "LARVA" = "Larva",
  "SETTLEMENT/METAMORPHOSIS" = "Settlement/metamorphosis",
  "JUVENILE/ADULT" = "Juvenile/adult"
)
stressor_levels <- c("Acidification", "Warming", "Warming + acidification")
mode_levels <- c("With metamorphic stage", "Without metamorphic stage", "Mixed/unknown")

# --- Colour system -----------------------------------------------------------
# Restrained, colourblind-safe editorial palette. Saturated colour is reserved
# for scientific grouping; neutral greys carry scaffolding and diagnostics.
pal <- c(
  blue = "#2477A8",
  blue_mid = "#72A9C8",
  blue_light = "#DCEAF2",
  red = "#C95D3A",
  red_mid = "#E19A7D",
  red_light = "#F3DDD5",
  green = "#2A8C72",
  purple = "#A96F9A",
  amber = "#D99A21",
  black = "#222426",
  grey_dark = "#555B61",
  grey = "#858A8F",
  grey_light = "#D5D8DA",
  grey_pale = "#F2F3F4",
  no_evidence = "#ECEEEF",
  white = "#FFFFFF"
)
col <- function(name) unname(pal[[name]])

# Drivers: three qualitatively distinct Okabe-Ito hues.
driver_pal <- c(
  "Acidification" = col("blue"),
  "Warming" = col("red"),
  "Warming + acidification" = col("purple"),
  "All stressors" = col("grey_dark")
)
mode_pal <- c(
  "With metamorphic stage" = col("blue"),
  "Without metamorphic stage" = col("amber"),
  "Mixed/unknown" = col("grey")
)
# Ordinal stages: muted dark-to-light progression, reused wherever stage is the
# grouping variable so the visual vocabulary stays stable across figures.
stage_pal <- c(
  "EMBRYO" = "#4B3F72",
  "LARVA" = "#3F708F",
  "SETTLEMENT/METAMORPHOSIS" = "#2F907E",
  "JUVENILE/ADULT" = "#86AA4E"
)
# Estimands: three CVD-safe accent hues, one per scientific question.
estimand_pal <- c(
  "Directional lnRR" = col("red"),
  "Endpoint-aligned performance lnRR" = col("green"),
  "Absolute |lnRR|" = col("blue")
)
# Endpoint-direction composition (Fig 1d).
direction_pal <- c(
  "Direction-interpretable" = col("blue"),
  "Context-dependent" = col("amber")
)
# Estimand-specific inference decision matrix.
inference_pal <- c(
  "Directional and absolute" = col("red"),
  "Absolute only" = col("blue"),
  "Directional only" = col("green"),
  "Neither" = col("grey_light")
)

# Journal figure geometry (Environmental Research / Elsevier).
# Double-column max width = 190 mm; single column = 90 mm. Base font 7 pt keeps
# text >= 6 pt at final printed size.
er_base_size <- 7.2
theme_publication <- function(base_size = er_base_size, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.34, colour = "black"),
      axis.ticks = element_line(linewidth = 0.34, colour = "black"),
      axis.ticks.length = unit(1.3, "mm"),
      axis.title = element_text(size = base_size, colour = "black"),
      axis.text = element_text(size = base_size - 0.6, colour = "black"),
      legend.title = element_text(size = base_size - 0.4, face = "bold"),
      legend.text = element_text(size = base_size - 0.6),
      legend.key.height = unit(3.2, "mm"),
      legend.key.width = unit(4.4, "mm"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.margin = margin(1, 1, 1, 1),
      strip.background = element_blank(),
      strip.text = element_text(size = base_size - 0.4, face = "bold"),
      plot.title = element_text(size = base_size + 0.6, face = "bold", colour = "black",
                                margin = margin(b = 2.5)),
      plot.subtitle = element_text(size = base_size - 0.6, colour = col("grey_dark"),
                                   margin = margin(b = 2)),
      plot.tag = element_text(size = 8, face = "bold", family = "Arial"),
      plot.tag.position = c(0.004, 0.996),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 5, 4, 5)
    )
}
theme_set(theme_publication())

# Faint x-grid for forest/estimate panels only (aids reading against the CI axis).
grid_x <- theme(
  panel.grid.major.x = element_line(linewidth = 0.22, colour = col("grey_pale"))
)

save_figure <- function(plot, file_base, width_mm = 190, height_mm = 125, dpi = 600) {
  file_base <- file.path(root_dir, file_base)
  w <- width_mm / 25.4
  h <- height_mm / 25.4
  svglite::svglite(paste0(file_base, ".svg"), width = w, height = h, bg = "white")
  print(plot)
  dev.off()
  grDevices::cairo_pdf(paste0(file_base, ".pdf"), width = w, height = h, family = "Arial", bg = "white")
  print(plot)
  dev.off()
  ragg::agg_tiff(paste0(file_base, ".tiff"), width = w, height = h, units = "in",
                 res = dpi, background = "white", compression = "lzw")
  print(plot)
  dev.off()
  ragg::agg_png(paste0(file_base, ".png"), width = w, height = h, units = "in",
                res = dpi, background = "white")
  print(plot)
  dev.off()
  invisible(file_base)
}

write_csv_safe <- function(x, path) {
  readr::write_csv(x, file.path(root_dir, path), na = "")
  invisible(x)
}

fmt <- function(x, digits = 3) ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))
p_fmt <- function(x) ifelse(is.na(x), "NA", ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format = "f")))
q_fmt <- function(x) {
  val <- p_fmt(as.numeric(x))
  ifelse(is.na(val), "q = NA", ifelse(str_detect(val, "^<"), paste0("q ", str_replace(val, "^<", "< ")), paste0("q = ", val)))
}
clean_chr <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, fixed("\u00A0"), " ")
  x <- str_replace_all(x, c(
    "P枚rtner" = "Poertner",
    "Tr眉benbach" = "Truebenbach",
    "M眉ller" = "Mueller",
    "B眉schner" = "Bueschner",
    "L贸pez" = "Lopez",
    "Sch枚nberg" = "Schoenberg",
    "Sk枚ld" = "Skoeld",
    "Ekl枚f" = "Ekloef",
    "O鈥機onnor" = "O'Connor",
    "O鈥" = "O'",
    "機onnor" = "Connor",
    "聽" = " "
  ))
  x <- str_replace_all(x, regex("O鈧\\?\\s*consumption|O\\?\\s*consumption|O2\\s*consumption"), "O2 consumption")
  x <- str_replace_all(x, fixed("current retained model row"), "current retained analysis row")
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  x
}

# ---------------------------------------------------------------------------
# Reusable meta-analysis figure helpers (shared across main + supplementary).
# ---------------------------------------------------------------------------

# Heterogeneity label, e.g. "I^2 = 99.4%; tau^2 = 0.08". Accepts pre-computed
# values; NA components are dropped so the annotation degrades gracefully.
het_label <- function(i2 = NA, tau2 = NA) {
  parts <- character(0)
  if (!is.na(i2))   parts <- c(parts, sprintf("italic(I)^2 == '%s%%'", formatC(i2, 1, format = "f")))
  if (!is.na(tau2)) parts <- c(parts, sprintf("tau^2 == '%s'", formatC(tau2, 3, format = "f")))
  if (!length(parts)) return(NA_character_)
  paste(parts, collapse = "*'; '*")
}

# "Favours ..." directional annotation strip drawn just below a forest panel.
# Returns a list of ggplot layers positioned in data coordinates.
favours_layer <- function(y, x_left, x_right, left_lab, right_lab,
                          size = er_base_size - 1.4, colour = col("grey_dark")) {
  mid <- 0
  list(
    annotate("segment", x = mid, xend = x_left, y = y, yend = y,
             arrow = arrow(length = unit(1.3, "mm"), type = "closed"),
             linewidth = 0.3, colour = colour),
    annotate("segment", x = mid, xend = x_right, y = y, yend = y,
             arrow = arrow(length = unit(1.3, "mm"), type = "closed"),
             linewidth = 0.3, colour = colour),
    annotate("text", x = x_left, y = y, label = left_lab, hjust = 0, vjust = -0.6,
             size = size / .pt, colour = colour),
    annotate("text", x = x_right, y = y, label = right_lab, hjust = 1, vjust = -0.6,
             size = size / .pt, colour = colour)
  )
}

# Contour-enhanced funnel layers: shaded p-value significance regions around a
# reference (pooled) estimate, plus the pseudo-95% CI funnel lines. `se_max`
# sets the vertical extent. Colours follow the standard grey significance ramp.
# Contour-enhanced funnel layers (Peters et al. 2008 convention).
# Significance contours are centred on the NULL (0): a study is significant when
# |estimate| > z * SE, so the bands widen with SE away from zero. Each band is
# drawn as TWO separate wedges (left / right) so the polygon never fills across
# the centre. The pseudo-95% CI funnel and the solid reference line are centred
# on the pooled estimate `ref`.
funnel_contours <- function(ref, se_max) {
  se <- seq(0, se_max, length.out = 120)
  wedge <- function(z1, z2, fill, side) {
    s <- if (side == "L") -1 else 1
    data.frame(
      x = c(s * z1 * se, rev(s * z2 * se)),
      y = c(se, rev(se)),
      fill = fill,
      grp = paste(fill, side)
    )
  }
  band <- function(z1, z2, fill) rbind(wedge(z1, z2, fill, "L"),
                                       wedge(z1, z2, fill, "R"))
  # 0.10 > p > 0.05 (z 1.64-1.96); 0.05 > p > 0.01 (1.96-2.58); p < 0.01 (>2.58)
  regions <- rbind(
    band(1.6449, 1.9600, "0.05 < p < 0.10"),
    band(1.9600, 2.5758, "0.01 < p < 0.05"),
    band(2.5758, 8.0000, "p < 0.01")
  )
  regions$fill <- factor(regions$fill,
                         levels = c("0.05 < p < 0.10", "0.01 < p < 0.05", "p < 0.01"))
  ci <- data.frame(se = se, lo = ref - 1.96 * se, hi = ref + 1.96 * se)
  list(
    geom_polygon(data = regions, aes(x = x, y = y, group = grp, fill = fill),
                 inherit.aes = FALSE, colour = NA),
    scale_fill_manual(
      values = c("0.05 < p < 0.10" = "#E6E6E6",
                 "0.01 < p < 0.05" = "#CFCFCF",
                 "p < 0.01" = "#B4B4B4"),
      name = "Significance\ncontour (vs 0)", drop = FALSE
    ),
    geom_line(data = ci, aes(x = lo, y = se), inherit.aes = FALSE,
              linewidth = 0.3, linetype = "dashed", colour = col("grey_dark")),
    geom_line(data = ci, aes(x = hi, y = se), inherit.aes = FALSE,
              linewidth = 0.3, linetype = "dashed", colour = col("grey_dark")),
    geom_vline(xintercept = ref, linewidth = 0.34, colour = col("black"))
  )
}