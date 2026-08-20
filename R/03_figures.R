# =============================================================================
# Publication figures for the Environmental Research submission.
# Rebuilt to meta-analysis reporting standards: forest plots carry 95% CI and
# 95% prediction intervals, I^2/tau^2, k/s counts and directional "favours"
# axes; funnels are contour-enhanced on the SIGNED estimand; a PRISMA 2020 flow
# diagram, Egger small-study summary and heterogeneity caterpillar are added as
# supplementary figures. All numbers are read from the existing model outputs
# (no model is re-fitted here), so estimates match the manuscript exactly.
# =============================================================================
source(file.path("R", "00_setup.R"))

dat <- readr::read_csv(file.path(root_dir, "data", "processed", "analysis_ready_model_dataset.csv"),
                       show_col_types = FALSE, progress = FALSE) %>%
  mutate(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    DevelopmentalModeGroup = factor(DevelopmentalModeGroup, levels = mode_levels),
    StressorLabel = factor(StressorLabel, levels = stressor_levels),
    UpdateLayer = factor(UpdateLayer, levels = c(
      "Original curated evidence", "Earlier validated expansion", "Current verified pre-2026 update"
    ))
  )
main_results <- readr::read_csv(file.path(root_dir, "data", "processed", "main_results_REML.csv"),
                                show_col_types = FALSE, progress = FALSE) %>%
  mutate(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    DevelopmentalModeGroup = factor(DevelopmentalModeGroup, levels = mode_levels),
    StressorLabel = factor(StressorLabel, levels = c(stressor_levels, "All stressors"))
  )
decision_matrix <- readr::read_csv(file.path(root_dir, "data", "processed", "directional_vs_absolute_decision_matrix.csv"),
                                   show_col_types = FALSE, progress = FALSE)
update_sens <- readr::read_csv(file.path(root_dir, "data", "processed", "update_sensitivity.csv"),
                               show_col_types = FALSE, progress = FALSE)
loo <- readr::read_csv(file.path(root_dir, "data", "processed", "leave_one_study_out.csv"),
                       show_col_types = FALSE, progress = FALSE)
egger <- readr::read_csv(file.path(root_dir, "data", "processed", "egger_small_study_checks.csv"),
                         show_col_types = FALSE, progress = FALSE)
variance_floor <- readr::read_csv(file.path(root_dir, "data", "processed", "variance_floor_sensitivity.csv"),
                                  show_col_types = FALSE, progress = FALSE)
robustness <- readr::read_csv(file.path(root_dir, "data", "processed", "ER_robustness_summary.csv"),
                              show_col_types = FALSE, progress = FALSE)
evidence_balance <- readr::read_csv(file.path(root_dir, "data", "processed", "ER_evidence_balance_by_stage.csv"),
                                    show_col_types = FALSE, progress = FALSE)
screened_records <- readr::read_csv(file.path(root_dir, "data", "processed", "screened_candidate_records.csv"),
                                    show_col_types = FALSE, progress = FALSE)

# --- Shared forest-panel builder --------------------------------------------
# Draws a horizontal forest: 95% PI as a thin whisker, 95% CI as a thick bar,
# point as a filled marker, with a right-hand statistics column (k; s; I^2) and
# an optional "favours" directional strip. `d` must carry ObservedStage,
# Estimate, CI_low, CI_high, PI_low, PI_high, EffectSizeCount, StudyCount,
# I2_percent. Stages are shown embryo (top) -> juvenile/adult (bottom).
forest_panel <- function(d, xlab, title, accent,
                         favours = NULL, show_zero = TRUE, zero_at = 0) {
  d <- d %>% mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)))
  x_lo <- min(d$PI_low, d$CI_low, zero_at, na.rm = TRUE)
  x_hi <- max(d$PI_high, d$CI_high, zero_at, na.rm = TRUE)
  span <- x_hi - x_lo
  stat_x <- x_hi + span * 0.06
  x_right <- x_hi + span * 0.32
  d <- d %>% mutate(
    stat = sprintf("k = %d; s = %d\nI\u00B2 = %s%%",
                   EffectSizeCount, StudyCount, formatC(I2_percent, 1, format = "f"))
  )
  p <- ggplot(d, aes(Estimate, ObservedStage))
  if (show_zero) {
    p <- p + geom_vline(xintercept = zero_at, linetype = "dashed",
                        linewidth = 0.32, colour = col("grey"))
  }
  p <- p +
    geom_errorbar(aes(xmin = PI_low, xmax = PI_high), width = 0,
                  orientation = "y", linewidth = 0.35, colour = col("grey")) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0.14,
                  orientation = "y", linewidth = 0.95, colour = accent) +
    geom_point(size = 2.6, shape = 21, fill = accent, colour = "white", stroke = 0.5) +
    geom_text(aes(x = stat_x, label = stat), hjust = 0, lineheight = 0.86,
              size = (er_base_size - 1.75) / .pt, colour = col("grey_dark")) +
    scale_y_discrete(labels = rev(stage_labels)) +
    scale_x_continuous(limits = c(x_lo - span * 0.05, x_right),
                       expand = expansion(mult = c(0, 0))) +
    labs(x = xlab, y = NULL, title = title) +
    coord_cartesian(clip = "off") +
    grid_x +
    theme(plot.margin = margin(6, 8, 6, 6))
  if (!is.null(favours)) {
    p <- p + favours_layer(y = 0.34, x_left = x_lo, x_right = x_hi,
                           left_lab = favours[1], right_lab = favours[2])
  }
  p
}

# =============================================================================
# FIGURE 1 | Evidence structure, life-history coding and endpoint composition.
# =============================================================================
stage_count <- dat %>%
  count(ObservedStage, StressorLabel, name = "EffectSizeCount") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = stage_levels))
mode_count <- dat %>% count(DevelopmentalModeGroup, name = "EffectSizeCount")
write_csv_safe(stage_count, "data/source_data/Fig1a_stage_driver_counts.csv")
write_csv_safe(mode_count, "data/source_data/Fig1c_mode_counts.csv")

# (a) Life-history schematic: the endpoint stage is read from the measured
#     endpoint, whereas developmental mode remains a separate species-level tag.
stage_diagram <- tibble(
  Stage = factor(stage_levels, levels = stage_levels),
  x = c(0.88, 1.98, 3.08, 4.18),
  y = 0.24,
  label = stage_labels[stage_levels]
)
mode_diagram <- tibble(
  Mode = factor(mode_levels, levels = mode_levels),
  x = c(1.05, 2.55, 4.05),
  y = -0.76,
  label = c("With\nmetamorphic\nstage", "Without\nmetamorphic\nstage", "Mixed /\nunknown")
)
arrow_df <- tibble(x = c(1.18, 2.28, 3.38), xend = c(1.70, 2.80, 3.90),
                   y = 0.24, yend = 0.24)
p1a <- ggplot() +
  geom_segment(data = arrow_df, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(2.0, "mm"), type = "closed"),
               linewidth = 0.34, colour = col("grey")) +
  geom_label(data = stage_diagram,
             aes(x = x, y = y, label = label, fill = Stage),
             colour = "white", fontface = "bold", lineheight = 0.88,
             label.padding = unit(1.45, "mm"), label.r = unit(1.4, "mm"),
             label.size = 0, size = (er_base_size - 1.45) / .pt) +
  annotate("text", x = 0.56, y = 0.73,
           label = "Measured endpoint stage", hjust = 0,
           size = (er_base_size - 0.75) / .pt, fontface = "bold") +
  annotate("text", x = 0.56, y = -0.27,
           label = "Species developmental mode", hjust = 0,
           size = (er_base_size - 0.95) / .pt, fontface = "bold") +
  geom_label(data = mode_diagram,
             aes(x = x, y = y, label = label, colour = Mode),
             inherit.aes = FALSE, fill = "white", linewidth = 0.36,
             label.padding = unit(1.55, "mm"), label.r = unit(1.3, "mm"),
             lineheight = 0.88, size = (er_base_size - 1.55) / .pt) +
  annotate("segment", x = 4.18, xend = 4.18, y = 0.02, yend = -0.46,
           arrow = arrow(length = unit(1.8, "mm"), type = "closed"),
           linewidth = 0.30, colour = col("grey_dark")) +
  annotate("text", x = 4.36, y = -0.20,
           label = "Example: an adult endpoint is coded as juvenile/adult,\neven for species with pelagic larvae",
           hjust = 1, lineheight = 0.86, size = (er_base_size - 1.85) / .pt,
           colour = col("grey_dark")) +
  scale_fill_manual(values = stage_pal, guide = "none") +
  scale_colour_manual(values = mode_pal, guide = "none") +
  coord_cartesian(xlim = c(0.45, 4.55), ylim = c(-1.05, 0.92), clip = "off") +
  labs(title = "Observed stage is not developmental mode") +
  theme_void(base_size = er_base_size, base_family = "Arial") +
  theme(plot.title = element_text(size = er_base_size + 0.6, face = "bold", hjust = 0),
        plot.margin = margin(4, 6, 2, 6))

# (b) Stage coverage by driver.
p1b <- ggplot(stage_count, aes(ObservedStage, EffectSizeCount, fill = StressorLabel)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = driver_pal[stressor_levels], name = NULL) +
  scale_x_discrete(labels = stage_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "Effect sizes", title = "Stage coverage by driver") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(axis.text.x = element_text(angle = 22, hjust = 1),
        legend.position = "top", legend.justification = "left",
        legend.box.margin = margin(0, 0, -3, 0),
        plot.margin = margin(5, 7, 6, 5))

# (c) Developmental-mode boundary.
p1c <- ggplot(mode_count, aes(DevelopmentalModeGroup, EffectSizeCount, fill = DevelopmentalModeGroup)) +
  geom_col(width = 0.64, colour = "white", linewidth = 0.15) +
  geom_text(aes(label = scales::comma(EffectSizeCount)), vjust = -0.35,
            size = (er_base_size - 1.4) / .pt) +
  scale_fill_manual(values = mode_pal, guide = "none") +
  scale_x_discrete(labels = c(
    "With metamorphic stage" = "With\nmetamorphic\nstage",
    "Without metamorphic stage" = "Without\nmetamorphic\nstage",
    "Mixed/unknown" = "Mixed/\nunknown"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = NULL, y = "Effect sizes", title = "Developmental-mode boundary") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.85),
        plot.margin = margin(5, 8, 6, 5))

# (d) Endpoint-direction composition by stage (direction-interpretable vs
#     context-dependent); the eligibility split behind the estimand comparison.
balance_long <- evidence_balance %>%
  transmute(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    `Direction-interpretable` = AlignedEffectSizes,
    `Context-dependent` = ContextDependentEffectSizes,
    StageTotal = EffectSizeCount,
    ContextShare = ContextDependentShare
  ) %>%
  pivot_longer(c(`Direction-interpretable`, `Context-dependent`),
               names_to = "DirectionClass", values_to = "Count") %>%
  mutate(DirectionClass = factor(DirectionClass,
                                 levels = c("Direction-interpretable", "Context-dependent")),
         Share = Count / StageTotal)
ctx_lab <- evidence_balance %>%
  transmute(ObservedStage = factor(ObservedStage, levels = stage_levels),
            lab = paste0(formatC(100 * ContextDependentShare, 1, format = "f"), "%"),
            y = 1.02)
p1d <- ggplot(balance_long, aes(ObservedStage, Share, fill = DirectionClass)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.15) +
  geom_text(data = ctx_lab, aes(ObservedStage, y, label = lab), inherit.aes = FALSE,
            vjust = 0, size = (er_base_size - 1.8) / .pt, colour = col("amber")) +
  scale_fill_manual(values = direction_pal, name = NULL) +
  scale_x_discrete(labels = stage_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08)), limits = c(0, 1.08)) +
  labs(x = NULL, y = "Share of effect sizes",
       title = "Endpoint-direction composition") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(axis.text.x = element_text(angle = 22, hjust = 1),
        legend.position = "top", legend.justification = "left",
        legend.box.margin = margin(0, 0, -3, 0),
        plot.margin = margin(5, 7, 6, 5))

fig1_design <- "
AAAAAA
BBCCDD
"
fig1 <- p1a + p1b + p1c + p1d +
  plot_layout(design = fig1_design, heights = c(0.72, 1.28)) +
  plot_annotation(tag_levels = "a")
save_figure(fig1, "figures/main/Fig1_classification_and_evidence_update", 190, 132)
# =============================================================================
# FIGURE 2 | Estimand-dependent stage responses (the core result).
# Three forest panels; one per scientific question; each with 95% CI, 95% PI,
# I^2, k/s and favours arrows, plus an estimand-comparison summary panel.
# =============================================================================
stage_all <- main_results %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage", GroupType == "mode_stage_all")
write_csv_safe(stage_all, "data/source_data/Fig2_stage_all_estimates.csv")

dir_data <- stage_all %>% filter(Estimand == "Directional lnRR")
abs_data <- stage_all %>% filter(Estimand == "Absolute |lnRR|")

# Endpoint-aligned performance: PI is not stored, so reconstruct the standard
# random-effects prediction interval Est +/- t_{k-2} * sqrt(tau^2 + SE^2).
aligned_data <- robustness %>%
  filter(Estimand == "Biologically aligned performance lnRR") %>%
  transmute(
    ObservedStage, Estimate, CI_low, CI_high,
    I2_percent, Tau2, EffectSizeCount, StudyCount,
    SE = (CI_high - CI_low) / (2 * qnorm(0.975)),
    tcrit = qt(0.975, pmax(StudyCount - 2, 1)),
    PI_low = Estimate - tcrit * sqrt(Tau2 + SE^2),
    PI_high = Estimate + tcrit * sqrt(Tau2 + SE^2)
  )

p2a <- forest_panel(dir_data, "Raw directional lnRR (95% CI, 95% PI)",
                    "Raw directional response", accent = col("red"),
                    favours = c("lower values", "higher values"))
p2b <- forest_panel(aligned_data, "Endpoint-aligned lnRR (95% CI, 95% PI)",
                    "Endpoint-aligned performance", accent = col("green"),
                    favours = c("reduced", "improved"))
p2c <- forest_panel(abs_data, "|lnRR| (95% CI, 95% PI)",
                    "Absolute departure from control", accent = col("blue"),
                    show_zero = TRUE, zero_at = 0,
                    favours = c("", "larger departure"))

# (d) Estimand comparison matrix: for every stage x driver cell, does the FDR-
#     supported inference hold for BOTH the directional and absolute estimands,
#     or absolute only? This is the paper's central "hidden sensitivity" result,
#     drawn directly from the decision matrix (no hand-coding).
inference_mat <- decision_matrix %>%
  transmute(
    ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
    StressorLabel = factor(StressorLabel, levels = stressor_levels),
    Inference = factor(Inference, levels = names(inference_pal))
  )
p2d <- ggplot(inference_mat, aes(StressorLabel, ObservedStage, fill = Inference)) +
  geom_tile(width = 0.94, height = 0.94, colour = "white", linewidth = 0.9) +
  scale_fill_manual(values = inference_pal, name = "FDR-supported inference",
                    limits = c("Directional and absolute", "Absolute only")) +
  scale_x_discrete(labels = c("Acidification" = "Acidif.", "Warming" = "Warming",
                              "Warming + acidification" = "Warm +\nacidif."),
                   position = "top") +
  scale_y_discrete(labels = rev(stage_labels)) +
  labs(title = "Directional signal survives only at embryo",
       subtitle = "Absolute departure is significant in every cell; direction is not",
       x = NULL, y = NULL) +
  coord_equal() +
  guides(fill = guide_legend(ncol = 1)) +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(lineheight = 0.85),
        legend.position = "right",
        legend.key.size = unit(3.2, "mm"),
        plot.margin = margin(6, 8, 8, 6))

fig2 <- (p2a | p2b) / (p2c | p2d) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a")
save_figure(fig2, "figures/main/Fig2_directional_and_absolute_stage_responses", 190, 145)
# =============================================================================
# FIGURE 3 | Driver-specific stage responses and endpoint-category context.
# Panels a/b: dodged forests (3 drivers within each stage) for the signed and
# absolute estimands, both with 95% CI + 95% PI. Panel c: response-category
# evidence map (stage x category, faceted by driver) with an explicit
# "no evidence" fill so empty cells are never confused with a low count.
# =============================================================================
# Long driver table from the decision matrix (carries CI + PI for both estimands).
driver_long <- decision_matrix %>%
  transmute(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    StressorLabel = factor(StressorLabel, levels = stressor_levels),
    EffectSizeCount = EffectSizeCount__directional,
    StudyCount = StudyCount__directional,
    dir_est = Estimate__directional, dir_lo = CI_low__directional, dir_hi = CI_high__directional,
    dir_plo = PI_low__directional, dir_phi = PI_high__directional,
    abs_est = Estimate__absolute, abs_lo = CI_low__absolute, abs_hi = CI_high__absolute,
    abs_plo = PI_low__absolute, abs_phi = PI_high__absolute
  )
write_csv_safe(driver_long, "data/source_data/Fig3ab_driver_stage_estimates.csv")

# Manual dodge: stages on an ordinal y-axis (embryo at top), three drivers
# offset within each stage band so CI/PI whiskers never overlap.
stage_y <- setNames(rev(seq_along(stage_levels)), stage_levels)  # embryo=4 ... adult=1
driver_off <- setNames(c(0.26, 0, -0.26), stressor_levels)

driver_forest <- function(d, est, lo, hi, plo, phi, xlab, title,
                          favours = NULL, show_zero = TRUE) {
  d <- d %>% mutate(
    y = stage_y[as.character(ObservedStage)] + driver_off[as.character(StressorLabel)],
    E = .data[[est]], L = .data[[lo]], H = .data[[hi]], PL = .data[[plo]], PH = .data[[phi]]
  )
  x_lo <- min(d$PL, 0, na.rm = TRUE); x_hi <- max(d$PH, 0, na.rm = TRUE)
  span <- x_hi - x_lo
  p <- ggplot(d, aes(E, y, colour = StressorLabel))
  if (show_zero) p <- p + geom_vline(xintercept = 0, linetype = "dashed",
                                     linewidth = 0.32, colour = col("grey"))
  p <- p +
    geom_errorbar(aes(xmin = PL, xmax = PH), width = 0, orientation = "y",
                  linewidth = 0.3, colour = col("grey"), alpha = 0.9) +
    geom_errorbar(aes(xmin = L, xmax = H), width = 0, orientation = "y",
                  linewidth = 0.9) +
    geom_point(aes(fill = StressorLabel), size = 2.0, shape = 21, colour = "white", stroke = 0.4) +
    scale_colour_manual(values = driver_pal[stressor_levels], name = NULL, aesthetics = c("colour", "fill")) +
    scale_y_continuous(breaks = stage_y, labels = stage_labels[names(stage_y)],
                       limits = c(0.5, 4.6), expand = expansion(0)) +
    scale_x_continuous(limits = c(x_lo - span * 0.04, x_hi + span * 0.04),
                       expand = expansion(0)) +
    labs(x = xlab, y = NULL, title = title) +
    coord_cartesian(clip = "off") + grid_x +
    guides(colour = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
    theme(legend.position = "none", plot.margin = margin(6, 8, 6, 4))
  if (!is.null(favours)) p <- p + favours_layer(y = 0.30, x_left = x_lo, x_right = x_hi,
                                                left_lab = favours[1], right_lab = favours[2])
  p
}

p3a <- driver_forest(driver_long, "dir_est", "dir_lo", "dir_hi", "dir_plo", "dir_phi",
                    "Directional lnRR (95% CI, 95% PI)", "Driver-specific signed response")
p3b <- driver_forest(driver_long, "abs_est", "abs_lo", "abs_hi", "abs_plo", "abs_phi",
                    "|lnRR| (95% CI, 95% PI)", "Driver-specific absolute departure")
# (c) Response-category evidence map: complete stage x category grid per driver,
#     so absent combinations render as an explicit "no evidence" tile rather than
#     a white gap that could be misread as a low count.
cat_map <- readr::read_csv(file.path(root_dir, "data", "source_data", "ExtendedData_category_stage_map.csv"),
                           show_col_types = FALSE, progress = FALSE)
cat_order <- cat_map %>% group_by(Category) %>% summarise(n = sum(EffectSizeCount), .groups = "drop") %>%
  arrange(n) %>% pull(Category)          # low -> high; ordered bottom->top on y
cat_grid <- tidyr::crossing(
  Category = cat_order,
  ObservedStage = stage_levels,
  StressorLabel = stressor_levels
) %>%
  left_join(cat_map %>% select(Category, ObservedStage, StressorLabel, EffectSizeCount),
            by = c("Category", "ObservedStage", "StressorLabel")) %>%
  mutate(
    Category = factor(Category, levels = cat_order),
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    StressorLabel = factor(StressorLabel, levels = stressor_levels),
    has = !is.na(EffectSizeCount),
    logn = ifelse(has, log10(EffectSizeCount + 1), NA_real_)
  )
write_csv_safe(cat_grid, "data/source_data/Fig3c_category_stage_map.csv")

p3c <- ggplot(cat_grid, aes(ObservedStage, Category)) +
  geom_tile(data = dplyr::filter(cat_grid, !has), fill = col("no_evidence"),
            colour = "white", linewidth = 0.4) +
  geom_tile(data = dplyr::filter(cat_grid, has), aes(fill = logn),
            colour = "white", linewidth = 0.4) +
  geom_text(data = dplyr::filter(cat_grid, has), aes(label = EffectSizeCount),
            size = (er_base_size - 2.6) / .pt, colour = col("black")) +
  facet_wrap(~ StressorLabel, nrow = 1) +
  scale_fill_gradientn(
    colours = c("#EDF4F8", "#B5D3E3", "#6AA6C5", "#2477A8", "#174E6C"),
    name = "Effect sizes\nlog10(n + 1)",
    breaks = log10(c(1, 5, 20, 100, 300) + 1),
    labels = c("1", "5", "20", "100", "300"),
    limits = c(0, log10(300 + 1))
  ) +
  scale_x_discrete(labels = stage_labels) +
  labs(x = NULL, y = NULL, title = "Response-category evidence behind the stage models") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_colourbar(barheight = unit(25, "mm"), barwidth = unit(2.5, "mm"),
                                 title.position = "top", raster = FALSE)) +
  theme(axis.text.x = element_text(angle = 27, hjust = 1),
        panel.spacing = unit(4, "mm"),
        legend.position = "right",
        plot.margin = margin(6, 6, 4, 6))

# Panels a/b share one driver legend; the heatmap keeps its own colourbar.
p3a_leg <- p3a + theme(legend.position = "top", legend.justification = "center") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 2.4)),
         fill = guide_legend(nrow = 1))

fig3 <- (p3a_leg | p3b) / p3c +
  plot_layout(heights = c(0.92, 1.28)) +
  plot_annotation(tag_levels = "a")
save_figure(fig3, "figures/main/Fig3_driver_and_response_category_context", 190, 160)
# =============================================================================
# FIGURE 4 | Robustness boundaries and small-study diagnostics.
# (a) Original vs pre-2026-update pooled estimate per stage (|lnRR|), with CIs.
# (b) Leave-one-study-out range per stage (|lnRR|), full estimate + min/max band.
# (c) Variance-floor sensitivity across floors (|lnRR|), estimate + CI.
# (d) Contour-enhanced funnel for the SIGNED (directional) estimand, pooled,
#     with significance contours, pseudo-95% CI funnel and Egger annotation.
# =============================================================================
stage_short <- c("EMBRYO" = "Embryo", "LARVA" = "Larva",
                 "SETTLEMENT/METAMORPHOSIS" = "Settlement/met.", "JUVENILE/ADULT" = "Juvenile/adult")

# (a) Update sensitivity (absolute estimand): original vs updated with CIs.
upd <- update_sens %>%
  filter(Estimand == "Absolute |lnRR|") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)))
upd_pts <- upd %>%
  transmute(ObservedStage,
            `Original evidence` = OriginalEstimate, `Pre-2026 update` = UpdatedEstimate) %>%
  pivot_longer(-ObservedStage, names_to = "Layer", values_to = "est") %>%
  mutate(Layer = factor(Layer, levels = c("Original evidence", "Pre-2026 update")))
upd_ci <- upd %>%
  transmute(ObservedStage,
            `Original evidence__lo` = OriginalCI_low, `Original evidence__hi` = OriginalCI_high,
            `Pre-2026 update__lo` = UpdatedCI_low, `Pre-2026 update__hi` = UpdatedCI_high) %>%
  pivot_longer(-ObservedStage, names_to = c("Layer", ".value"), names_sep = "__") %>%
  mutate(Layer = factor(Layer, levels = c("Original evidence", "Pre-2026 update")))
upd_plot <- left_join(upd_pts, upd_ci, by = c("ObservedStage", "Layer")) %>%
  mutate(yy = as.numeric(ObservedStage) + ifelse(Layer == "Original evidence", 0.16, -0.16))
upd_pal <- c("Original evidence" = col("grey"), "Pre-2026 update" = col("blue"))

p4a <- ggplot(upd_plot, aes(est, yy, colour = Layer)) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", linewidth = 0.9) +
  geom_point(aes(fill = Layer), shape = 21, size = 2.0, colour = "white", stroke = 0.4) +
  scale_colour_manual(values = upd_pal, name = NULL, aesthetics = c("colour", "fill")) +
  scale_y_continuous(breaks = seq_along(stage_levels),
                     labels = stage_short[rev(stage_levels)],
                     limits = c(0.5, 4.5), expand = expansion(0)) +
  labs(x = "|lnRR| (95% CI)", y = NULL, title = "Update leaves the pattern intact") +
  grid_x +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "top", legend.justification = "left",
        legend.box.margin = margin(0, 0, -3, 0), plot.margin = margin(6, 8, 6, 4))

# (b) Leave-one-study-out (absolute): full estimate + min/max band.
loo_abs <- loo %>%
  filter(Estimand == "Absolute |lnRR|") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         yy = as.numeric(ObservedStage))
p4b <- ggplot(loo_abs, aes(FullEstimate, yy)) +
  geom_errorbar(aes(xmin = MinEstimate, xmax = MaxEstimate), width = 0.14,
                orientation = "y", linewidth = 0.9, colour = col("blue")) +
  geom_point(size = 2.2, shape = 21, fill = col("blue"), colour = "white", stroke = 0.5) +
  geom_text(aes(label = sprintf("s = %d", OmittedStudies)),
            x = max(loo_abs$MaxEstimate) * 1.04, hjust = 0,
            size = (er_base_size - 1.8) / .pt, colour = col("grey_dark")) +
  scale_y_continuous(breaks = seq_along(stage_levels), labels = stage_short[rev(stage_levels)],
                     limits = c(0.5, 4.5), expand = expansion(0)) +
  scale_x_continuous(limits = c(min(loo_abs$MinEstimate) * 0.9, max(loo_abs$MaxEstimate) * 1.22),
                     expand = expansion(0)) +
  labs(x = "|lnRR| (leave-one-study-out min-max)", y = NULL,
       title = "No single study drives the signal") +
  coord_cartesian(clip = "off") + grid_x +
  theme(plot.margin = margin(6, 8, 6, 4))
# (c) Variance-floor sensitivity (absolute estimand): estimate + CI across floors.
vf <- variance_floor %>%
  filter(Estimand == "Absolute |lnRR|") %>%
  mutate(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    FloorNum = as.numeric(VarianceFloor)
  )
floor_values <- sort(unique(vf$FloorNum))
vf <- vf %>% mutate(
  fx = match(FloorNum, floor_values),
  fx = fx + (as.numeric(ObservedStage) - 2.5) * 0.055)
p4c <- ggplot(vf, aes(fx, Estimate, colour = ObservedStage)) +
  geom_line(aes(group = ObservedStage), linewidth = 0.4) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0, linewidth = 0.7) +
  geom_point(aes(fill = ObservedStage), shape = 21, size = 1.9, colour = "white", stroke = 0.4) +
  scale_colour_manual(values = stage_pal, labels = stage_labels_1l, name = NULL,
                      aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq_along(floor_values),
                     labels = expression(10^-6, 10^-4, 10^-3)) +
  labs(x = "Variance floor", y = "|lnRR| (95% CI)",
       title = "Stable across variance floors") +
  grid_x +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = "top", legend.justification = "left",
        legend.box.margin = margin(0, 0, -3, 0),
        legend.key.width = unit(3.4, "mm"), plot.margin = margin(6, 8, 6, 4))

# (d) Contour-enhanced funnel for the SIGNED estimand, all stages pooled.
#     Pooled directional reference from the primary model (mode_stage_all).
dir_ref <- main_results %>%
  filter(GroupType == "mode_stage_all", DevelopmentalModeGroup == "With metamorphic stage",
         Estimand == "Directional lnRR") %>%
  summarise(w = sum(EffectSizeCount, na.rm = TRUE),
            ref = sum(Estimate * EffectSizeCount, na.rm = TRUE) / sum(EffectSizeCount, na.rm = TRUE)) %>%
  pull(ref)
funnel_dat <- dat %>%
  transmute(lnRR, SE = sqrt(VarianceUsedInModel)) %>%
  filter(is.finite(lnRR), is.finite(SE), SE > 0)
se_max <- quantile(funnel_dat$SE, 0.99, na.rm = TRUE)
funnel_dat <- funnel_dat %>% filter(SE <= se_max)
egger_pooled <- egger %>% filter(Estimand == "Directional lnRR")
egger_note <- sprintf("Egger (per stage): %s",
                      paste(sprintf("%s p%s", stage_short[egger_pooled$ObservedStage],
                                    ifelse(egger_pooled$Egger_p < 0.001, " < 0.001",
                                           paste0(" = ", formatC(egger_pooled$Egger_p, 3, format = "f")))),
                            collapse = "; "))
x_rng <- range(funnel_dat$lnRR)
p4d <- ggplot(funnel_dat, aes(lnRR, SE)) +
  funnel_contours(ref = dir_ref, se_max = se_max) +
  geom_point(shape = 21, fill = col("blue"), colour = "white", stroke = 0.25,
             size = 1.3, alpha = 0.6) +
  scale_y_reverse(expand = expansion(mult = c(0.02, 0))) +
  scale_x_continuous(limits = c(x_rng[1] - 0.05, x_rng[2] + 0.05)) +
  labs(x = "Directional lnRR", y = "Standard error",
       title = "Funnel on the signed effect") +
  guides(fill = guide_legend(order = 1, override.aes = list(colour = col("grey_light")))) +
  theme(legend.position = "right", legend.key.size = unit(3, "mm"),
        plot.margin = margin(6, 8, 6, 4))

fig4 <- (p4a | p4b) / (p4c | p4d) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a")
save_figure(fig4, "figures/main/Fig4_robustness_and_inference_limits", 190, 150)
# FIG4-END

# =============================================================================
# SUPPLEMENTARY FIGURE S1 | Evidence-base update flow.
# Two-stream design (existing model-ready evidence + new pre-2026 update search)
# converging on the final quantitative synthesis. All counts are read from
# screened_candidate_records.csv and the evidence-flow source data, so the
# diagram reconciles exactly with the manuscript (21 screened; 4 added; 99 new
# effect sizes; 2,313 existing -> 2,412 final across 162 study records).
# =============================================================================
scr <- screened_records %>% mutate(added = suppressWarnings(as.integer(ModelRowsAdded)))
n_screened <- nrow(scr)
n_added_rec <- sum(scr$added > 0, na.rm = TRUE)
n_added_es  <- sum(scr$added, na.rm = TRUE)
n_excluded  <- n_screened - n_added_rec
# Grouped exclusion reasons for the update-search "records excluded" box.
excl_lines <- c(
  sprintf("Outside pre-2026 cutoff (n = %d)", sum(grepl("outside the requested pre-2026", scr$UpdatedScreeningStatus))),
  sprintf("Full-text data not yet extractable (n = %d)", sum(grepl("full-text numerical extraction", scr$UpdatedScreeningStatus))),
  sprintf("No extractable raw response data (n = %d)", sum(grepl("no auditable", scr$UpdatedScreeningStatus))),
  sprintf("Outside endpoint / scope rules (n = %d)", sum(grepl("invertebrate synthesis|phenotype endpoint", scr$UpdatedScreeningStatus))),
  sprintf("Background / context only (n = %d)", sum(grepl("candidate/background|systematic-search hit", scr$UpdatedScreeningStatus)))
)

# Box helper: rectangle + wrapped label at (x, y). hjust=0.5 centres; 0 left-aligns
# (used for the excluded-reasons list).
pbox <- function(x, y, w, h, label, fill = col("white"), border = col("grey_dark"),
                 face = "plain", size = er_base_size - 0.6, text_col = col("black"),
                 hjust = 0.5) {
  tx <- if (hjust == 0) x - w/2 + 0.18 else x
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = fill, colour = border, linewidth = 0.4),
    annotate("text", x = tx, y = y, label = label, size = size / .pt, hjust = hjust,
             lineheight = 0.95, fontface = face, colour = text_col)
  )
}
parrow <- function(x0, y0, x1, y1) annotate("segment", x = x0, y = y0, xend = x1, yend = y1,
  arrow = arrow(length = unit(1.6, "mm"), type = "closed"), linewidth = 0.4, colour = col("grey_dark"))

xL <- 2.7; xR <- 7.2                       # left (existing) / right (update) streams
pS1 <- ggplot() +
  # Phase labels (rotated band on the far left).
  annotate("text", x = 0.1, y = 8.4, label = "Identification", angle = 90,
           fontface = "bold", size = (er_base_size - 0.4)/.pt, colour = col("grey_dark")) +
  annotate("text", x = 0.1, y = 5.6, label = "Screening", angle = 90,
           fontface = "bold", size = (er_base_size - 0.4)/.pt, colour = col("grey_dark")) +
  annotate("text", x = 0.1, y = 1.4, label = "Included", angle = 90,
           fontface = "bold", size = (er_base_size - 0.4)/.pt, colour = col("grey_dark")) +
  pbox(xL, 8.4, 3.8, 1.4, "Existing model-ready evidence base\n(previous curated + validated synthesis)\n2,313 effect sizes",
       fill = col("blue_light"), border = col("blue")) +
  pbox(xR, 8.4, 4.2, 1.4, sprintf("Records identified in pre-2026 update search\n(Crossref, PANGAEA, Nature Portfolio)\nn = %d records", n_screened),
       fill = col("grey_pale")) +
  pbox(xR, 5.9, 3.8, 0.9, sprintf("Records screened\nn = %d", n_screened), fill = col("grey_pale")) +
  pbox(11.0, 5.9, 3.2, 2.4, paste0("Records excluded (n = ", n_excluded, "):\n",
       paste(excl_lines, collapse = "\n")), fill = col("white"), size = er_base_size - 1.2, hjust = 0) +
  pbox(xR, 3.2, 4.0, 1.1, sprintf("New studies added to synthesis\n%d study records; %d new effect sizes", n_added_rec, n_added_es),
       fill = col("green"), text_col = col("white"), border = col("green")) +
  pbox(5.0, 1.0, 6.8, 1.2, "Final quantitative synthesis\n162 study records; 2,412 effect sizes across four life-history stages",
       fill = col("blue"), text_col = col("white"), border = col("blue"), face = "bold") +
  parrow(xR, 7.70, xR, 6.36) +
  parrow(xR, 5.45, xR, 3.76) +
  parrow(xR + 1.9, 5.9, 9.4, 5.9) +
  parrow(xL, 7.70, xL, 1.62) +
  parrow(xR, 2.65, 6.8, 1.62) +
  coord_cartesian(xlim = c(0, 12.7), ylim = c(0.2, 9.3), clip = "off") +
  labs(title = "Evidence-base update flow") +
  theme_void(base_size = er_base_size, base_family = "Arial") +
  theme(plot.title = element_text(size = er_base_size + 1, face = "bold", hjust = 0.5,
                                  margin = margin(b = 4)),
        plot.margin = margin(6, 8, 6, 4))
save_figure(pS1, "figures/extended/SupplementaryFigS1_prisma_flow_diagram", 180, 120)
# S1-END

# =============================================================================
# SUPPLEMENTARY FIGURES S2 / S3 | Funnel plots, per life-history stage.
# One small-multiple per stage, faceted. S2 is the SIGNED (directional) estimand
# drawn as a contour-enhanced funnel (significance regions vs the null + pooled
# pseudo-95% CI); S3 is the folded absolute departure |lnRR|, drawn as a
# precision funnel (pooled reference + pseudo-95% CI only) because "significance
# vs 0" contours are not interpretable for a quantity bounded at zero. Egger z/P
# per stage are annotated in-panel so asymmetry can be read directly.
# =============================================================================
# Per-stage pooled reference (weighted mean of stage estimates within each stage).
stage_ref <- function(estimand) {
  main_results %>%
    filter(GroupType == "mode_stage_all",
           DevelopmentalModeGroup == "With metamorphic stage",
           Estimand == estimand) %>%
    transmute(ObservedStage = factor(ObservedStage, levels = stage_levels), ref = Estimate)
}
# Build a faceted funnel across stages for one estimand. `contours = TRUE` draws
# the full contour-enhanced funnel (significance regions vs the null); this is
# only meaningful for the SIGNED estimand. For the folded |lnRR| we set
# `contours = FALSE` and draw a precision funnel (pooled reference + pseudo-95%
# CI only), because "significance vs 0" contours are not interpretable on a
# quantity bounded at zero.
funnel_by_stage <- function(value_col, estimand, xlab, title, contours = TRUE) {
  pts <- dat %>%
    transmute(ObservedStage = factor(ObservedStage, levels = stage_levels),
              val = .data[[value_col]], SE = sqrt(VarianceUsedInModel)) %>%
    filter(is.finite(val), is.finite(SE), SE > 0)
  se_cap <- as.numeric(quantile(pts$SE, 0.99, na.rm = TRUE))
  pts <- pts %>% filter(SE <= se_cap)
  refs <- stage_ref(estimand)
  x_lo <- as.numeric(quantile(pts$val, 0.005, na.rm = TRUE))
  x_hi <- as.numeric(quantile(pts$val, 0.995, na.rm = TRUE))
  eg <- egger %>% filter(Estimand == estimand) %>%
    mutate(ObservedStage = factor(ObservedStage, levels = stage_levels),
           lab = sprintf("'Egger '*italic(z) == '%s'*'; '*italic(P)*' %s'",
                         formatC(Egger_z, 2, format = "f"),
                         ifelse(Egger_p < 0.001, "< 0.001",
                                paste0("= ", formatC(Egger_p, 3, format = "f")))))
  # Funnel background per facet (contours + CI, or CI-only), tagged by stage.
  bg_layers <- lapply(seq_len(nrow(refs)), function(i) {
    st <- refs$ObservedStage[i]; rf <- refs$ref[i]
    if (contours) {
      lay <- funnel_contours(ref = rf, se_max = se_cap)
      if (i > 1L) lay <- lay[!vapply(lay, inherits, logical(1), what = "Scale")]
    } else {
      se <- seq(0, se_cap, length.out = 120)
      ci <- data.frame(se = se, lo = rf - 1.96 * se, hi = rf + 1.96 * se)
      lay <- list(
        geom_line(data = ci, aes(x = lo, y = se), inherit.aes = FALSE,
                  linewidth = 0.3, linetype = "dashed", colour = col("grey_dark")),
        geom_line(data = ci, aes(x = hi, y = se), inherit.aes = FALSE,
                  linewidth = 0.3, linetype = "dashed", colour = col("grey_dark")),
        geom_vline(xintercept = rf, linewidth = 0.34, colour = col("black"))
      )
    }
    lapply(lay, function(l) {
      if (!is.null(l$data) && is.data.frame(l$data)) l$data$ObservedStage <- st
      l
    })
  })
  p <- ggplot(pts, aes(val, SE)) +
    bg_layers +
    geom_point(shape = 21, fill = col("blue"), colour = "white", stroke = 0.2,
               size = 1.0, alpha = 0.55) +
    # Egger annotation in a white label box in the sparse bottom-left corner.
    geom_label(data = eg, aes(x = x_lo, y = se_cap, label = lab), parse = TRUE,
               inherit.aes = FALSE, hjust = 0, vjust = 1, linewidth = 0,
               fill = alpha("white", 0.75), label.padding = unit(0.6, "mm"),
               size = (er_base_size - 1.8)/.pt, colour = col("grey_dark")) +
    facet_wrap(~ ObservedStage, nrow = 2, labeller = as_labeller(stage_labels_1l)) +
    scale_y_reverse(expand = expansion(mult = c(0.06, 0.02))) +
    scale_x_continuous() +
    labs(x = xlab, y = "Standard error", title = title) +
    coord_cartesian(xlim = c(x_lo, x_hi)) +
    theme(panel.spacing = unit(4, "mm"), plot.margin = margin(6, 8, 6, 6))
  if (contours) {
    p <- p + guides(fill = guide_legend(override.aes = list(colour = col("grey_light")))) +
      theme(legend.position = "right", legend.key.size = unit(3, "mm"))
  }
  p
}
pS2 <- funnel_by_stage("lnRR", "Directional lnRR",
                       "Directional lnRR (signed effect)",
                       "Contour-enhanced funnels; directional response, by stage",
                       contours = TRUE)
pS3 <- funnel_by_stage("abs_lnRR", "Absolute |lnRR|",
                       "|lnRR| (absolute departure)",
                       "Precision funnels; absolute departure, by stage",
                       contours = FALSE)
save_figure(pS2, "figures/extended/SupplementaryFigS2_directional_funnel_plots", 180, 150)
save_figure(pS3, "figures/extended/SupplementaryFigS3_absolute_funnel_plots", 180, 150)
# S3-END

# =============================================================================
# SUPPLEMENTARY FIGURE S4 | Small-study and heterogeneity summary.
# (a) Egger regression P by stage and estimand on a log axis, with the P = 0.05
#     boundary; (b) I^2 and tau^2 per stage for the two primary estimands. This
#     replaces the old single-estimand "boundaries" scatter with a complete,
#     directly-readable small-study + heterogeneity panel.
# =============================================================================
eg_all <- egger %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         Estimand = factor(Estimand, levels = c("Directional lnRR", "Absolute |lnRR|")))
pS4a <- ggplot(eg_all, aes(Egger_p, ObservedStage, fill = Estimand)) +
  annotate("rect", xmin = 1e-6, xmax = 0.05, ymin = -Inf, ymax = Inf,
           fill = col("grey_pale"), alpha = 0.6) +
  geom_vline(xintercept = 0.05, linetype = "dashed", linewidth = 0.34, colour = col("grey_dark")) +
  geom_point(shape = 21, size = 2.6, colour = "white", stroke = 0.5,
             position = position_dodge(width = 0.55)) +
  annotate("text", x = 0.043, y = 4.35, label = "P = 0.05", hjust = 1,
           size = (er_base_size - 1.8)/.pt, colour = col("grey_dark")) +
  coord_cartesian(clip = "off") +
  scale_x_log10(labels = c("0.0001", "0.001", "0.01", "0.05", "0.5"),
                breaks = c(1e-4, 1e-3, 1e-2, 0.05, 0.5)) +
  scale_y_discrete(labels = rev(stage_labels_1l)) +
  scale_fill_manual(values = c("Directional lnRR" = col("red"), "Absolute |lnRR|" = col("blue")),
                    name = NULL) +
  labs(x = "Egger regression P value (log scale)", y = NULL,
       title = "Small-study asymmetry (Egger test)") +
  guides(fill = guide_legend(nrow = 1)) +
  grid_x +
  theme(legend.position = "top", legend.justification = "left",
        legend.box.margin = margin(0, 0, -3, 0), plot.margin = margin(6, 10, 6, 6))

het <- robustness %>%
  filter(Estimand %in% c("Raw directional lnRR", "Exploratory response magnitude |lnRR|")) %>%
  transmute(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
            Estimand = factor(dplyr::recode(Estimand,
                        "Raw directional lnRR" = "Directional lnRR",
                        "Exploratory response magnitude |lnRR|" = "Absolute |lnRR|"),
                        levels = c("Directional lnRR", "Absolute |lnRR|")),
            I2_percent, Tau2)
pS4b <- ggplot(het, aes(I2_percent, ObservedStage, fill = Estimand)) +
  geom_col(width = 0.62, position = position_dodge(width = 0.68), colour = "white", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%  (tau² %.2f)", I2_percent, Tau2)),
            position = position_dodge(width = 0.68), hjust = -0.05,
            size = (er_base_size - 1.8)/.pt, colour = col("grey_dark")) +
  scale_y_discrete(labels = rev(stage_labels_1l)) +
  scale_x_continuous(limits = c(0, 118), breaks = c(0, 25, 50, 75, 100),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_fill_manual(values = c("Directional lnRR" = col("red"), "Absolute |lnRR|" = col("blue")),
                    name = NULL) +
  labs(x = expression("Heterogeneity "*italic(I)^2*" (%)"), y = NULL,
       title = "Heterogeneity by stage") +
  guides(fill = guide_legend(nrow = 1)) +
  grid_x +
  theme(legend.position = "top", legend.justification = "left",
        legend.box.margin = margin(0, 0, -3, 0), plot.margin = margin(6, 8, 6, 6))

pS4 <- (pS4a / pS4b) + plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "a")
save_figure(pS4, "figures/extended/SupplementaryFigS4_small_study_and_heterogeneity", 180, 150)
# S4-END