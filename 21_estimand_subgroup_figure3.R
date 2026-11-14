# Figure 3: common-subset estimand comparison, paired target differences, and
# exploratory taxonomic/ecological subgroup estimates.
if (!exists("root_dir")) source(file.path("R", "00_setup.R"))

analysis_dat <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "analysis_ready_model_dataset_deduplicated.csv"),
  show_col_types = FALSE, progress = FALSE
)
coding_dat <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "ER_endpoint_direction_row_coding.csv"),
  show_col_types = FALSE, progress = FALSE
)
if (nrow(analysis_dat) != nrow(coding_dat) ||
    any(as.character(analysis_dat$StudyID) != as.character(coding_dat$StudyID)) ||
    any(as.character(analysis_dat$SpeciesID) != as.character(coding_dat$SpeciesID))) {
  stop("Endpoint-direction coding rows no longer align with the analysis dataset.")
}

common_dat <- coding_dat %>%
  dplyr::mutate(
    DevelopmentalModeGroup = analysis_dat$DevelopmentalModeGroup,
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    StressorLabel = factor(StressorLabel, levels = stressor_levels)
  ) %>%
  dplyr::filter(
    DevelopmentalModeGroup == "With metamorphic stage",
    is.finite(lnRR), is.finite(aligned_lnRR_strict),
    is.finite(vi_model), vi_model > 0, !is.na(StudyID)
  )

# Every target below is derived from the same direction-interpretable rows.
# Absolute magnitude is applied after signed aggregation within each independent
# StudyID x stage x stressor cell, so opposite endpoint directions can cancel.
common_cells <- common_dat %>%
  dplyr::group_by(ObservedStage, StressorLabel, StudyID) %>%
  dplyr::summarise(
    Directional = sum(lnRR / vi_model) / sum(1 / vi_model),
    AlignedPerformance = sum(aligned_lnRR_strict / vi_model) / sum(1 / vi_model),
    vi = (.2 * sum(1 / vi_model) + .8 * sum(1 / sqrt(vi_model))^2) / sum(1 / vi_model)^2,
    RawEffectSizeCount = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(AbsoluteMagnitude = abs(Directional)) %>%
  dplyr::filter(is.finite(Directional), is.finite(AlignedPerformance), is.finite(vi), vi > 0)

estimand_spec <- tibble::tribble(
  ~Column, ~Estimand, ~PanelLabel, ~ZeroTest,
  "Directional", "Directional lnRR", "Directional", TRUE,
  "AlignedPerformance", "Endpoint-aligned performance lnRR", "Aligned performance", TRUE,
  "AbsoluteMagnitude", "Absolute magnitude after signed cell aggregation", "Absolute magnitude", FALSE
)

fit_cell_target <- function(x, column, estimand, panel_label, zero_test) {
  x <- x %>% dplyr::filter(is.finite(.data[[column]]), is.finite(vi), vi > 0)
  k <- sum(x$RawEffectSizeCount)
  s <- nrow(x)
  if (s < 3L) {
    return(tibble::tibble(
      Estimand = estimand, PanelLabel = panel_label, Estimate = NA_real_, SE = NA_real_,
      CI_low = NA_real_, CI_high = NA_real_, PI_low = NA_real_, PI_high = NA_real_,
      P_value = NA_real_, EffectSizeCount = k, StudyCount = s
    ))
  }
  fit <- metafor::rma.uni(yi = x[[column]], vi = x$vi, method = "REML", test="knha")
  pred <- metafor::predict.rma(fit)
  tibble::tibble(
    Estimand = estimand, PanelLabel = panel_label,
    Estimate = as.numeric(fit$b), SE = as.numeric(fit$se),
    CI_low = as.numeric(fit$ci.lb), CI_high = as.numeric(fit$ci.ub),
    PI_low = as.numeric(pred$pi.lb), PI_high = as.numeric(pred$pi.ub),
    P_value = if (zero_test) as.numeric(fit$pval) else NA_real_,
    EffectSizeCount = k, StudyCount = s
  )
}

estimand_driver <- dplyr::bind_rows(lapply(seq_len(nrow(estimand_spec)), function(i) {
  spec <- estimand_spec[i, ]
  dplyr::bind_rows(lapply(stage_levels, function(stage) {
    dplyr::bind_rows(lapply(stressor_levels, function(stressor) {
      fit_cell_target(
        common_cells %>% dplyr::filter(ObservedStage == stage, StressorLabel == stressor),
        spec$Column, spec$Estimand, spec$PanelLabel, spec$ZeroTest
      ) %>% dplyr::mutate(ObservedStage = stage, StressorLabel = stressor)
    }))
  }))
})) %>%
  dplyr::group_by(Estimand) %>%
  dplyr::mutate(FDR_q_value = if (all(is.na(P_value))) NA_real_ else p.adjust(P_value, method = "BH")) %>%
  dplyr::ungroup()

write_csv_safe(estimand_driver, "data/source_data/Fig4a_common_subset_estimand_driver_estimates.csv")

# Paired differences use equal StudyID-cell weights. Complete cells are
# resampled so all three target values remain paired. Two-sided P values use a
# centered-null bootstrap distribution; all 27 estimable comparisons form one
# prespecified BH-FDR family.
target_pairs <- list(
  c("AlignedPerformance", "Directional", "Aligned - directional"),
  c("AbsoluteMagnitude", "Directional", "Absolute - directional"),
  c("AbsoluteMagnitude", "AlignedPerformance", "Absolute - aligned")
)
inferential_pair <- function(pair_label) identical(pair_label, "Aligned - directional")
paired_difference_rows <- list()
row_index <- 1L
for (stage_index in seq_along(stage_levels)) {
  for (stressor_index in seq_along(stressor_levels)) {
    cells <- common_cells %>% dplyr::filter(
      ObservedStage == stage_levels[stage_index],
      StressorLabel == stressor_levels[stressor_index]
    )
    for (pair_index in seq_along(target_pairs)) {
      pair <- target_pairs[[pair_index]]
      delta <- cells[[pair[1]]] - cells[[pair[2]]]
      delta <- delta[is.finite(delta)]
      if (length(delta) >= 3L) {
        set.seed(20260908L + 100L * stage_index + 10L * stressor_index + pair_index)
        bootstrap_resamples <- 9999L
        boot <- replicate(bootstrap_resamples, mean(sample(delta, length(delta), replace = TRUE)))
        ci <- as.numeric(stats::quantile(boot, c(0.025, 0.975), na.rm = TRUE))
        estimate <- mean(delta)
        # A centered zero-null test is appropriate for the signed aligned-minus-
        # directional contrast.  It is not appropriate for contrasts involving
        # abs(Directional): those differences are structurally non-negative
        # (|D|-D) and zero is a boundary value.  Keep their bootstrap intervals
        # descriptive and do not manufacture FDR stars from a degenerate null.
        if (inferential_pair(pair[3])) {
          boot_null <- boot - mean(boot)
          p_value <- (1 + sum(abs(boot_null) >= abs(estimate))) / (bootstrap_resamples + 1)
        } else {
          p_value <- NA_real_
        }
      } else {
        bootstrap_resamples <- 9999L
        estimate <- ci <- p_value <- NA_real_
      }
      paired_difference_rows[[row_index]] <- tibble::tibble(
        ObservedStage = stage_levels[stage_index],
        StressorLabel = stressor_levels[stressor_index],
        TargetDifference = pair[3],
        Estimate = estimate,
        CI_low = ci[1], CI_high = ci[2],
        StudyCellCount = length(delta),
        IntervalExcludesZero = is.finite(ci[1]) && (ci[1] > 0 || ci[2] < 0),
        P_value = p_value,
        BootstrapResamples = bootstrap_resamples,
        Weighting = "Equal StudyID-cell weight",
        IntervalMethod = "Paired StudyID-cell percentile bootstrap; 9999 resamples",
        PValueMethod = if (inferential_pair(pair[3])) {
          "Two-sided centered-null paired StudyID-cell bootstrap with +1 correction"
        } else {
          "Not tested: folded absolute target has a structural non-negative difference"
        }
      )
      row_index <- row_index + 1L
    }
  }
}
paired_differences <- dplyr::bind_rows(paired_difference_rows)
q_signed <- rep(NA_real_, nrow(paired_differences))
signed_idx <- which(paired_differences$TargetDifference == "Aligned - directional" & is.finite(paired_differences$P_value))
if (length(signed_idx)) q_signed[signed_idx] <- stats::p.adjust(paired_differences$P_value[signed_idx], method = "BH")
paired_differences <- paired_differences %>%
  dplyr::mutate(
    FDRFamily = dplyr::case_when(
      TargetDifference == "Aligned - directional" ~ "Nine signed aligned-minus-directional contrasts",
      TRUE ~ "Not inferentially tested: folded absolute contrasts"
    ),
    FDR_q_value = q_signed,
    SignificantFDR = !is.na(FDR_q_value) & FDR_q_value < 0.05,
    Significance = dplyr::case_when(
      is.na(FDR_q_value) ~ "NE",
      FDR_q_value < 0.001 ~ "***",
      FDR_q_value < 0.01 ~ "**",
      FDR_q_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )
write_csv_safe(paired_differences, "data/processed/ER_estimand_target_difference_summary.csv")
write_csv_safe(paired_differences, "data/source_data/Fig4b_estimand_target_differences.csv")

class_subgroups <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "taxonomy_class_stressor_RVE_all_rho.csv"),
  show_col_types = FALSE, progress = FALSE
) %>% dplyr::filter(Rho == 0.8)
guild_subgroups <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "ecological_guild_stressor_meta_all_rho.csv"),
  show_col_types = FALSE, progress = FALSE
) %>% dplyr::filter(Rho == 0.8)
write_csv_safe(class_subgroups, "data/source_data/Fig3c_taxonomic_class_subgroups.csv")
write_csv_safe(guild_subgroups, "data/source_data/Fig3d_ecological_guild_subgroups.csv")

stage_y <- setNames(rev(seq_along(stage_levels)), stage_levels)
driver_off <- setNames(c(0.23, 0, -0.23), stressor_levels)
plot_estimates <- estimand_driver %>%
  dplyr::mutate(
    PanelLabel = factor(PanelLabel, levels = estimand_spec$PanelLabel),
    y = stage_y[ObservedStage] + driver_off[StressorLabel],
    NE = ifelse(StudyCount < 3, "NE", NA_character_)
  )
cell_points <- common_cells %>%
  dplyr::select(ObservedStage, StressorLabel, Directional, AlignedPerformance, AbsoluteMagnitude) %>%
  tidyr::pivot_longer(c(Directional, AlignedPerformance, AbsoluteMagnitude), names_to = "Column", values_to = "CellEstimate") %>%
  dplyr::left_join(estimand_spec %>% dplyr::select(Column, PanelLabel), by = "Column") %>%
  dplyr::mutate(PanelLabel = factor(PanelLabel, levels = estimand_spec$PanelLabel),
                y = stage_y[ObservedStage] + driver_off[StressorLabel])

p_est <- ggplot2::ggplot(plot_estimates, ggplot2::aes(Estimate, y, colour = StressorLabel)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.28, colour = col("grey")) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = PI_low, xmax = PI_high), orientation = "y", width = 0,
    linewidth = 0.25, colour = col("grey"), na.rm = TRUE
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = 0,
    linewidth = 0.8, na.rm = TRUE
  ) +
  ggplot2::geom_point(ggplot2::aes(fill = StressorLabel), shape = 21, size = 1.8,
                      colour = "white", stroke = 0.35, na.rm = TRUE) +
  ggplot2::geom_text(
    data = plot_estimates %>% dplyr::filter(!is.na(NE)),
    ggplot2::aes(x = 0, y = y, label = NE), colour = col("grey_dark"), size = 1.8,
    inherit.aes = FALSE
  ) +
  ggplot2::facet_wrap(~ PanelLabel, nrow = 1, scales = "free_x") +
  ggplot2::scale_colour_manual(values = driver_pal[stressor_levels], name = NULL) +
  ggplot2::scale_fill_manual(values = driver_pal[stressor_levels], name = NULL) +
  ggplot2::scale_y_continuous(
    breaks = stage_y, labels = stage_labels_1l[names(stage_y)],
    limits = c(0.55, 4.45), expand = ggplot2::expansion(0)
  ) +
  ggplot2::labs(x = "Estimate (95% CI; thin line = 95% prediction interval)", y = "Life-history stage") +
  grid_x +
  ggplot2::theme(
    legend.position = "top", legend.justification = "center",
    axis.text.y = ggplot2::element_text(size = 6.0),
    panel.spacing = grid::unit(5, "mm"), plot.margin = ggplot2::margin(4, 5, 3, 4)
  )

diff_vals <- abs(c(paired_differences$CI_low, paired_differences$CI_high))
diff_limit <- max(0.1, as.numeric(stats::quantile(diff_vals[is.finite(diff_vals)], 0.95, na.rm = TRUE)) )
plot_differences <- paired_differences %>%
  dplyr::mutate(
    ObservedStage = factor(ObservedStage, levels = rev(stage_levels), labels = rev(stage_labels_1l)),
    StressorLabel = factor(StressorLabel, levels = stressor_levels,
                           labels = c("Acidification", "Warming", "Both")),
    TargetDifference = factor(TargetDifference, levels = vapply(target_pairs, `[[`, character(1), 3)),
    Label = ifelse(
      is.finite(Estimate),
      paste0(sprintf("%.2f\n[%.2f, %.2f]", Estimate, CI_low, CI_high),
             ifelse(Significance == "", "", paste0(" ", Significance))), "NE"
    )
  )
p_diff <- ggplot2::ggplot(plot_differences, ggplot2::aes(StressorLabel, ObservedStage, fill = Estimate)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
  ggplot2::geom_tile(
    data = plot_differences %>% dplyr::filter(SignificantFDR),
    fill = NA, colour = col("grey_dark"), linewidth = 0.28
  ) +
  ggplot2::geom_text(ggplot2::aes(label = Label), size = 1.35, lineheight = 0.95, colour = "#3B335F") +
  ggplot2::facet_wrap(~ TargetDifference, nrow = 1) +
  ggplot2::coord_fixed(ratio = 1) +
  ggplot2::scale_fill_gradient2(
    low = "#0072B2", mid = "#F4F2F9", high = "#D55E00", midpoint = 0,
    limits = c(-diff_limit, diff_limit), oob = scales::squish, name = "Mean paired\ndifference"
  ) +
  ggplot2::labs(
    x = "Driver", y = "Life-history stage",
    caption = "Mean paired difference [95% bootstrap CI]. Stars apply only to the signed aligned-minus-directional family (BH q<0.05); folded absolute contrasts are descriptive because their differences are structurally non-negative."
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 22, hjust = 1),
    legend.position = "right", panel.spacing = grid::unit(6, "mm"),
    plot.caption = ggplot2::element_text(size = 5.7, hjust = 0)
  )

subgroup_limit <- max(abs(c(class_subgroups$Estimate, guild_subgroups$Estimate)), na.rm = TRUE)
subgroup_forest <- function(d, group_col, y_title) {
  group_values <- sort(unique(d[[group_col]]))
  dd <- d %>%
    dplyr::mutate(
      Group = factor(.data[[group_col]], levels = rev(group_values)),
      StressorLabel = factor(as.character(StressorLabel), levels = as.character(stressor_levels),
                             labels = c("Acidification", "Warming", "Combined exposure")),
      Label = dplyr::case_when(is.na(Estimate) ~ "NE", SignificantFDR ~ paste0(sprintf("%.2f", Estimate), "*"), TRUE ~ sprintf("%.2f", Estimate)),
      CountLabel = ifelse(is.na(Estimate), "NE", paste0("k", EffectSizeCount, "/s", StudyCount,
        ifelse(is.finite(I2), paste0("; I²=", round(I2), "%"), "")))
    )
  dd <- dd %>% dplyr::mutate(y = as.numeric(Group) + dplyr::recode(as.character(StressorLabel), Acidification = 0.18, Warming = 0, `Combined exposure` = -0.18))
  ggplot2::ggplot(dd, ggplot2::aes(Estimate, y, colour = StressorLabel)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25, colour = col("grey")) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = CI_low, xmax = CI_high), orientation = "y", height = 0, linewidth = 0.44, na.rm = TRUE) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = PI_low, xmax = PI_high), orientation = "y", height = 0, linewidth = 0.16, colour = col("grey"), na.rm = TRUE) +
    ggplot2::geom_point(size = 1.7, shape = 16, na.rm = TRUE) +
    ggplot2::geom_text(data = dd %>% dplyr::filter(is.na(Estimate)), ggplot2::aes(x = 0, label = Label), inherit.aes = FALSE, y = dd$y[is.na(dd$Estimate)], colour = col("grey_dark"), size = 1.8) +
    ggplot2::scale_colour_manual(values = driver_pal[stressor_levels], name = NULL) +
    ggplot2::scale_y_continuous(breaks = seq_along(group_values), labels = rev(group_values), limits = c(0.45, length(group_values) + 0.55), expand = c(0,0)) +
    ggplot2::geom_text(data = dd %>% dplyr::filter(is.finite(Estimate)), ggplot2::aes(x = Inf, label = CountLabel), hjust = 1.05, inherit.aes = FALSE, y = dd$y[is.finite(dd$Estimate)], size = 1.65, colour = col("grey_dark")) +
    ggplot2::labs(x = "Directional lnRR (95% CI; thin line = 95% PI)", y = y_title) +
    grid_x +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 5.7), axis.text.y = ggplot2::element_text(size = 5.7),
      legend.position = "top", plot.margin = ggplot2::margin(3, 3, 3, 3)
    )
}
p_class <- subgroup_forest(class_subgroups, "Class", "Taxonomic class")
p_guild <- subgroup_forest(guild_subgroups, "EcologicalGuild", "Ecological guild")

moderator_omnibus <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "source_data", "Fig3e_moderator_omnibus_tests.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  dplyr::mutate(
    Moderator = factor(Moderator, levels = rev(c(
      "Phylum", "Taxonomic class", "Ecological guild", "Endpoint pathway", "Life-history stage"
    ))),
    StressorLabel = factor(StressorLabel, levels = stressor_levels,
                           labels = c("Acidification", "Warming", "Both")),
    NegLogQ = ifelse(is.na(FDR_q_value), NA_real_, -log10(pmax(FDR_q_value, 1e-6))),
    Label = dplyr::case_when(
      is.na(FDR_q_value) ~ "NE",
      Significance != "" ~ Significance,
      TRUE ~ "ns"
    )
  )
p_moderator <- ggplot2::ggplot(
  moderator_omnibus, ggplot2::aes(StressorLabel, Moderator, fill = NegLogQ)
) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(label = Label, fontface = ifelse(SignificantFDR, "bold", "plain")),
    size = 1.65, lineheight = 0.86
  ) +
  ggplot2::scale_discrete_identity(aesthetics = "fontface") +
  ggplot2::scale_fill_gradientn(
    colours = c("#F4F2F9", "#A9CBE6", "#0072B2", "#0B4F7F"),
    values = c(0, 0.25, 0.65, 1), na.value = "#EAE7F3", name = expression(-log[10](q))
  ) +
  ggplot2::labs(x = "Driver", y = "Moderator") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 24, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 5.5), legend.position = "right",
    plot.margin = ggplot2::margin(3, 3, 3, 3)
  )

fig3_updated <- p_est / p_diff +
  patchwork::plot_layout(heights = c(1.0, 1.25)) +
  patchwork::plot_annotation(tag_levels = "a")
save_figure(
  fig3_updated, "figures/main/Fig3_driver_estimand_and_subgroup_comparison",
  190, 125, dpi = 1000
)

# Subgroup results are rendered as a separate main figure. Forest plots are used
# for class and guild estimates; heatmaps are reserved for corrected omnibus
# evidence, with a perceptually balanced diverging scale and explicit labels.
fig4_subgroup <- (p_class | p_guild) / p_moderator +
  patchwork::plot_layout(heights = c(1.35, 0.9), guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(legend.position = "right")
save_figure(
  fig4_subgroup, "figures/main/Fig4_ecological_subgroup_responses",
  190, 185, dpi = 1000
)

# Replace the classification-only panel in Supplementary Figure S7 whenever
# the detailed class calibration objects from R/18_taxonomy_class_meta.R exist.
if (exists("p_heat") && exists("p_sig") && exists("make_opentree_plot")) {
  s7_updated <- (make_opentree_plot(compact = FALSE) | p_heat) / p_sig +
    patchwork::plot_layout(heights = c(1.0, 1.18)) +
    patchwork::plot_annotation(tag_levels = "a")
  save_figure(
    s7_updated, "figures/extended/Fig_taxonomy_class_stressor_calibration",
    190, 162, dpi = 1000
  )
}

writeLines(
  c(
    "Figure 3 common subset: conservatively direction-interpretable endpoints in metamorphic taxa.",
    "Directional and aligned targets retain sign; absolute magnitude is applied after signed StudyID-stage-stressor aggregation.",
    "Between-target differences are equal-StudyID-cell paired means with 9999-resample percentile bootstrap intervals.",
    "Only signed aligned-minus-directional contrasts receive centered-null P values and BH correction; contrasts involving absolute folding are descriptive because |D|-D is structurally non-negative.",
    "Taxonomic-class and ecological-guild panels are exploratory directional REML estimates at rho = 0.8; moderator omnibus tests are BH-adjusted across the 15 prespecified moderator-by-driver combinations.",
    "Detailed subgroup calibration and moderator estimates are in Supplementary Figures S7-S9."
  ),
  file.path(root_dir, "analysis_logs", "Fig3_estimand_subgroup_render_log.txt")
)
