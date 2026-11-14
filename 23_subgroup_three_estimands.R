# Three estimands for ecological subgroups and paired bootstrap contrasts
if (!exists("root_dir")) source(file.path("R", "00_setup.R"))

analysis <- readr::read_csv(guess_max=100000, file.path(root_dir, "data", "processed", "analysis_ready_with_taxonomy_ecological_guild.csv"), show_col_types = FALSE, progress = FALSE)
coding <- readr::read_csv(guess_max=100000, file.path(root_dir, "data", "processed", "ER_endpoint_direction_row_coding.csv"), show_col_types = FALSE, progress = FALSE)
stopifnot(nrow(analysis) == nrow(coding))
dat <- analysis %>%
  dplyr::mutate(.row = dplyr::row_number()) %>%
  dplyr::select(.row, StudyID, SpeciesID, ObservedStage, StressorLabel, Phylum, Class, EcologicalGuild, Category) %>%
  dplyr::left_join(coding %>% dplyr::mutate(.row = dplyr::row_number()) %>% dplyr::select(.row, lnRR, aligned_lnRR_strict, vi_model), by = ".row") %>%
  dplyr::filter(is.finite(lnRR), is.finite(aligned_lnRR_strict), is.finite(vi_model), vi_model > 0, !is.na(StudyID), StressorLabel %in% stressor_levels)

# Use only direction-interpretable rows so all three targets have identical evidence.
mods <- list(
  Phylum = "Phylum",
  `Taxonomic class` = "Class",
  `Ecological guild` = "EcologicalGuild",
  `Endpoint category` = "Category",
  `Life-history stage` = "ObservedStage"
)
target_cols <- c("Directional", "AlignedPerformance", "AbsoluteMagnitude")
target_labels <- c(Directional = "Directional change", AlignedPerformance = "Endpoint-aligned response", AbsoluteMagnitude = "Departure from reference")

cells <- list(); estimates <- list(); contrasts <- list(); z <- 1L; zi <- 1L; zc <- 1L
for (mod in names(mods)) {
  moderator_col <- mods[[mod]]
  x <- dat %>% dplyr::mutate(Group = as.character(.data[[moderator_col]])) %>% dplyr::filter(!is.na(Group), Group != "", Group != "Mixed-uncertain")
  cc <- x %>% dplyr::group_by(Moderator = mod, Group, StressorLabel, StudyID) %>% dplyr::summarise(
    Directional = sum(lnRR / vi_model) / sum(1 / vi_model),
    AlignedPerformance = sum(aligned_lnRR_strict / vi_model) / sum(1 / vi_model),
    vi = (.2 * sum(1 / vi_model) + .8 * sum(1 / sqrt(vi_model))^2) / sum(1 / vi_model)^2, EffectSizeCount = dplyr::n(), SpeciesCount = dplyr::n_distinct(SpeciesID), .groups = "drop") %>%
    dplyr::mutate(AbsoluteMagnitude = abs(Directional))
  cells[[z]] <- cc; z <- z + 1L
  for (g in sort(unique(cc$Group))) for (s in stressor_levels) {
    q <- cc %>% dplyr::filter(Group == g, StressorLabel == s)
    for (t in target_cols) {
      if (nrow(q) >= 3L) {
        fit <- metafor::rma.uni(yi = q[[t]], vi = q$vi, method = "REML", test="knha")
        pr <- metafor::predict.rma(fit)
        pv <- if (t == "AbsoluteMagnitude") NA_real_ else as.numeric(fit$pval)
        estimates[[zi]] <- tibble::tibble(Moderator = mod, Group = g, StressorLabel = s, Estimand = target_labels[[t]], EstimandCode = t, Estimate = as.numeric(fit$b), SE = as.numeric(fit$se), CI_low = as.numeric(fit$ci.lb), CI_high = as.numeric(fit$ci.ub), PI_low = as.numeric(pr$pi.lb), PI_high = as.numeric(pr$pi.ub), P_value = pv, StudyCount = nrow(q), EffectSizeCount = sum(q$EffectSizeCount), SpeciesCount = sum(q$SpeciesCount), EstimableStatus = "estimable", CommonSubsetDefinition = "direction-interpretable endpoint rows; signed StudyID-cell aggregation")
      } else estimates[[zi]] <- tibble::tibble(Moderator = mod, Group = g, StressorLabel = s, Estimand = target_labels[[t]], EstimandCode = t, Estimate = NA_real_, SE = NA_real_, CI_low = NA_real_, CI_high = NA_real_, PI_low = NA_real_, PI_high = NA_real_, P_value = NA_real_, StudyCount = nrow(q), EffectSizeCount = sum(q$EffectSizeCount), SpeciesCount = sum(q$SpeciesCount), EstimableStatus = "NE: insufficient for inference", CommonSubsetDefinition = "direction-interpretable endpoint rows; signed StudyID-cell aggregation")
      zi <- zi + 1L
    }
    # paired estimand differences are only formed from the same independent cells
    if (nrow(q) >= 3L) for (pair in list(c("AlignedPerformance", "Directional", "Endpoint-aligned response - directional change"), c("AbsoluteMagnitude", "Directional", "Departure - directional change"), c("AbsoluteMagnitude", "AlignedPerformance", "Departure - endpoint-aligned response"))) {
      d <- q[[pair[1]]] - q[[pair[2]]]; d <- d[is.finite(d)]
      if (length(d) >= 3L) { set.seed(23000 + zc); b <- replicate(9999L, mean(sample(d, length(d), replace = TRUE))); est <- mean(d); ci <- as.numeric(stats::quantile(b, c(.025, .975))); p <- if (pair[3] == "Endpoint-aligned response - directional change") (1 + sum(abs(b - mean(b)) >= abs(est))) / 10000 else NA_real_ } else { est <- p <- NA_real_; ci <- c(NA_real_, NA_real_) }
      contrasts[[zc]] <- tibble::tibble(Moderator = mod, Group = g, StressorLabel = s, Contrast = pair[3], Estimate = est, CI_low = ci[1], CI_high = ci[2], StudyCellCount = length(d), P_value = p, BootstrapResamples = 9999L, PValueMethod = if (pair[3] == "Endpoint-aligned response - directional change") "two-sided centered-null paired StudyID-cell bootstrap" else "Not tested: folded absolute target has a structural non-negative difference")
      zc <- zc + 1L
    }
  }
}
subgroup_cells <- dplyr::bind_rows(cells)
subgroup_estimates <- dplyr::bind_rows(estimates) %>% dplyr::group_by(Moderator, EstimandCode) %>% dplyr::mutate(FDR_q_value = if(all(is.na(P_value))) rep(NA_real_,dplyr::n()) else stats::p.adjust(P_value, "BH")) %>% dplyr::ungroup()
subgroup_contrasts <- dplyr::bind_rows(contrasts)
q_signed <- rep(NA_real_, nrow(subgroup_contrasts)); idx <- which(subgroup_contrasts$Contrast == "Endpoint-aligned response - directional change" & is.finite(subgroup_contrasts$P_value)); if (length(idx)) q_signed[idx] <- stats::p.adjust(subgroup_contrasts$P_value[idx], "BH")
subgroup_contrasts <- subgroup_contrasts %>% dplyr::mutate(FDRFamily = dplyr::if_else(Contrast == "Endpoint-aligned response - directional change", "Signed aligned-minus-directional subgroup contrasts", "Not inferentially tested: folded absolute contrasts"), FDR_q_value = q_signed, SignificantFDR = !is.na(FDR_q_value) & FDR_q_value < .05, Significance = dplyr::case_when(FDR_q_value < .001 ~ "***", FDR_q_value < .01 ~ "**", FDR_q_value < .05 ~ "*", TRUE ~ ""))
write_csv_safe(subgroup_cells, "data/processed/subgroup_three_estimand_study_cells.csv")
write_csv_safe(subgroup_estimates, "data/source_data/Fig2_subgroup_three_estimands.csv")
write_csv_safe(subgroup_contrasts, "data/source_data/Fig3_subgroup_estimand_paired_differences.csv")

# Publication forest matrix: one main row per ecological subgroup, three driver
# columns, and three vertically offset estimand micro-rows in every cell.
plotd <- subgroup_estimates %>% dplyr::mutate(GroupKey = paste(Moderator, Group, sep = "|"),
  Estimand = factor(Estimand, levels = target_labels),
  StressorLabel = factor(StressorLabel, levels = stressor_levels, labels = c("Acidification", "Warming", "Combined")),
  Significance = dplyr::case_when(
    FDR_q_value < .001 ~ "***",
    FDR_q_value < .01 ~ "**",
    FDR_q_value < .05 ~ "*",
    TRUE ~ ""
  ))
make_forest <- function(m) {
  d <- plotd %>% dplyr::filter(Moderator == m)
  if (m == "Taxonomic class") {
    displayed <- d %>% dplyr::group_by(Group) %>% dplyr::summarise(any_estimable=any(is.finite(Estimate)), .groups="drop") %>% dplyr::filter(any_estimable) %>% dplyr::pull(Group)
    d <- d %>% dplyr::filter(Group %in% displayed)
  }
  groups <- if (m == "Life-history stage") rev(stage_levels[stage_levels %in% d$Group]) else rev(unique(d$Group))
  d <- d %>% dplyr::mutate(Group = factor(Group, levels = groups),
    y = as.numeric(Group) + dplyr::recode(as.character(StressorLabel),
      Acidification = 0.22, Warming = 0, Combined = -0.22),
    sig_hjust = -0.25)
  ne <- d %>% dplyr::group_by(Group, Estimand) %>%
    dplyr::summarise(all_ne=all(!is.finite(Estimate)), .groups="drop") %>%
    dplyr::filter(all_ne) %>% dplyr::mutate(y=as.numeric(Group))
  # Journal figures should not let a few very wide prediction intervals squash
  # all pooled estimates. Use shared, symmetric display ranges within each
  # column pair; clipped PIs receive outward arrowheads below.
  x_lim <- if (m %in% c("Taxonomic class", "Ecological guild")) c(-1.25, 1.25) else c(-2.25, 2.25)
  x_breaks <- if (m %in% c("Taxonomic class", "Ecological guild")) c(-1, 0, 1) else c(-2, 0, 2)
  letter_map <- c(`Taxonomic class`="a", `Endpoint category`="b", `Ecological guild`="c", `Life-history stage`="d")
  panel_letter <- if (m %in% names(letter_map)) unname(letter_map[m]) else ""
  strip_labels <- c(`Directional change`=trimws(paste(panel_letter, "Directional")), `Endpoint-aligned response`="Aligned", `Departure from reference`="Absolute")
  d <- d %>% dplyr::mutate(
    PI_plot_low=pmax(PI_low, x_lim[1]), PI_plot_high=pmin(PI_high, x_lim[2]),
    ClipLeft=is.finite(PI_low) & PI_low < x_lim[1], ClipRight=is.finite(PI_high) & PI_high > x_lim[2],
    sig_x=CI_high)
  ggplot2::ggplot(d, ggplot2::aes(Estimate, y, colour = StressorLabel, shape = StressorLabel)) +
    ggplot2::geom_vline(xintercept=0, linetype=2, linewidth=.20, colour="#978DC6") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin=PI_plot_low,xmax=PI_plot_high), orientation="y", width=0, linewidth=.16, colour="#C3BDDF", na.rm=TRUE) +
    ggplot2::geom_text(data=d %>% dplyr::filter(ClipLeft), ggplot2::aes(x=x_lim[1], y=y, label="<"), inherit.aes=FALSE, size=1.8, fontface="bold", colour="#978DC6") +
    ggplot2::geom_text(data=d %>% dplyr::filter(ClipRight), ggplot2::aes(x=x_lim[2], y=y, label=">"), inherit.aes=FALSE, size=1.8, fontface="bold", colour="#978DC6") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin=CI_low,xmax=CI_high), orientation="y", width=0, linewidth=.35, na.rm=TRUE) +
    ggplot2::geom_point(size=1.6, stroke=.24, na.rm=TRUE) +
    ggplot2::geom_text(data=d %>% dplyr::filter(Significance != "", is.finite(sig_x)),
      ggplot2::aes(x=sig_x, y=y, label=Significance, hjust=sig_hjust), inherit.aes=FALSE,
      colour="#3B335F", fontface="bold", size=1.85, na.rm=TRUE) +
    ggplot2::geom_text(data=ne, ggplot2::aes(x=0, y=y, label="NE"), inherit.aes=FALSE, colour="#978DC6", size=1.65) +
    ggplot2::facet_grid(. ~ Estimand, labeller=ggplot2::labeller(Estimand=strip_labels)) +
    ggplot2::scale_y_continuous(breaks=seq_along(groups), labels=if (m=="Life-history stage") unname(stage_labels_1l[groups]) else groups, limits=c(.55,length(groups)+.45), expand=c(0,0)) +
    ggplot2::scale_x_continuous(limits=x_lim, breaks=x_breaks, expand=ggplot2::expansion(mult=c(.03,.10))) +
    ggplot2::scale_colour_manual(values=c(Acidification=driver_pal[["Acidification"]],
      Warming=driver_pal[["Warming"]], Combined=driver_pal[["Warming + acidification"]]),
      breaks=c("Acidification","Warming","Combined"), name="Full database | Driver") +
    ggplot2::scale_shape_manual(values=c(Acidification=16, Warming=17, Combined=15),
      breaks=c("Acidification","Warming","Combined"), name="Full database | Driver") +
    ggplot2::labs(x="lnRR estimate", y=m) + grid_x + theme_publication(base_size=8) +
    ggplot2::theme(panel.grid=ggplot2::element_blank(), axis.text.y=ggplot2::element_text(size=7, colour="#3B335F"), axis.text.x=ggplot2::element_text(size=6.5),
      strip.text=ggplot2::element_text(size=6.5, face="bold"), strip.background=ggplot2::element_blank(),
      legend.position="top", panel.grid.major.y=ggplot2::element_blank(), panel.spacing.x=grid::unit(2.2,"mm"),
      plot.margin=ggplot2::margin(2,3,2,3))
}
forest_panels <- stats::setNames(lapply(names(mods), make_forest), names(mods))
# Four-panel main figure. Phylum is retained in source data and supplementary
# reporting because it is a coarser, non-supported moderator; the main figure
# prioritises class, endpoint category, ecological guild and life-history stage.
p_class3 <- forest_panels[["Taxonomic class"]]
p_endpoint3 <- forest_panels[["Endpoint category"]]
p_guild3 <- forest_panels[["Ecological guild"]]
p_stage3 <- forest_panels[["Life-history stage"]]
# Use a single, non-nested 2 x 2 patchwork. This lets patchwork align panel
# widths globally, so the b/d y axes and a/c y axes share exact x positions.
fig2 <- patchwork::wrap_plots(
    p_class3, p_endpoint3, p_guild3, p_stage3,
    ncol=2, byrow=TRUE, widths=c(1, 1), heights=c(1.42, 1.0),
    guides="collect", axis_titles="collect_x"
  ) &
  ggplot2::theme(legend.position="top", plot.margin=ggplot2::margin(4, 4, 4, 4))
save_figure(fig2, "figures/main/Fig2_ecological_subgroup_responses", 190, 235, dpi=1000)

# Last panel: paired differences with q-value stars and estimates in cells.
cd <- subgroup_contrasts %>%
  dplyr::mutate(
    Contrast = factor(Contrast, levels=c("Endpoint-aligned response - directional change", "Departure - directional change", "Departure - endpoint-aligned response")),
    StressorLabel=factor(StressorLabel, levels=stressor_levels, labels=c("Acidification","Warming","Combined"))
  )
driver_offset <- c(Acidification = 0.22, Warming = 0, Combined = -0.22)
make_difference_forest <- function(m) {
  d <- cd %>% dplyr::filter(Moderator == m)
  groups <- if (m == "Life-history stage") rev(stage_levels[stage_levels %in% d$Group]) else rev(unique(d$Group))
  d <- d %>% dplyr::mutate(
    Group = factor(Group, levels=groups),
    y_plot = as.numeric(Group) + unname(driver_offset[as.character(StressorLabel)]),
    sig_label = dplyr::if_else(is.na(Significance), "", Significance)
  )
  letter_map <- c(`Taxonomic class`="a", `Endpoint category`="b", `Ecological guild`="c", `Life-history stage`="d")
  contrast_labels <- c(
    `Endpoint-aligned response - directional change`=paste0(unname(letter_map[m]),"  Aligned -\ndirectional"),
    `Departure - directional change`="Absolute -\ndirectional",
    `Departure - endpoint-aligned response`="Absolute -\naligned"
  )
  ggplot2::ggplot(d, ggplot2::aes(Estimate, y_plot, colour=StressorLabel, shape=StressorLabel)) +
    ggplot2::geom_vline(xintercept=0, linetype=2, linewidth=.20, colour="#978DC6") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin=CI_low,xmax=CI_high), orientation="y", width=.08, linewidth=.36, na.rm=TRUE) +
    ggplot2::geom_point(size=1.65, stroke=.20, na.rm=TRUE) +
    ggplot2::geom_text(data=d %>% dplyr::filter(sig_label != ""),
      ggplot2::aes(x=CI_high, y=y_plot, label=sig_label), inherit.aes=FALSE,
      colour="#3B335F", fontface="bold", size=1.85, hjust=-.18, na.rm=TRUE) +
    ggplot2::facet_grid(. ~ Contrast, scales="free_x", space="fixed",
      labeller=ggplot2::labeller(Contrast=contrast_labels)) +
    ggplot2::scale_colour_manual(values=c(Acidification=driver_pal[["Acidification"]],
      Warming=driver_pal[["Warming"]], Combined=driver_pal[["Warming + acidification"]]),
      breaks=c("Acidification","Warming","Combined"), name="Full database | Driver") +
    ggplot2::scale_shape_manual(values=c(Acidification=16, Warming=17, Combined=15),
      breaks=c("Acidification","Warming","Combined"), name="Full database | Driver") +
    ggplot2::scale_y_continuous(breaks=seq_along(groups), labels=if (m=="Life-history stage") unname(stage_labels_1l[groups]) else groups,
      limits=c(.55,length(groups)+.45), expand=c(0,0)) +
    ggplot2::scale_x_continuous(breaks=scales::breaks_extended(n=3), expand=ggplot2::expansion(mult=c(.05,.20))) +
    ggplot2::labs(x="Paired difference (95% bootstrap CI)", y=m) +
    grid_x + theme_publication(base_size=8) +
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_text(size=7), axis.text.x=ggplot2::element_text(size=6.5),
      strip.text=ggplot2::element_text(size=6.5, face="bold"), legend.position="top",
      panel.spacing.x=grid::unit(4.5,"mm"), plot.margin=ggplot2::margin(2,3,2,3))
}
diff_panels <- stats::setNames(lapply(c("Taxonomic class", "Endpoint category", "Ecological guild", "Life-history stage"), make_difference_forest),
                               c("Taxonomic class", "Endpoint category", "Ecological guild", "Life-history stage"))
fig4 <- patchwork::wrap_plots(diff_panels, ncol=2, byrow=TRUE,
  widths=c(1,1), heights=c(1.42,1.0), guides="collect",
  axis_titles="collect_x") &
  ggplot2::theme(legend.position="top", plot.margin=ggplot2::margin(4,4,4,4))
save_figure(fig4, "figures/main/Fig4_driver_estimand_and_subgroup_comparison", 190, 235, dpi=1000)
writeLines(c("Computed all three estimands for taxonomic phylum/class, ecological guild, endpoint category and life stage subgroups.", "All targets use the same direction-interpretable rows and signed StudyID-cell inverse-variance aggregation; absolute magnitude is taken only after aggregation.", "Paired contrasts use 9999-resample centered-null bootstrap and an independent BH-FDR family across estimable subgroup-by-stressor contrasts.", "NE means fewer than three independent StudyID cells and is insufficient for inference."), file.path(root_dir,"analysis_logs","subgroup_three_estimands_render_log.txt"))
