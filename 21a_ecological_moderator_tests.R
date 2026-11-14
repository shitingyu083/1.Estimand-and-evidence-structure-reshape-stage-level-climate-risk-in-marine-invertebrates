# Exploratory ecological moderator meta-regressions. The analysis families and
# eligibility rule are declared here before model fitting: five moderators x
# three drivers, with at least three independent StudyID clusters per level.
if (!exists("root_dir")) source(file.path("R", "00_setup.R"))

moderator_dat <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "analysis_ready_with_taxonomy_ecological_guild.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  dplyr::filter(
    is.finite(lnRR), is.finite(vi_model), vi_model > 0, !is.na(StudyID),
    StressorLabel %in% stressor_levels
  )

moderator_spec <- tibble::tribble(
  ~Moderator, ~Variable, ~EcologicalQuestion, ~ExcludedLevels,
  "Phylum", "Phylum", "Does directional response vary across major evolutionary lineages?", "",
  "Taxonomic class", "Class", "Does directional response vary among taxonomic classes?", "",
  "Ecological guild", "EcologicalGuild", "Does directional response vary with adult mobility and habitat coupling?", "Mixed-uncertain",
  "Endpoint pathway", "Category", "Does directional response vary among organismal response pathways?", "Biodiversity",
  "Life-history stage", "ObservedStage", "Does directional response vary among recruitment and adult life-history windows?", ""
)

fit_level_summary <- function(x, moderator, level, stressor, question) {
  cell_dat <- x %>%
    dplyr::group_by(StudyID) %>%
    dplyr::summarise(
      yi = sum(lnRR / vi_model) / sum(1 / vi_model),
      vi = 1 / sum(1 / vi_model),
      RawEffectSizeCount = dplyr::n(),
      SpeciesCount = dplyr::n_distinct(SpeciesID),
      .groups = "drop"
    )
  fit <- tryCatch(
    metafor::rma.uni(yi = cell_dat$yi, vi = cell_dat$vi, method = "REML"),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(tibble::tibble(
      Moderator = moderator, Level = level, StressorLabel = stressor,
      EcologicalQuestion = question, Estimate = NA_real_, SE = NA_real_,
      CI_low = NA_real_, CI_high = NA_real_, P_value = NA_real_,
      EffectSizeCount = sum(cell_dat$RawEffectSizeCount), StudyCount = nrow(cell_dat),
      SpeciesCount = sum(cell_dat$SpeciesCount)
    ))
  }
  tibble::tibble(
    Moderator = moderator, Level = level, StressorLabel = stressor,
    EcologicalQuestion = question, Estimate = as.numeric(fit$b), SE = as.numeric(fit$se),
    CI_low = as.numeric(fit$ci.lb), CI_high = as.numeric(fit$ci.ub),
    P_value = as.numeric(fit$pval),
    EffectSizeCount = sum(cell_dat$RawEffectSizeCount), StudyCount = nrow(cell_dat),
    SpeciesCount = sum(cell_dat$SpeciesCount)
  )
}

fit_moderator <- function(data, spec_row, stressor) {
  moderator <- spec_row$Moderator[[1]]
  variable <- spec_row$Variable[[1]]
  question <- spec_row$EcologicalQuestion[[1]]
  excluded <- strsplit(spec_row$ExcludedLevels[[1]], "\\|", fixed = FALSE)[[1]]
  excluded <- excluded[nzchar(excluded)]
  x <- data %>%
    dplyr::filter(StressorLabel == stressor) %>%
    dplyr::mutate(Level = stringr::str_squish(as.character(.data[[variable]]))) %>%
    dplyr::filter(!is.na(Level), Level != "", !(Level %in% excluded))
  counts <- x %>%
    dplyr::group_by(Level) %>%
    dplyr::summarise(
      EffectSizeCount = dplyr::n(), StudyCount = dplyr::n_distinct(StudyID),
      SpeciesCount = dplyr::n_distinct(SpeciesID), .groups = "drop"
    )
  eligible <- counts %>% dplyr::filter(StudyCount >= 3L) %>% dplyr::pull(Level)
  x <- x %>% dplyr::filter(Level %in% eligible)

  level_results <- dplyr::bind_rows(lapply(sort(eligible), function(level) {
    fit_level_summary(x %>% dplyr::filter(Level == level), moderator, level, stressor, question)
  }))

  if (length(eligible) < 2L) {
    omnibus <- tibble::tibble(
      Moderator = moderator, StressorLabel = stressor, EcologicalQuestion = question,
      QM = NA_real_, df_num = NA_integer_, df_denom = NA_real_, P_value = NA_real_,
      EligibleLevelCount = length(eligible),
      StudyCount = dplyr::n_distinct(x$StudyID), StudyLevelCellCount = 0L,
      EffectSizeCount = nrow(x), Model = "Not estimable: fewer than two eligible levels"
    )
    return(list(levels = level_results, omnibus = omnibus))
  }

  cells <- x %>%
    dplyr::group_by(StudyID, Level) %>%
    dplyr::summarise(
      yi = sum(lnRR / vi_model) / sum(1 / vi_model),
      vi = 1 / sum(1 / vi_model), RawEffectSizeCount = dplyr::n(), .groups = "drop"
    ) %>%
    dplyr::mutate(Level = stats::relevel(factor(Level), ref = sort(eligible)[1]))
  fit <- tryCatch(
    metafor::rma.mv(yi = yi, V = vi, mods = ~ Level, random = ~ 1 | StudyID,
                    method = "REML", data = cells),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    qm <- df_num <- df_denom <- p <- NA_real_
    model_label <- "Not estimable"
  } else {
    robust_fit <- tryCatch(
      metafor::robust(fit, cluster = cells$StudyID, adjust = TRUE),
      error = function(e) NULL
    )
    if (is.null(robust_fit)) {
      qm <- df_num <- df_denom <- p <- NA_real_
      model_label <- "Cluster-robust moderator test failed"
    } else {
      qm <- as.numeric(robust_fit$QM)
      df_num <- as.integer(robust_fit$QMdf[1])
      df_denom <- as.numeric(robust_fit$QMdf[2])
      p <- as.numeric(robust_fit$QMp)
      model_label <- paste(
        "StudyID random-intercept REML meta-regression;",
        "StudyID-cluster sandwich-robust omnibus F test with finite-sample adjustment"
      )
    }
  }
  omnibus <- tibble::tibble(
    Moderator = moderator, StressorLabel = stressor, EcologicalQuestion = question,
    QM = qm, df_num = df_num, df_denom = df_denom, P_value = p,
    EligibleLevelCount = length(eligible),
    StudyCount = dplyr::n_distinct(cells$StudyID), StudyLevelCellCount = nrow(cells),
    EffectSizeCount = sum(cells$RawEffectSizeCount), Model = model_label
  )
  list(levels = level_results, omnibus = omnibus)
}

moderator_results <- lapply(seq_len(nrow(moderator_spec)), function(i) {
  lapply(stressor_levels, function(stressor) fit_moderator(moderator_dat, moderator_spec[i, ], stressor))
})
moderator_results <- unlist(moderator_results, recursive = FALSE)

moderator_levels <- dplyr::bind_rows(lapply(moderator_results, `[[`, "levels")) %>%
  dplyr::group_by(Moderator, StressorLabel) %>%
  dplyr::mutate(LevelFDR_q_value = stats::p.adjust(P_value, method = "BH")) %>%
  dplyr::ungroup()

# The 15 omnibus tests are one explicitly defined exploratory BH-FDR family.
moderator_omnibus <- dplyr::bind_rows(lapply(moderator_results, `[[`, "omnibus")) %>%
  dplyr::mutate(
    FDRFamily = "All 15 prespecified moderator-by-driver omnibus tests",
    FDR_q_value = stats::p.adjust(P_value, method = "BH"),
    SignificantFDR = !is.na(FDR_q_value) & FDR_q_value < 0.05,
    Significance = dplyr::case_when(
      is.na(FDR_q_value) ~ "NE",
      FDR_q_value < 0.001 ~ "***",
      FDR_q_value < 0.01 ~ "**",
      FDR_q_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

write_csv_safe(moderator_levels, "data/processed/exploratory_moderator_level_estimates.csv")
write_csv_safe(moderator_omnibus, "data/processed/exploratory_moderator_omnibus_tests.csv")
write_csv_safe(moderator_levels, "data/source_data/SupplementaryFigS9_moderator_level_estimates.csv")
write_csv_safe(moderator_omnibus, "data/source_data/Fig3e_moderator_omnibus_tests.csv")

plot_level_heatmap <- function(data, moderator, y_title) {
  dd <- data %>%
    dplyr::filter(Moderator == moderator) %>%
    dplyr::mutate(
      Level = if (moderator == "Life-history stage") {
        factor(dplyr::recode(Level, EMBRYO = "Embryo", LARVA = "Larva",
                             `SETTLEMENT/METAMORPHOSIS` = "Settlement/metamorphosis",
                             `JUVENILE/ADULT` = "Juvenile/adult"),
               levels = rev(c("Embryo", "Larva", "Settlement/metamorphosis", "Juvenile/adult")))
      } else factor(Level, levels = rev(sort(unique(Level)))),
      StressorLabel = factor(StressorLabel, levels = stressor_levels,
                             labels = c("Acidification", "Warming", "Combined")),
      Label = sprintf("%.2f", Estimate)
    )
  lim <- max(abs(c(dd$CI_low, dd$CI_high)), na.rm = TRUE)
  ggplot2::ggplot(dd, ggplot2::aes(StressorLabel, Level, fill = Estimate)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = Label), size = 1.75) +
    ggplot2::scale_fill_gradient2(
      low = "#0072B2", mid = "#F4F2F9", high = "#D55E00", midpoint = 0,
      limits = c(-lim, lim), name = "Directional\nlnRR"
    ) +
    ggplot2::labs(x = "Driver", y = y_title) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 5.7), legend.position = "right"
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) + ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::coord_fixed()
}

moderator_order <- moderator_spec$Moderator
plot_omnibus <- moderator_omnibus %>%
  dplyr::mutate(
    Moderator = factor(Moderator, levels = rev(moderator_order)),
    StressorLabel = factor(StressorLabel, levels = stressor_levels,
                           labels = c("Acidification", "Warming", "Combined")),
    NegLogQ = ifelse(is.na(FDR_q_value), NA_real_, -log10(pmax(FDR_q_value, 1e-6))),
    Label = dplyr::case_when(
      is.na(FDR_q_value) ~ "NE",
      Significance != "" ~ Significance,
      TRUE ~ "ns"
    )
  )
p_omnibus <- ggplot2::ggplot(plot_omnibus, ggplot2::aes(StressorLabel, Moderator, fill = NegLogQ)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
  ggplot2::geom_text(ggplot2::aes(label = Label, fontface = ifelse(SignificantFDR, "bold", "plain")),
                     size = 1.75, lineheight = 0.88) +
  ggplot2::scale_discrete_identity(aesthetics = "fontface") +
  ggplot2::scale_fill_gradient(low = "#F4F2F9", high = "#6F63A8", na.value = "#EAE7F3",
                               name = expression(-log[10](q))) +
  ggplot2::labs(x = "Driver", y = "Moderator") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 5.7), legend.position = "right"
  ) +
  ggplot2::scale_x_discrete(expand = c(0, 0)) + ggplot2::scale_y_discrete(expand = c(0, 0)) +
  ggplot2::coord_fixed()

p_phylum <- plot_level_heatmap(moderator_levels, "Phylum", "Phylum")
p_endpoint <- plot_level_heatmap(moderator_levels, "Endpoint pathway", "Endpoint category")
p_stage <- plot_level_heatmap(moderator_levels, "Life-history stage", "Observed stage")
s9 <- (p_phylum | p_endpoint) / (p_stage | p_omnibus) +
  patchwork::plot_annotation(tag_levels = "a")
save_figure(s9, "figures/extended/SupplementaryFigS7_ecological_moderator_meta", 190, 178, dpi = 1000)

writeLines(
  c(
    "Exploratory moderator family: Phylum, taxonomic class, ecological guild, endpoint pathway and life-history stage x three drivers (15 omnibus tests).",
    "Eligibility: at least three independent StudyID clusters per moderator level and at least two eligible levels per omnibus test.",
    "Models: inverse-variance aggregation within StudyID x moderator level followed by StudyID random-intercept REML meta-regression and StudyID-cluster sandwich-robust omnibus F tests with finite-sample adjustment.",
    "Multiplicity: one BH-FDR adjustment across all 15 prespecified omnibus tests; level summaries are descriptive context."
  ),
  file.path(root_dir, "analysis_logs", "ecological_moderator_tests_log.txt")
)
