# Endpoint-direction coding, robust variance estimation and estimand sensitivities.
source(file.path("R", "00_setup.R"))
suppressPackageStartupMessages(library(robumeta))

coded_dat <- readr::read_csv(
  file.path(root_dir, "data", "processed", "analysis_ready_model_dataset.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  mutate(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    endpoint_text = stringr::str_to_lower(stringr::str_squish(
      paste(Metric, TransformedMetric, Category, LifeStage, sep = " | ")
    )),
    EndpointDirectionClassStrict = dplyr::case_when(
      stringr::str_detect(
        endpoint_text,
        "mortality|\\bdead\\b|\\bdeath\\b|abnormal|deform|malformation|delay|time to|bleach|lesion|damage|apoptosis|necrosis"
      ) ~ "adverse_when_increases",
      stringr::str_detect(
        endpoint_text,
        "survival|viability|hatching success|hatch success|settlement success|metamorph.*success|fertiliz.*success|fecundity|egg production|reproductive success"
      ) ~ "beneficial_when_increases",
      Category == "Growth" & !stringr::str_detect(
        endpoint_text,
        "condition|ratio|allometr|shape|thickness|diameter|length|size|weight|biomass"
      ) ~ "beneficial_when_increases",
      Category == "Growth" & stringr::str_detect(
        endpoint_text,
        "growth|biomass|weight|length|size|diameter|condition index|condition factor|hatching size|test thickness"
      ) ~ "beneficial_when_increases",
      Category == "Calcification" & stringr::str_detect(
        endpoint_text,
        "calcification rate|net calcification|calcified mass|\\bgnet\\b"
      ) & !stringr::str_detect(
        endpoint_text,
        "proportion|ratio|peak intensity|aragonite|calcite:|content in shell"
      ) ~ "beneficial_when_increases",
      TRUE ~ "context_dependent"
    ),
    aligned_lnRR_strict = dplyr::case_when(
      EndpointDirectionClassStrict == "beneficial_when_increases" ~ lnRR,
      EndpointDirectionClassStrict == "adverse_when_increases" ~ -lnRR,
      TRUE ~ NA_real_
    )
  )

coding_summary <- coded_dat %>%
  count(Category, EndpointDirectionClassStrict, name = "EffectSizeCount") %>%
  tidyr::complete(
    Category,
    EndpointDirectionClassStrict = c(
      "beneficial_when_increases", "adverse_when_increases", "context_dependent"
    ),
    fill = list(EffectSizeCount = 0L)
  ) %>%
  arrange(Category, EndpointDirectionClassStrict)
write_csv_safe(coding_summary, "data/processed/ER_endpoint_direction_summary.csv")

row_coding <- coded_dat %>%
  transmute(
    StudyID, SpeciesID, ObservedStage, StressorLabel, Category, Metric,
    TransformedMetric, EndpointDirectionClassStrict, lnRR,
    aligned_lnRR_strict, vi_model
  )
write_csv_safe(row_coding, "data/processed/ER_endpoint_direction_row_coding.csv")

rve_base <- coded_dat %>%
  filter(
    DevelopmentalModeGroup == "With metamorphic stage",
    is.finite(lnRR), is.finite(vi_model), vi_model > 0, !is.na(StudyID),
    !is.na(ObservedStage)
  ) %>%
  mutate(ObservedStage = factor(ObservedStage, levels = stage_levels))

rve_fit_cache <- new.env(parent = emptyenv())
get_robu_fit <- function(d, yi_col, rho) {
  key <- paste(yi_col, format(rho, nsmall = 1), sep = "__")
  if (exists(key, envir = rve_fit_cache, inherits = FALSE)) {
    return(get(key, envir = rve_fit_cache, inherits = FALSE))
  }
  dd_all <- d %>% filter(is.finite(.data[[yi_col]]))
  eligible_stages <- dd_all %>%
    distinct(ObservedStage, StudyID) %>%
    count(ObservedStage, name = "StudyCount") %>%
    filter(StudyCount >= 3L) %>%
    pull(ObservedStage) %>%
    as.character()
  dd <- dd_all %>%
    filter(as.character(ObservedStage) %in% eligible_stages) %>%
    mutate(ObservedStage = droplevels(ObservedStage))
  fit <- tryCatch(
    robumeta::robu(
      stats::as.formula(paste0(yi_col, " ~ 0 + ObservedStage")),
      data = dd, studynum = StudyID, var.eff.size = vi_model,
      modelweights = "CORR", rho = rho, small = TRUE
    ),
    error = function(e) NULL
  )
  assign(key, list(fit = fit, data = dd_all, fit_data = dd),
         envir = rve_fit_cache)
  list(fit = fit, data = dd_all, fit_data = dd)
}

run_robu_stage <- function(d, yi_col, rho, estimand) {
  cached <- get_robu_fit(d, yi_col, rho)
  fit <- cached$fit
  dd <- cached$data
  if (is.null(fit)) return(tibble::tibble())
  tab <- tibble::as_tibble(fit$reg_table)
  if (nrow(tab) == length(stage_levels)) {
    tab$ObservedStage <- stage_levels
  } else {
    lab <- sub("^ObservedStage", "", tab$labels)
    tab$ObservedStage <- stage_levels[match(lab, make.names(stage_levels))]
  }
  inferential <- tab %>%
    transmute(
      Estimand = estimand, Rho = rho, ObservedStage,
      Estimate = b.r, SE, df = dfs, CI_low = CI.L, CI_high = CI.U,
      P_value = prob,
      EffectSizeCount = vapply(ObservedStage, function(st) {
        sum(dd$ObservedStage == st)
      }, integer(1)),
      StudyCount = vapply(ObservedStage, function(st) {
        dplyr::n_distinct(dd$StudyID[dd$ObservedStage == st])
      }, integer(1))
    ) %>%
    mutate(
      across(c(SE, df, CI_low, CI_high, P_value), ~ if_else(StudyCount < 3L, NA_real_, .x)),
      FDR_q_value = p.adjust(P_value, method = "BH")
    )
  excluded <- setdiff(stage_levels, as.character(inferential$ObservedStage))
  descriptive <- dplyr::bind_rows(lapply(excluded, function(st) {
    x <- dd %>% filter(as.character(ObservedStage) == st)
    if (!nrow(x)) return(NULL)
    tibble::tibble(
      Estimand = estimand, Rho = rho, ObservedStage = st,
      Estimate = sum(x[[yi_col]] / x$vi_model) / sum(1 / x$vi_model),
      SE = NA_real_, df = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
      P_value = NA_real_, EffectSizeCount = nrow(x),
      StudyCount = dplyr::n_distinct(x$StudyID), FDR_q_value = NA_real_
    )
  }))
  bind_rows(inferential, descriptive) %>%
    mutate(ObservedStage = factor(ObservedStage, levels = stage_levels)) %>%
    arrange(ObservedStage) %>%
    mutate(ObservedStage = as.character(ObservedStage))
}

run_robu_contrasts <- function(d, yi_col, rho, estimand) {
  cached <- get_robu_fit(d, yi_col, rho)
  fit <- cached$fit
  if (is.null(fit)) return(tibble::tibble())
  tab <- tibble::as_tibble(fit$reg_table)
  fit_stages <- levels(cached$fit_data$ObservedStage)
  out <- list()
  z <- 0L
  for (i in seq_len(length(fit_stages) - 1L)) {
    for (j in (i + 1L):length(fit_stages)) {
      contrast_vector <- rep(0, length(fit_stages))
      contrast_vector[i] <- -1
      contrast_vector[j] <- 1
      estimate <- sum(contrast_vector * fit$b.r)
      se <- sqrt(drop(t(contrast_vector) %*% fit$VR.r %*% contrast_vector))
      # A conservative small-sample approximation for a linear combination:
      # use the smaller coefficient-specific degrees of freedom.
      df <- min(tab$dfs[c(i, j)], na.rm = TRUE)
      t_value <- estimate / se
      p_value <- 2 * stats::pt(-abs(t_value), df = df)
      crit <- stats::qt(0.975, df = df)
      z <- z + 1L
      out[[z]] <- tibble::tibble(
        Estimand = estimand, Rho = rho,
        ReferenceStage = fit_stages[i], ComparisonStage = fit_stages[j],
        Contrast = estimate, SE = se, df = df,
        CI_low = estimate - crit * se, CI_high = estimate + crit * se,
        P_value = p_value
      )
    }
  }
  counts <- cached$data %>%
    count(ObservedStage, name = "EffectSizeCount") %>%
    left_join(
      cached$fit_data %>% distinct(ObservedStage, StudyID) %>%
        count(ObservedStage, name = "StudyCount"),
      by = "ObservedStage"
    )
  dplyr::bind_rows(out) %>%
    left_join(counts %>% select(ReferenceStage = ObservedStage,
                               ReferenceStudyCount = StudyCount),
              by = "ReferenceStage") %>%
    left_join(counts %>% select(ComparisonStage = ObservedStage,
                               ComparisonStudyCount = StudyCount),
              by = "ComparisonStage") %>%
    mutate(
      across(c(SE, df, CI_low, CI_high, P_value),
             ~ if_else(pmin(ReferenceStudyCount, ComparisonStudyCount) < 3L,
                       NA_real_, .x)),
      FDR_q_value = p.adjust(P_value, method = "BH")
    )
}

rhos <- c(0, 0.5, 0.8)
rve_stage <- dplyr::bind_rows(lapply(rhos, function(rho) {
  dplyr::bind_rows(
    run_robu_stage(rve_base, "lnRR", rho, "Raw directional lnRR"),
    run_robu_stage(
      rve_base, "aligned_lnRR_strict", rho,
      "Conservatively aligned performance lnRR"
    )
  )
}))
rve_contrasts <- dplyr::bind_rows(lapply(rhos, function(rho) {
  dplyr::bind_rows(
    run_robu_contrasts(rve_base, "lnRR", rho, "Raw directional lnRR"),
    run_robu_contrasts(
      rve_base, "aligned_lnRR_strict", rho,
      "Conservatively aligned performance lnRR"
    )
  )
}))
write_csv_safe(rve_stage, "data/processed/ER_RVE_stage_estimates.csv")
write_csv_safe(rve_contrasts, "data/processed/ER_RVE_stage_contrasts.csv")

aggregate_study_stage <- function(d, yi_col = "lnRR") {
  d %>%
    filter(is.finite(.data[[yi_col]]), is.finite(vi_model), vi_model > 0) %>%
    group_by(ObservedStage, StudyID) %>%
    summarise(
      yi = sum(.data[[yi_col]] / vi_model) / sum(1 / vi_model),
      vi = 1 / sum(1 / vi_model), RawEffectSizeCount = n(), .groups = "drop"
    ) %>%
    filter(is.finite(yi), is.finite(vi), vi > 0)
}

extract_uni <- function(x, transform = identity) {
  if (nrow(x) < 3L) return(tibble::tibble(
    Estimate = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
    StudyCount = nrow(x), EffectSizeCount = sum(x$RawEffectSizeCount)
  ))
  y <- transform(x$yi)
  fit <- metafor::rma.uni(yi = y, vi = x$vi, method = "REML")
  tibble::tibble(
    Estimate = as.numeric(fit$b), CI_low = fit$ci.lb, CI_high = fit$ci.ub,
    StudyCount = nrow(x), EffectSizeCount = sum(x$RawEffectSizeCount)
  )
}

bootstrap_stat <- function(x, stat, reps = 4000L, seed = 20260727L) {
  set.seed(seed)
  vals <- replicate(reps, stat(x[sample.int(nrow(x), nrow(x), replace = TRUE), , drop = FALSE]))
  c(Estimate = stat(x), CI_low = unname(stats::quantile(vals, 0.025, na.rm = TRUE)),
    CI_high = unname(stats::quantile(vals, 0.975, na.rm = TRUE)))
}

median_abs <- function(x) stats::median(abs(x$yi), na.rm = TRUE)
noise_corrected_rms <- function(x) {
  sqrt(max(mean(x$yi^2 - x$vi, na.rm = TRUE), 0))
}

make_magnitude_rows <- function(d, evidence_set, seed_offset = 0L) {
  signed_agg <- aggregate_study_stage(d, "lnRR")
  endpoint_abs_agg <- aggregate_study_stage(d %>% mutate(abs_endpoint = abs(lnRR)), "abs_endpoint")
  dplyr::bind_rows(lapply(seq_along(stage_levels), function(i) {
    st <- stage_levels[i]
    sa <- signed_agg %>% filter(ObservedStage == st)
    ea <- endpoint_abs_agg %>% filter(ObservedStage == st)
    model_endpoint <- extract_uni(ea)
    model_agg_abs <- extract_uni(sa, abs)
    med <- bootstrap_stat(sa, median_abs, seed = 20260727L + seed_offset + i)
    rms <- bootstrap_stat(sa, noise_corrected_rms, seed = 20261727L + seed_offset + i)
    dplyr::bind_rows(
      model_endpoint %>% mutate(Definition = "Endpoint-level |lnRR| averaged within study-stage"),
      model_agg_abs %>% mutate(Definition = "Absolute signed study-stage aggregate"),
      tibble::tibble(
        Estimate = med[["Estimate"]], CI_low = med[["CI_low"]],
        CI_high = med[["CI_high"]], StudyCount = nrow(sa),
        EffectSizeCount = sum(sa$RawEffectSizeCount),
        Definition = "Median absolute signed study-stage aggregate"
      ),
      tibble::tibble(
        Estimate = rms[["Estimate"]], CI_low = rms[["CI_low"]],
        CI_high = rms[["CI_high"]], StudyCount = nrow(sa),
        EffectSizeCount = sum(sa$RawEffectSizeCount),
        Definition = "Noise-corrected RMS signed study-stage aggregate"
      )
    ) %>%
      mutate(EvidenceSet = evidence_set, ObservedStage = st)
  })) %>%
    group_by(EvidenceSet, Definition) %>%
    mutate(StageRank = min_rank(desc(Estimate))) %>%
    ungroup() %>%
    select(EvidenceSet, Definition, ObservedStage, Estimate, CI_low, CI_high,
           StageRank, StudyCount, EffectSizeCount)
}

full_mag_data <- rve_base
development_mag_data <- rve_base %>% filter(Category == "Development")
common_mag_data <- rve_base %>% filter(is.finite(aligned_lnRR_strict))
magnitude_sensitivity <- bind_rows(
  make_magnitude_rows(full_mag_data, "All eligible endpoints", 0L),
  make_magnitude_rows(development_mag_data, "Development endpoints only", 100L),
  make_magnitude_rows(common_mag_data, "Conservatively direction-interpretable endpoints", 200L)
)
write_csv_safe(
  magnitude_sensitivity,
  "data/processed/ER_magnitude_definition_sensitivity.csv"
)

fit_stage_signed <- function(d, evidence_set) {
  agg <- aggregate_study_stage(d, "lnRR")
  bind_rows(lapply(stage_levels, function(st) {
    x <- agg %>% filter(ObservedStage == st)
    extract_uni(x) %>% mutate(ObservedStage = st, EvidenceSet = evidence_set)
  }))
}
development_only <- fit_stage_signed(
  development_mag_data, "Development endpoints only: directional lnRR"
) %>%
  mutate(FDR_q_value = p.adjust(
    2 * stats::pnorm(-abs(Estimate / ((CI_high - CI_low) / (2 * qnorm(0.975))))),
    method = "BH"
  ))
write_csv_safe(development_only, "data/processed/ER_development_only_sensitivity.csv")

study_overlap <- rve_base %>%
  distinct(StudyID, ObservedStage) %>%
  count(StudyID, name = "StageCount") %>%
  count(StageCount, name = "StudyCount")
write_csv_safe(study_overlap, "data/processed/ER_study_stage_overlap.csv")
rve08 <- rve_stage %>% filter(Rho == 0.8)
stage_axis <- function(x) factor(x, levels = rev(stage_levels))

estimand_forest <- function(d, title, xlab, accent, subtitle = NULL) {
  dd <- d %>% mutate(ObservedStage = stage_axis(ObservedStage))
  ggplot(dd, aes(Estimate, ObservedStage)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35,
               colour = col("grey")) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y",
                  width = 0, linewidth = 0.85, colour = accent, na.rm = TRUE) +
    geom_point(aes(alpha = StudyCount >= 3), shape = 21, size = 2.5,
               fill = accent, colour = "white", stroke = 0.45) +
    geom_text(
      data = dd %>% filter(StudyCount < 3),
      aes(label = paste0("s=", StudyCount, "; NE")),
      x = 0.050, hjust = 0, size = 5.8 / .pt, colour = col("grey_dark")
    ) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
    scale_y_discrete(labels = stage_labels_1l) +
    labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    grid_x +
    theme(legend.position = "none", plot.margin = margin(6, 8, 6, 5),
          plot.title = element_text(size = er_base_size + 0.1, face = "bold"))
}

p2a_estimand <- estimand_forest(
  rve08 %>% filter(Estimand == "Raw directional lnRR"),
  "Embryo shows the clearest signed decrease",
  "Study-cluster RVE lnRR (95% CI; rho = 0.8)", col("red")
)
p2b_estimand <- estimand_forest(
  rve08 %>% filter(Estimand == "Conservatively aligned performance lnRR"),
  "No aligned-performance hierarchy",
  "Aligned RVE lnRR (95% CI; rho = 0.8)", col("green"),
  "Settlement/metamorphosis has only one contributing study"
)

mag_methods <- c(
  "Endpoint-level |lnRR| averaged within study-stage",
  "Absolute signed study-stage aggregate",
  "Median absolute signed study-stage aggregate",
  "Noise-corrected RMS signed study-stage aggregate"
)
mag_method_labels <- c(
  "Endpoint |lnRR| mean", "|signed study-stage mean|",
  "Median |study-stage mean|", "Noise-corrected RMS"
)
mag_cols <- c(col("blue"), col("purple"), col("amber"), col("grey_dark"))
mag_full <- magnitude_sensitivity %>%
  filter(EvidenceSet == "All eligible endpoints", Definition %in% mag_methods) %>%
  mutate(
    ObservedStage = stage_axis(ObservedStage),
    Definition = factor(Definition, levels = mag_methods)
  )
write_csv_safe(mag_full, "data/source_data/Fig2c_magnitude_definition_sensitivity.csv")
p2c_estimand <- ggplot(
  mag_full,
  aes(Estimate, ObservedStage, colour = Definition, group = Definition)
) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high), orientation = "y",
    position = position_dodge(width = 0.58), width = 0, linewidth = 0.55
  ) +
  geom_point(position = position_dodge(width = 0.58), size = 2.1) +
  scale_colour_manual(values = mag_cols, labels = mag_method_labels, name = NULL) +
  scale_y_discrete(labels = stage_labels_1l) +
  labs(
    title = "Transformation order changes magnitude",
    subtitle = "Exploratory summaries; intervals are not tests against zero",
    x = "Departure from control", y = NULL
  ) +
  grid_x +
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +
  theme(legend.position = "bottom", legend.key.width = unit(3.5, "mm"),
        plot.margin = margin(6, 8, 6, 5))

rank_rows <- bind_rows(
  rve08 %>% filter(Estimand == "Raw directional lnRR") %>%
    mutate(Method = "Raw directional RVE", Rank = min_rank(Estimate),
           Estimable = StudyCount >= 3) %>% select(Method, ObservedStage, Rank, Estimable),
  rve08 %>% filter(Estimand == "Conservatively aligned performance lnRR") %>%
    mutate(Method = "Aligned performance RVE", Rank = if_else(StudyCount >= 3,
                                                               min_rank(Estimate), NA_integer_),
           Estimable = StudyCount >= 3) %>% select(Method, ObservedStage, Rank, Estimable),
  magnitude_sensitivity %>%
    filter(Definition == "Absolute signed study-stage aggregate") %>%
    mutate(
      Method = recode(
        EvidenceSet,
        "All eligible endpoints" = "All endpoints: magnitude",
        "Development endpoints only" = "Development only: magnitude",
        "Conservatively direction-interpretable endpoints" = "Interpretable only: magnitude"
      ),
      Rank = as.integer(StageRank), Estimable = StudyCount >= 3
    ) %>% select(Method, ObservedStage, Rank, Estimable)
) %>%
  mutate(
    Method = factor(Method, levels = rev(c(
      "Raw directional RVE", "Aligned performance RVE",
      "All endpoints: magnitude", "Development only: magnitude",
      "Interpretable only: magnitude"
    ))),
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    RankLabel = if_else(Estimable & !is.na(Rank), as.character(Rank), "NE"),
    RankFactor = factor(Rank, levels = 1:4)
  )
write_csv_safe(rank_rows, "data/source_data/Fig2d_stage_rank_sensitivity.csv")
p2d_estimand <- ggplot(rank_rows, aes(ObservedStage, Method, fill = RankFactor)) +
  geom_tile(colour = "white", linewidth = 0.75) +
  geom_text(aes(label = RankLabel), size = 6.0 / .pt,
            colour = col("black"), fontface = "bold") +
  scale_fill_manual(
    values = c("1" = "#DCEAF2", "2" = "#E8EEF1", "3" = "#F2E9E6", "4" = "#F3DDD5"),
    limits = as.character(1:4), drop = FALSE, na.value = col("grey_light"), name = "Rank"
  ) +
  scale_x_discrete(labels = c(
    "EMBRYO" = "Embryo", "LARVA" = "Larva",
    "SETTLEMENT/METAMORPHOSIS" = "Settlement/\nmetamorphosis",
    "JUVENILE/ADULT" = "Juvenile/\nadult"
  ), position = "top") +
  labs(
    title = "No stable top-ranked stage",
    subtitle = "1 = strongest signal; NE = not estimable (<3 studies)",
    x = NULL, y = NULL
  ) +
  theme(
    panel.grid = element_blank(), axis.line = element_blank(),
    axis.ticks = element_blank(), axis.text.x = element_text(lineheight = 0.9),
    legend.position = "right", legend.key.height = unit(3.4, "mm"),
    plot.margin = margin(6, 8, 6, 5)
  )

fig2_estimand <- (p2a_estimand | p2b_estimand) / (p2c_estimand | p2d_estimand) +
  patchwork::plot_layout(heights = c(1, 1.12)) +
  patchwork::plot_annotation(tag_levels = "a")
save_figure(
  fig2_estimand,
  "figures/main/Fig2_directional_and_absolute_stage_responses", 190, 154
)

# Figure 4: dependence, common-endpoint and contrast boundaries.
rve_rho_plot <- rve_stage %>%
  filter(StudyCount >= 3) %>%
  mutate(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    Estimand = factor(
      Estimand,
      levels = c("Raw directional lnRR", "Conservatively aligned performance lnRR"),
      labels = c("Raw directional", "Aligned performance")
    )
  )
write_csv_safe(rve_rho_plot, "data/source_data/Fig4a_RVE_rho_sensitivity.csv")
p4a_robustness <- ggplot(rve_rho_plot, aes(Rho, Estimate, colour = ObservedStage)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3,
             colour = col("grey")) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.8) +
  facet_wrap(~ Estimand, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = stage_pal, labels = stage_labels_1l, name = NULL) +
  scale_x_continuous(breaks = rhos) +
  labs(title = "Dependence sensitivity", subtitle = "Correlated-effects RVE",
       x = "Assumed within-study correlation (rho)", y = "lnRR") +
  theme(legend.position = "bottom", legend.key.width = unit(3.5, "mm"),
        plot.margin = margin(6, 8, 6, 5))

dev_plot <- development_only %>% mutate(ObservedStage = stage_axis(ObservedStage))
write_csv_safe(dev_plot, "data/source_data/Fig4b_development_only_directional.csv")
p4b_robustness <- ggplot(dev_plot, aes(Estimate, ObservedStage)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35,
             colour = col("grey")) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y",
                width = 0, linewidth = 0.75, colour = col("red")) +
  geom_point(shape = 21, size = 2.4, fill = col("red"), colour = "white") +
  scale_y_discrete(labels = stage_labels_1l) +
  labs(title = "Common endpoint-category sensitivity",
       subtitle = "Development endpoints only",
       x = "Directional lnRR (95% CI)", y = NULL) +
  grid_x + theme(plot.margin = margin(6, 8, 6, 5))

contrast_plot <- rve_contrasts %>%
  filter(Rho == 0.8) %>%
  mutate(
    ContrastLabel = paste(stage_labels_1l[ComparisonStage], "-",
                          stage_labels_1l[ReferenceStage]),
    ContrastLabel = factor(ContrastLabel, levels = rev(unique(ContrastLabel))),
    Estimand = factor(
      Estimand,
      levels = c("Raw directional lnRR", "Conservatively aligned performance lnRR"),
      labels = c("Raw directional", "Aligned performance")
    )
  )
write_csv_safe(contrast_plot, "data/source_data/Fig4c_RVE_stage_contrasts.csv")
contrast_plot_display <- contrast_plot %>% filter(is.finite(CI_low), is.finite(CI_high))
p4c_robustness <- ggplot(contrast_plot_display,
                     aes(Contrast, ContrastLabel, colour = Estimand)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35,
             colour = col("grey")) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y",
                position = position_dodge(width = 0.52), width = 0,
                linewidth = 0.6, na.rm = TRUE) +
  geom_point(position = position_dodge(width = 0.52), size = 2, na.rm = TRUE) +
  scale_colour_manual(values = c(col("red"), col("green")), name = NULL) +
  labs(title = "Stage contrasts remain imprecise",
       subtitle = "Study-cluster RVE; rho = 0.8",
       x = "Stage contrast in lnRR (95% CI)", y = NULL) +
  grid_x + theme(legend.position = "bottom", plot.margin = margin(6, 8, 6, 5))

signed_agg_all <- aggregate_study_stage(rve_base, "lnRR") %>%
  mutate(SE = sqrt(vi), ObservedStage = factor(ObservedStage, levels = stage_levels))
write_csv_safe(signed_agg_all, "data/source_data/Fig4d_signed_funnel_source_data.csv")
funnel_ref <- with(signed_agg_all, weighted.mean(yi, 1 / vi))
se_seq <- seq(0, max(signed_agg_all$SE, na.rm = TRUE), length.out = 150)
funnel_lines <- tibble::tibble(
  SE = rep(se_seq, 2),
  x = c(funnel_ref - qnorm(0.975) * se_seq,
        funnel_ref + qnorm(0.975) * se_seq),
  Side = rep(c("Lower", "Upper"), each = length(se_seq))
)
p4d_robustness <- ggplot(signed_agg_all, aes(yi, SE, colour = ObservedStage)) +
  geom_path(data = funnel_lines, aes(x, SE, group = Side), inherit.aes = FALSE,
            linewidth = 0.4, linetype = "dashed", colour = col("grey_dark")) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = col("grey")) +
  geom_vline(xintercept = funnel_ref, linewidth = 0.45, colour = col("black")) +
  geom_point(size = 1.35, alpha = 0.58) +
  scale_y_reverse() +
  scale_colour_manual(values = stage_pal, labels = stage_labels_1l, name = NULL) +
  labs(title = "Signed precision structure",
       subtitle = "Diagnostic only; asymmetry is not proof of publication bias",
       x = "Study-stage directional lnRR", y = "Standard error") +
  guides(colour = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom", plot.margin = margin(6, 8, 6, 5))

fig4_robustness <- (p4a_robustness | p4b_robustness) / (p4c_robustness | p4d_robustness) +
  patchwork::plot_layout(heights = c(1.03, 1)) +
  patchwork::plot_annotation(tag_levels = "a")
save_figure(
  fig4_robustness,
  "figures/main/Fig4_robustness_and_inference_limits", 190, 154