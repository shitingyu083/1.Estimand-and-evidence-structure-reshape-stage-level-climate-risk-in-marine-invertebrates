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

aggregate_for_model <- function(d, yi_col, group_cols) {
  d %>%
    filter(is.finite(.data[[yi_col]]), is.finite(vi_model), vi_model > 0) %>%
    group_by(across(all_of(c(group_cols, "StudyID")))) %>%
    summarise(
      yi = sum(.data[[yi_col]] / vi_model, na.rm = TRUE) / sum(1 / vi_model, na.rm = TRUE),
      vi = 1 / sum(1 / vi_model, na.rm = TRUE),
      RawEffectSizeCount = n(),
      SpeciesCountInStudyCell = n_distinct(SpeciesID),
      .groups = "drop"
    ) %>%
    filter(is.finite(yi), is.finite(vi), vi > 0)
}

fit_agg <- function(d, min_studies = 3) {
  d <- d %>% filter(is.finite(yi), is.finite(vi), vi > 0)
  if (nrow(d) < min_studies) return(NULL)
  tryCatch(
    metafor::rma.uni(
      yi = d$yi,
      vi = d$vi,
      method = "REML",
      control = list(maxiter = 80, stepadj = 0.5)
    ),
    error = function(e) NULL
  )
}

extract_fit <- function(fit, d, estimand, mode, stage, stressor, group_type) {
  if (is.null(fit) || nrow(d) == 0) return(NULL)
  pred <- tryCatch(metafor::predict.rma(fit), error = function(e) NULL)
  tibble(
    Estimand = estimand,
    DevelopmentalModeGroup = as.character(mode),
    ObservedStage = as.character(stage),
    StressorLabel = as.character(stressor),
    GroupType = group_type,
    Estimate = as.numeric(fit$b),
    SE = fit$se,
    CI_low = fit$ci.lb,
    CI_high = fit$ci.ub,
    PI_low = if (!is.null(pred) && "cr.lb" %in% names(pred)) as.numeric(pred$cr.lb) else NA_real_,
    PI_high = if (!is.null(pred) && "cr.ub" %in% names(pred)) as.numeric(pred$cr.ub) else NA_real_,
    P_value = fit$pval,
    EffectSizeCount = sum(d$RawEffectSizeCount, na.rm = TRUE),
    StudyCount = n_distinct(d$StudyID),
    SpeciesCount = sum(d$SpeciesCountInStudyCell, na.rm = TRUE),
    I2_percent = suppressWarnings(as.numeric(fit$I2)),
    Tau2 = suppressWarnings(as.numeric(fit$tau2)),
    Significant = fit$ci.lb > 0 | fit$ci.ub < 0,
    EvidenceClass = case_when(
      sum(d$RawEffectSizeCount, na.rm = TRUE) >= 20 & n_distinct(d$StudyID) >= 5 ~ "primary",
      sum(d$RawEffectSizeCount, na.rm = TRUE) >= 10 & n_distinct(d$StudyID) >= 3 ~ "moderate",
      TRUE ~ "sparse"
    )
  )
}

fit_mode_stage <- function(d, yi_col, estimand) {
  by_driver_agg <- aggregate_for_model(
    d, yi_col,
    c("DevelopmentalModeGroup", "ObservedStage", "StressorLabel")
  )
  by_all_agg <- aggregate_for_model(
    d %>% mutate(StressorLabel = factor("All stressors", levels = c(stressor_levels, "All stressors"))),
    yi_col,
    c("DevelopmentalModeGroup", "ObservedStage", "StressorLabel")
  )

  by_driver <- bind_rows(lapply(
    split(by_driver_agg, interaction(by_driver_agg$DevelopmentalModeGroup,
                                     by_driver_agg$ObservedStage,
                                     by_driver_agg$StressorLabel,
                                     drop = TRUE)),
    function(x) {
      extract_fit(fit_agg(x), x, estimand, x$DevelopmentalModeGroup[1],
                  x$ObservedStage[1], x$StressorLabel[1], "mode_stage_driver")
    }
  ))

  by_all <- bind_rows(lapply(
    split(by_all_agg, interaction(by_all_agg$DevelopmentalModeGroup,
                                  by_all_agg$ObservedStage,
                                  drop = TRUE)),
    function(x) {
      extract_fit(fit_agg(x), x, estimand, x$DevelopmentalModeGroup[1],
                  x$ObservedStage[1], "All stressors", "mode_stage_all")
    }
  ))

  bind_rows(by_driver, by_all)
}

main_results <- bind_rows(
  fit_mode_stage(dat, "lnRR", "Directional lnRR"),
  fit_mode_stage(dat, "abs_lnRR", "Absolute |lnRR|")
) %>%
  group_by(Estimand, DevelopmentalModeGroup, GroupType) %>%
  mutate(
    FDR_q_value = p.adjust(P_value, method = "BH"),
    Significant_FDR_0_05 = FDR_q_value < 0.05
  ) %>%
  ungroup() %>%
  mutate(
    ObservedStage = factor(ObservedStage, levels = stage_levels),
    StressorLabel = factor(StressorLabel, levels = c(stressor_levels, "All stressors")),
    DevelopmentalModeGroup = factor(DevelopmentalModeGroup, levels = mode_levels),
    PercentChangeApprox = 100 * (exp(Estimate) - 1)
  ) %>%
  arrange(Estimand, DevelopmentalModeGroup, ObservedStage, StressorLabel)

decision_matrix <- main_results %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage", GroupType == "mode_stage_driver") %>%
  select(Estimand, ObservedStage, StressorLabel, Estimate, CI_low, CI_high,
         PI_low, PI_high, P_value, FDR_q_value, Significant, Significant_FDR_0_05,
         EffectSizeCount, StudyCount, EvidenceClass) %>%
  mutate(
    ObservedStage = as.character(ObservedStage),
    StressorLabel = as.character(StressorLabel),
    EstimandKey = recode(Estimand,
                         "Directional lnRR" = "directional",
                         "Absolute |lnRR|" = "absolute")
  ) %>%
  select(-Estimand) %>%
  tidyr::pivot_wider(
    names_from = EstimandKey,
    values_from = c(
      Estimate, CI_low, CI_high, PI_low, PI_high, P_value, FDR_q_value,
      Significant, Significant_FDR_0_05, EffectSizeCount, StudyCount, EvidenceClass
    ),
    names_sep = "__"
  ) %>%
  mutate(
    Inference = case_when(
      Significant_FDR_0_05__directional %in% TRUE & Significant_FDR_0_05__absolute %in% TRUE ~ "Directional and absolute",
      !(Significant_FDR_0_05__directional %in% TRUE) & Significant_FDR_0_05__absolute %in% TRUE ~ "Absolute only",
      Significant_FDR_0_05__directional %in% TRUE & !(Significant_FDR_0_05__absolute %in% TRUE) ~ "Directional only",
      TRUE ~ "Neither"
    )
  )

fit_group_estimate <- function(d, yi_col) {
  agg <- aggregate_for_model(d, yi_col, c("ObservedStage"))
  bind_rows(lapply(split(agg, agg$ObservedStage, drop = TRUE), function(x) {
    fit <- fit_agg(x)
    if (is.null(fit)) return(NULL)
    tibble(
      ObservedStage = as.character(x$ObservedStage[1]),
      Estimate = as.numeric(fit$b),
      CI_low = fit$ci.lb,
      CI_high = fit$ci.ub,
      K = sum(x$RawEffectSizeCount, na.rm = TRUE)
    )
  }))
}

update_sensitivity <- bind_rows(lapply(c("lnRR", "abs_lnRR"), function(yi_col) {
  estimand <- ifelse(yi_col == "lnRR", "Directional lnRR", "Absolute |lnRR|")
  d0 <- dat %>% filter(DevelopmentalModeGroup == "With metamorphic stage")
  full <- fit_group_estimate(d0, yi_col) %>%
    rename(UpdatedEstimate = Estimate, UpdatedCI_low = CI_low, UpdatedCI_high = CI_high, UpdatedK = K)
  original <- fit_group_estimate(d0 %>% filter(UpdateLayer == "Original curated evidence"), yi_col) %>%
    rename(OriginalEstimate = Estimate, OriginalCI_low = CI_low, OriginalCI_high = CI_high, OriginalK = K)
  full_join(original, full, by = "ObservedStage") %>%
    mutate(
      Estimand = estimand,
      DeltaEstimate = UpdatedEstimate - OriginalEstimate
    ) %>%
    select(Estimand, ObservedStage, OriginalEstimate, OriginalCI_low, OriginalCI_high,
           UpdatedEstimate, UpdatedCI_low, UpdatedCI_high, DeltaEstimate, OriginalK, UpdatedK)
}))

leave_one_out <- bind_rows(lapply(c("abs_lnRR"), function(yi_col) {
  estimand <- ifelse(yi_col == "lnRR", "Directional lnRR", "Absolute |lnRR|")
  d0 <- dat %>% filter(DevelopmentalModeGroup == "With metamorphic stage")
  agg <- aggregate_for_model(d0, yi_col, c("ObservedStage"))
  bind_rows(lapply(split(agg, agg$ObservedStage, drop = TRUE), function(x) {
    full <- fit_agg(x)
    if (is.null(full) || n_distinct(x$StudyID) < 4) return(NULL)
    per <- bind_rows(lapply(unique(x$StudyID), function(sid) {
      sub <- x %>% filter(StudyID != sid)
      fit <- fit_agg(sub)
      if (is.null(fit)) return(NULL)
      tibble(OmittedStudyID = sid, Estimate = as.numeric(fit$b))
    }))
    if (nrow(per) == 0) return(NULL)
    tibble(
      Estimand = estimand,
      ObservedStage = as.character(x$ObservedStage[1]),
      FullEstimate = as.numeric(full$b),
      MinEstimate = min(per$Estimate),
      MaxEstimate = max(per$Estimate),
      OmittedStudies = nrow(per)
    )
  }))
}))

egger_checks <- bind_rows(lapply(c("lnRR", "abs_lnRR"), function(yi_col) {
  estimand <- ifelse(yi_col == "lnRR", "Directional lnRR", "Absolute |lnRR|")
  d0 <- dat %>% filter(DevelopmentalModeGroup == "With metamorphic stage")
  agg <- aggregate_for_model(d0, yi_col, c("ObservedStage"))
  bind_rows(lapply(split(agg, agg$ObservedStage, drop = TRUE), function(x) {
    if (nrow(x) < 10 || n_distinct(x$StudyID) < 10) return(NULL)
    fit <- fit_agg(x, min_studies = 10)
    if (is.null(fit)) return(NULL)
    egger <- tryCatch(metafor::regtest(fit, model = "rma", predictor = "sei"), error = function(e) NULL)
    tibble(
      Estimand = estimand,
      ObservedStage = as.character(x$ObservedStage[1]),
      EffectSizeCount = sum(x$RawEffectSizeCount, na.rm = TRUE),
      StudyCount = n_distinct(x$StudyID),
      Egger_z = if (!is.null(egger)) as.numeric(egger$zval) else NA_real_,
      Egger_p = if (!is.null(egger)) as.numeric(egger$pval) else NA_real_,
      Flag = if_else(!is.na(Egger_p) & Egger_p < 0.05, "Egger p < 0.05", "No Egger signal")
    )
  }))
}))

variance_floor <- bind_rows(lapply(c(1e-6, 1e-4, 1e-3), function(floor_value) {
  d0 <- dat %>%
    filter(DevelopmentalModeGroup == "With metamorphic stage") %>%
    mutate(vi_model = pmax(vi_raw, floor_value, na.rm = TRUE))
  bind_rows(lapply(c("lnRR", "abs_lnRR"), function(yi_col) {
    estimand <- ifelse(yi_col == "lnRR", "Directional lnRR", "Absolute |lnRR|")
    agg <- aggregate_for_model(
      d0 %>% mutate(StressorLabel = factor("All stressors", levels = c(stressor_levels, "All stressors"))),
      yi_col,
      c("DevelopmentalModeGroup", "ObservedStage", "StressorLabel")
    )
    bind_rows(lapply(split(agg, agg$ObservedStage, drop = TRUE), function(x) {
      fit <- fit_agg(x)
      extract_fit(fit, x, estimand, "With metamorphic stage", x$ObservedStage[1],
                  "All stressors", "variance_floor") %>%
        mutate(VarianceFloor = floor_value)
    }))
  }))
}))

write_csv_safe(main_results, "data/processed/main_results_REML.csv")
write_csv_safe(decision_matrix, "data/processed/directional_vs_absolute_decision_matrix.csv")
write_csv_safe(update_sensitivity, "data/processed/update_sensitivity.csv")
write_csv_safe(leave_one_out, "data/processed/leave_one_study_out.csv")
write_csv_safe(egger_checks, "data/processed/egger_small_study_checks.csv")
write_csv_safe(variance_floor, "data/processed/variance_floor_sensitivity.csv")

message("Fitted study-level aggregated REML models and sensitivity checks.")