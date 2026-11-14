# Review round 20 (4 November 2026).
# Fig. 5 must compare the paper's three response definitions in every panel:
#   Directional lnRR (= lnRR), Endpoint-aligned lnRR (= aligned_lnRR_strict),
#   Absolute magnitude (= abs_lnRR, native column; never abs(lnRR)).
# The archived Fig. 5 inputs were produced by engines that were hard-coded to
# one or two definitions (R/12 balanced resampling, R/24 signed-REML rho loop,
# R/127 within-study contrasts, R/133 settlement specifications). This script
# re-runs those identical engines on all three columns so every Fig. 5 panel is
# fed by a real fit. No dataset is altered; the parent scripts are left intact
# because their directional outputs also feed the Supplementary Figures.
source(file.path("R", "00_setup.R"))
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(robumeta); library(clubSandwich); library(metafor)
})
RD <- function(p) readr::read_csv(file.path(root_dir, p), guess_max = 100000,
                                  show_col_types = FALSE, progress = FALSE)

DEF  <- c("Directional" = "lnRR", "Endpoint-aligned" = "aligned_lnRR_strict",
          "Absolute magnitude" = "abs_lnRR")
DLAB <- names(DEF)
clean_stage <- function(x) gsub("[.]", "/", sub("^ObservedStage", "", x))

## ---------------------------------------------------------------- load
d  <- RD("data/processed/analysis_ready_model_dataset_deduplicated.csv")
cc <- RD("data/processed/ER_endpoint_direction_row_coding.csv")
stopifnot(nrow(d) == nrow(cc), identical(d$StudyID, cc$StudyID),
          identical(d$ObservedStage, cc$ObservedStage))
d <- d %>%
  bind_cols(cc %>% dplyr::select(aligned_lnRR_strict, EndpointDirectionClassStrict)) %>%
  mutate(ObservedStage = factor(ObservedStage, levels = stage_levels))

primary <- d %>% filter(DevelopmentalModeGroup == "With metamorphic stage")
rve_base <- primary %>%
  filter(is.finite(lnRR), is.finite(vi_model), vi_model > 0,
         !is.na(StudyID), !is.na(ObservedStage))
cat("primary", nrow(primary), "| rve_base", nrow(rve_base),
    "| studies", dplyr::n_distinct(rve_base$StudyID), "\n")
for (lb in DLAB)
  cat(sprintf("  %-18s finite %4d | studies %3d\n", lb,
              sum(is.finite(rve_base[[DEF[[lb]]]])),
              dplyr::n_distinct(rve_base$StudyID[is.finite(rve_base[[DEF[[lb]]]])])))
stopifnot(!anyNA(rve_base$abs_lnRR))

## =============================================== a. stage RVE, rho grid
rve_fit <- function(x, yi, rho) {
  z <- x %>% filter(is.finite(.data[[yi]])) %>% mutate(.yi = .data[[yi]])
  if (dplyr::n_distinct(z$StudyID) < 3 || nrow(z) < 4) return(NULL)
  tryCatch(robumeta::robu(.yi ~ 0 + ObservedStage, data = z, studynum = StudyID,
                          var.eff.size = vi_model, modelweights = "CORR",
                          rho = rho, small = TRUE), error = function(e) NULL)
}
stage_table <- function(x, rho) {
  if (is.null(x)) return(tibble())
  tab <- x$reg_table
  tibble(Rho = rho,
         ObservedStage = factor(clean_stage(tab$labels), levels = as.character(stage_levels)),
         Estimate = tab$b.r, SE = tab$SE, df = tab$dfs,
         CI_low = tab$CI.L, CI_high = tab$CI.U, P_value = tab$prob)
}
counts_by_stage <- function(x, yi) {
  x %>% filter(is.finite(.data[[yi]])) %>%
    mutate(ObservedStage = factor(as.character(ObservedStage), levels = as.character(stage_levels))) %>%
    group_by(ObservedStage) %>%
    summarise(EffectSizeCount = n(), StudyCount = dplyr::n_distinct(StudyID), .groups = "drop")
}
rve_rho <- bind_rows(lapply(DLAB, function(lb) {
  yi <- DEF[[lb]]
  bind_rows(lapply(c(0, 0.5, 0.8), function(rh)
    stage_table(rve_fit(rve_base, yi, rh), rh) %>% mutate(Definition = lb, .before = 1))) %>%
    left_join(counts_by_stage(rve_base, yi) %>% mutate(Definition = lb, .before = 1),
              by = c("Definition", "ObservedStage"))
})) %>%
  group_by(Definition, Rho) %>%
  mutate(P_value = if_else(StudyCount < 3L, NA_real_, P_value),
         FDR_q_value = p.adjust(P_value, method = "BH")) %>%
  ungroup()
write_csv_safe(rve_rho, "data/source_data/Fig5_three_definitions_RVE_rho.csv")

## =============================================== b. stage contrasts + omnibus
run_contrasts <- function(x, ref = "EMBRYO") {
  if (is.null(x)) return(tibble())
  b <- as.numeric(x$b.r); nm <- clean_stage(x$reg_table$labels)
  i <- match(ref, nm); if (is.na(i)) return(tibble())
  cr2 <- clubSandwich::vcovCR(x, type = "CR2")
  purrr::map_dfr(setdiff(seq_along(b), i), function(j) {
    v <- rep(0, length(b)); v[i] <- -1; v[j] <- 1
    test <- clubSandwich::Wald_test(x, constraints = matrix(v, nrow = 1), vcov = cr2, test = "HTZ")
    est <- b[j] - b[i]; df <- as.numeric(test$df_denom)
    ## Wald_test() has no SE field; for one linear constraint F = (est/SE)^2.
    se <- abs(est) / sqrt(as.numeric(test$Fstat))
    crit <- stats::qt(0.975, df = df)
    tibble(ReferenceStage = ref, ComparisonStage = nm[j], Contrast = est, SE = se, df = df,
           CI_low = est - crit * se, CI_high = est + crit * se,
           P_value = 2 * stats::pt(-abs(est / se), df = df))
  })
}
run_omnibus <- function(x) {
  if (is.null(x)) return(tibble())
  k <- length(x$b.r); if (k < 2) return(tibble())
  test <- clubSandwich::Wald_test(x, constraints = cbind(-1, diag(k - 1L)), vcov = "CR2", test = "HTZ")
  tibble(Fstat = as.numeric(test$Fstat), df_num = as.numeric(test$df_num),
         df_denom = as.numeric(test$df_denom), P_value = as.numeric(test$p_val))
}
all_study <- bind_rows(lapply(DLAB, function(lb)
  run_contrasts(rve_fit(rve_base, DEF[[lb]], 0.8)) %>% mutate(Definition = lb, .before = 1)))
omnibus3  <- bind_rows(lapply(DLAB, function(lb)
  run_omnibus(rve_fit(rve_base, DEF[[lb]], 0.8)) %>% mutate(Definition = lb, .before = 1)))
write_csv_safe(all_study, "data/source_data/Fig5_three_definitions_RVE_contrasts.csv")
write_csv_safe(omnibus3,  "data/source_data/Fig5_three_definitions_omnibus.csv")

## within-study contrasts, same design as R/127, looped over three definitions
analysis <- RD("data/processed/analysis_ready_with_taxonomy_ecological_guild.csv")
stopifnot(nrow(analysis) == nrow(cc))
blocks <- analysis %>%
  mutate(.row = row_number()) %>%
  dplyr::select(.row, StudyID, SpeciesID, ObservedStage, StressorLabel,
                DevelopmentalModeGroup, abs_lnRR) %>%
  left_join(cc %>% mutate(.row = row_number()) %>%
              dplyr::select(.row, lnRR, aligned_lnRR_strict, vi_model), by = ".row") %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage", is.finite(lnRR),
         is.finite(vi_model), vi_model > 0, !is.na(StudyID),
         StressorLabel %in% stressor_levels) %>%
  mutate(Block = paste(StudyID, SpeciesID, StressorLabel, sep = "|"))
cell_stage <- function(x, yi) {
  x %>% filter(is.finite(.data[[yi]])) %>% group_by(StudyID, Block, ObservedStage) %>%
    summarise(yi = sum(.data[[yi]] / vi_model) / sum(1 / vi_model),
              vi = (.2 * sum(1 / vi_model) + .8 * sum(1 / sqrt(vi_model))^2) / sum(1 / vi_model)^2,
              EffectSizeCount = n(), .groups = "drop")
}
fit_contrasts <- function(cc2, design, scope) {
  cc2 <- cc2 %>% mutate(ObservedStage = factor(ObservedStage,
                    levels = stage_levels[stage_levels %in% ObservedStage]))
  if (nlevels(cc2$ObservedStage) < 2) return(tibble())
  form <- if (design == "Within-study (block fixed effects)") ~ ObservedStage + factor(Block) else ~ ObservedStage
  f <- tryCatch(metafor::rma.uni(yi = cc2$yi, vi = cc2$vi, mods = form, data = cc2, method = "REML",
                                 control = list(stepadj = .5, maxiter = 1000)), error = function(e) NULL)
  if (is.null(f)) return(tibble())
  ct <- tryCatch(clubSandwich::coef_test(f, vcov = "CR2", cluster = cc2$StudyID), error = function(e) NULL)
  if (is.null(ct)) return(tibble())
  ref <- levels(cc2$ObservedStage)[1]; idx <- grep("^ObservedStage", rownames(f$beta))
  purrr::map_dfr(seq_along(idx), function(m) {
    i <- idx[m]; nm <- sub("^ObservedStage", "", rownames(f$beta)[i])
    paired <- cc2 %>% filter(ObservedStage %in% c(ref, nm)) %>%
      group_by(Block) %>% filter(dplyr::n_distinct(ObservedStage) == 2L) %>% ungroup()
    tibble(Design = design, Scope = scope, ReferenceStage = ref, ComparisonStage = nm,
           Contrast = ct$beta[i], SE = ct$SE[i], df = ct$df_Satt[i], P_value = ct$p_Satt[i],
           CellCount = nrow(cc2), PairedBlockCount = dplyr::n_distinct(paired$Block),
           PairedStudyCount = dplyr::n_distinct(paired$StudyID),
           Inference = ifelse(is.finite(ct$df_Satt[i]) && ct$df_Satt[i] >= 4,
                              "CR2 Satterthwaite", "Descriptive: CR2 df < 4"))
  })
}
multi <- blocks %>% group_by(Block) %>% filter(dplyr::n_distinct(ObservedStage) >= 2L) %>% ungroup()
ws <- bind_rows(lapply(DLAB, function(lb) {
  yi <- DEF[[lb]]
  bind_rows(
    fit_contrasts(cell_stage(multi, yi), "Within-study (block fixed effects)", "Multi-stage blocks"),
    fit_contrasts(cell_stage(multi, yi) %>% mutate(Block = "all"),
                  "Between-stage, no block adjustment", "Multi-stage blocks"),
    fit_contrasts(cell_stage(blocks, yi) %>% mutate(Block = "all"),
                  "Between-stage, no block adjustment", "All evidence")) %>%
    mutate(Definition = lb, .before = 1)
}))
write_csv_safe(ws, "data/source_data/Fig5_three_definitions_within_study_contrasts.csv")

## =============================================== c. leave-one-study-out
per <- RD("data/source_data/SupplementaryTableS7b_leave_one_out_per_omission.csv")
cat("panel c Estimand counts:",
    paste(names(table(per$Estimand)), table(per$Estimand), collapse = " / "), "\n")
write_csv_safe(per %>% mutate(Definition = dplyr::recode(Estimand,
  `Directional lnRR` = "Directional", `Aligned performance lnRR` = "Endpoint-aligned",
  `Absolute |lnRR|` = "Absolute magnitude")),
  "data/source_data/Fig5_three_definitions_leave_one_out.csv")

## =============================================== d. settlement specifications
held <- RD("data/source_data/Settlement_variance_unverifiable_StudyID23.csv") %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage")
stopifnot(nrow(held) == 6L, all(held$StudyID == 23))
cat("held-out row mean abs_lnRR:", sprintf("%.6f", mean(held$abs_lnRR)), "\n")
held <- held %>% transmute(LatestModelRowID, StudyID, ObservedStage, StressorLabel,
                           lnRR, aligned_lnRR_strict = NA_real_, abs_lnRR, vi_model)
stopifnot(!any(held$LatestModelRowID %in% primary$LatestModelRowID))
# Same population as R/133: metamorphic taxa only (the full database would let
# non-metamorphic rows enter the pooled RVE variance components).
restored <- bind_rows(primary %>% dplyr::select(LatestModelRowID, StudyID, ObservedStage,
                                          StressorLabel, lnRR, aligned_lnRR_strict, abs_lnRR, vi_model), held)
fit_settlement <- function(x, yi) {
  z <- x %>% filter(is.finite(.data[[yi]])) %>%
    mutate(.yi = .data[[yi]], ObservedStage = factor(ObservedStage, levels = stage_levels))
  stages <- z %>% distinct(StudyID, ObservedStage) %>% count(ObservedStage) %>%
    filter(n >= 3) %>% pull(ObservedStage)
  z <- z %>% filter(ObservedStage %in% stages) %>% mutate(ObservedStage = droplevels(ObservedStage))
  f <- tryCatch(robumeta::robu(.yi ~ 0 + ObservedStage, data = z, studynum = StudyID,
                               var.eff.size = vi_model, modelweights = "CORR", rho = 0.8, small = TRUE),
                error = function(e) NULL)
  if (is.null(f)) return(tibble())
  tab <- f$reg_table
  tibble(Stage = clean_stage(tab$labels), Estimate = tab$b.r, SE = tab$SE, df = tab$dfs,
         CI_low = tab$CI.L, CI_high = tab$CI.U, P_value = tab$prob)
}
specs <- list("Main data (k = 35, m = 9)" = primary,
              "StudyID 23 restored (k = 41, m = 10)" = restored,
              "StudyID 23 restored, row 1083 removed" = restored %>% filter(LatestModelRowID != 1083))
settle <- bind_rows(lapply(names(specs), function(sp)
  bind_rows(lapply(DLAB, function(lb)
    fit_settlement(specs[[sp]], DEF[[lb]]) %>% mutate(Definition = lb, .before = 1))) %>%
    mutate(Specification = sp, .before = 1)))
write_csv_safe(settle, "data/source_data/Fig5_three_definitions_settlement.csv")

## =============================================== e. signed REML, rho loop
foldmean <- function(mu, tau) if (tau < 1e-12) abs(mu) else
  tau * sqrt(2 / pi) * exp(-mu^2 / (2 * tau^2)) + mu * (2 * pnorm(mu / tau) - 1)
reml_rows <- list(); z <- 0L
for (lb in DLAB) for (yi in DEF[[lb]]) for (rho in c(0, 0.5, 0.8)) {
  sub <- primary %>% filter(is.finite(.data[[yi]]))
  cells <- sub %>% group_by(StudyID, ObservedStage) %>%
    summarise(yi = sum(.data[[yi]] / vi_model) / sum(1 / vi_model),
              vi = ((1 - rho) * sum(1 / vi_model) + rho * sum(1 / sqrt(vi_model))^2) / sum(1 / vi_model)^2,
              k = n(), .groups = "drop")
  for (st in stage_levels) {
    x <- filter(cells, ObservedStage == st); if (nrow(x) < 3) next
    f <- tryCatch(metafor::rma.uni(yi = x$yi, vi = x$vi, method = "REML", test = "knha"),
                  error = function(e) NULL); if (is.null(f)) next
    mu <- as.numeric(f$b); tau <- sqrt(f$tau2); z <- z + 1L
    reml_rows[[z]] <- tibble(Definition = lb, EvidenceSet = "All eligible endpoints", Rho = rho,
      ObservedStage = st, k = sum(x$k), m = nrow(x), Estimate = mu, CI_low = f$ci.lb,
      CI_high = f$ci.ub, P_value = f$pval, Tau2 = f$tau2,
      ImpliedLatentAbsoluteMean = foldmean(mu, tau),
      CenteredHeterogeneityAbsoluteMean = foldmean(0, tau),
      MeanShiftIncrement = foldmean(mu, tau) - foldmean(0, tau))
  }
}
reml3 <- bind_rows(reml_rows) %>% group_by(Definition, Rho) %>%
  mutate(FDR_q_value = p.adjust(P_value, "BH")) %>% ungroup()
write_csv_safe(reml3, "data/source_data/Fig5_three_definitions_signed_REML.csv")

## =============================================== f. balanced equal-weight
study_stage_all <- bind_rows(lapply(DLAB, function(lb) {
  yi <- DEF[[lb]]
  primary %>% filter(is.finite(.data[[yi]]), is.finite(vi_model), vi_model > 0) %>%
    group_by(ObservedStage, StudyID) %>%
    summarise(yi = sum(.data[[yi]] / vi_model) / sum(1 / vi_model), .groups = "drop") %>%
    mutate(Definition = lb, .before = 1)
}))
set.seed(20260827L)
balanced3 <- bind_rows(lapply(DLAB, function(lb) {
  ssa <- filter(study_stage_all, Definition == lb)
  n_st <- ssa %>% count(ObservedStage, name = "StudyCount")
  min_n <- min(n_st$StudyCount, na.rm = TRUE)
  draws <- bind_rows(lapply(stage_levels, function(st) {
    x <- filter(ssa, ObservedStage == st); if (!nrow(x)) return(NULL)
    tibble(ObservedStage = st,
           draw = replicate(2000L, mean(sample(x$yi, min_n, replace = FALSE), na.rm = TRUE)))
  }))
  ssa %>% group_by(ObservedStage) %>%
    summarise(EqualWeightEstimate = mean(yi, na.rm = TRUE), EqualWeightSD = sd(yi, na.rm = TRUE),
              StudyCount = n(), .groups = "drop") %>%
    right_join(draws %>% group_by(ObservedStage) %>%
      summarise(BalancedCI_low = quantile(draw, 0.025, na.rm = TRUE),
                BalancedCI_high = quantile(draw, 0.975, na.rm = TRUE),
                BalancedMedian = median(draw, na.rm = TRUE),
                BalancedNegativeProbability = mean(draw < 0, na.rm = TRUE), .groups = "drop"),
      by = "ObservedStage") %>%
    mutate(Definition = lb, CommonBalancedStudyCount = min_n, .before = 1)
}))
write_csv_safe(balanced3, "data/source_data/Fig5_three_definitions_balanced_stage.csv")

cat("\n--- three-definition check (rho = 0.8) ---\n")
print(as.data.frame(rve_rho %>% filter(Rho == 0.8) %>%
  dplyr::select(Definition, ObservedStage, Estimate, CI_low, CI_high, P_value,
                StudyCount, EffectSizeCount)), digits = 4)
print(as.data.frame(omnibus3), digits = 4)
