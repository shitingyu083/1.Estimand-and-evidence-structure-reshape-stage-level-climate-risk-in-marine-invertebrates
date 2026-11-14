source("R/00_setup.R")
# Sensitivity of the signed stage coefficients to the clustering unit.
suppressPackageStartupMessages(library(robumeta))
effect_dat <- read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "analysis_ready_model_dataset_deduplicated.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage",
         is.finite(lnRR), is.finite(VarianceUsedInModel), VarianceUsedInModel > 0,
         !is.na(StudyID), !is.na(SpeciesID), !is.na(Family), !is.na(ObservedStage)) %>%
  mutate(ObservedStage = factor(ObservedStage))

# A common complete-case dataset is required to compare clustering units.
# Three otherwise eligible StudyID clusters (229, 234 and 235) lack Family,
# leaving 146 StudyID clusters for this sensitivity versus 149 in the primary
# StudyID-cluster analysis.

fit_cluster_sensitivity <- function(cluster_col, cluster_label) {
  dd <- effect_dat %>% mutate(ClusterID = .data[[cluster_col]])
  fit <- robumeta::robu(
    lnRR ~ 0 + ObservedStage, data = dd, studynum = ClusterID,
    var.eff.size = VarianceUsedInModel, modelweights = "CORR", rho = 0.8, small = TRUE
  )
  fit$reg_table %>%
    as_tibble() %>%
    transmute(
      ClusterDefinition = cluster_label,
      Stage = sub("^ObservedStage", "", labels),
      Estimate = b.r, SE = SE, df = dfs, T = t,
      P_value = prob, CI_low = CI.L, CI_high = CI.U,
      ClusterCount = n_distinct(dd$ClusterID)
    )
}
cluster_sensitivity <- bind_rows(
  fit_cluster_sensitivity("StudyID", "StudyID"),
  fit_cluster_sensitivity("SpeciesID", "Species"),
  fit_cluster_sensitivity("Family", "Family")
)
write_csv(
  cluster_sensitivity,
  file.path(root_dir, "data", "source_data", "Supplementary_RVE_cluster_sensitivity.csv")
)

# Compare the all-taxa and metamorphic-taxa stage estimates on the same study-stage scale.
all_effects <- read_csv(guess_max=100000, 
  file.path(root_dir, "data", "processed", "analysis_ready_model_dataset_deduplicated.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  filter(is.finite(lnRR), is.finite(VarianceUsedInModel), VarianceUsedInModel > 0,
         !is.na(StudyID), !is.na(ObservedStage))

fit_stage_subset <- function(dd, subset_label) {
  agg <- dd %>%
    group_by(StudyID, ObservedStage) %>%
    summarise(
      yi = weighted.mean(lnRR, 1 / VarianceUsedInModel),
      vi = 1 / sum(1 / VarianceUsedInModel), .groups = "drop"
    )
  bind_rows(lapply(split(agg, agg$ObservedStage), function(x) {
    fit <- metafor::rma.uni(yi = x$yi, vi = x$vi, method = "REML")
    tibble(
      EvidenceSubset = subset_label, Stage = as.character(x$ObservedStage[1]),
      Estimate = as.numeric(fit$b), SE = fit$se, CI_low = fit$ci.lb, CI_high = fit$ci.ub,
      P_value = fit$pval, StudyCount = n_distinct(x$StudyID)
    )
  }))
}
taxa_subset_sensitivity <- bind_rows(
  fit_stage_subset(all_effects, "All taxa"),
  fit_stage_subset(
    filter(all_effects, DevelopmentalModeGroup == "With metamorphic stage"),
    "Taxa with a metamorphic stage"
  )
)
write_csv(
  taxa_subset_sensitivity,
  file.path(root_dir, "data", "source_data", "Supplementary_taxa_subset_stage_sensitivity.csv")
)


