message("Running R meta-analysis and sensitivity workflow from the archived analysis-ready dataset")
source(file.path("R", "25_resolve_cross_layer_duplicates.R"))
python <- Sys.getenv("ER_PYTHON", unset="python")
for(script in c("R/42_restore_boundary_metadata.py","R/136_exclude_unverifiable_variance.py","R/43_endpoint_source_rules.py","R/44_jury_hierarchical_reextraction.py","R/49_williams_repair.py","R/51_merge_exposure_data.py","R/128_settlement_source_corrections.py")) {
  if(system2(python,shQuote(script))!=0) stop("Boundary repair failed: ",script)
}
source(file.path("R", "27_refresh_evidence_counts.R"))
source(file.path("R", "02_meta_models.R"))
theme_pub_clean <- function(base_size = 7, base_family = "Arial") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.key = ggplot2::element_blank())
}
source(file.path("R", "04_estimand_sensitivity.R"))
source(file.path("R", "28_refresh_derived_summaries.R"))
source(file.path("R", "03_figures.R"))
python <- Sys.getenv("ER_PYTHON", unset="python")
status <- system2(python, shQuote("R/26_reference_calculation.py"))
if (status != 0) stop("Reference-calculation Python step failed")
# Reference-deviation calibration is generated without predictive modelling.
source(file.path("R", "09_reference_deviation.R"))
# Legacy visual-restyle script removed; submitted figures are rendered by the
# dedicated figure scripts below.
source(file.path("R", "11_final_fig1a_polish.R"))
source(file.path("R", "12_supplementary_sensitivity.R"))
source(file.path("R", "18_taxonomy_class_meta.R"))
source(file.path("R", "19_ecological_guild_meta.R"))
source(file.path("R", "21a_ecological_moderator_tests.R"))
source(file.path("R", "21_estimand_subgroup_figure3.R"))
source(file.path("R", "24_review_revision_analysis.R"))
source(file.path("R", "34_dependence_subset_sensitivity.R"))
source(file.path("R", "46_boundary_sensitivity.R"))
source(file.path(root_dir, "fig1_final_vector.R"))
source(file.path("R", "22_rebuild_submission_main_figures.R"))
# Re-render the three-estimand subgroup forest and paired-difference panels
# last, so the canonical outputs cannot be overwritten by the legacy figure
# assembly above.
source(file.path("R", "23_subgroup_three_estimands.R"))
save_figure(make_opentree_three_part(), "figures/extended/SupplementaryFigS2_complete_species_topology", 240, 150, dpi=600)
for (script in c("R/15_graphical_abstract.py","R/29_recover_exposure_metadata.py","R/31_sync_manuscript_text.py","R/50_sync_boundary_manuscript.py","R/32_revision_quality_checks.py","R/57_sync_review_and_style.py","R/61_human_endorsed_coding_review.py","R/64_concise_review_reporting.py","R/67_finalize_source_verification.py")) {
  if (system2(python,shQuote(script)) != 0) stop("Python step failed: ",script)
}
# Script 29 rewrites Review_exposure_recovery.csv; re-apply the idempotent
# settlement source corrections so the corrected exposure values persist.
if (system2(python, shQuote("R/128_settlement_source_corrections.py")) != 0) stop("Settlement source corrections failed")
if (system2(python, shQuote("R/136_exclude_unverifiable_variance.py")) != 0) stop("Variance hold-out failed")
# Keep only the canonical numbered main-figure exports in figures/main.  Older
# scripts intentionally write superseded names for auditability; move those
# products out of the submission-facing directory after the final render.
legacy_names <- c(
  "Fig2_directional_and_absolute_stage_responses.pdf",
  "Fig2_directional_and_absolute_stage_responses.png",
  "Fig2_directional_and_absolute_stage_responses.svg",
  "Fig2_directional_and_absolute_stage_responses.tiff",
  "Fig3_driver_estimand_and_subgroup_comparison.pdf",
  "Fig3_driver_estimand_and_subgroup_comparison.png",
  "Fig3_driver_estimand_and_subgroup_comparison.svg",
  "Fig3_driver_estimand_and_subgroup_comparison.tiff",
  "Fig4_ecological_subgroup_responses.pdf",
  "Fig4_ecological_subgroup_responses.png",
  "Fig4_ecological_subgroup_responses.svg",
  "Fig4_ecological_subgroup_responses.tiff"
)
legacy_dir <- file.path(root_dir, "figures", "legacy_exports_20260910")
dir.create(legacy_dir, recursive = TRUE, showWarnings = FALSE)
for (nm in legacy_names) {
  src <- file.path(root_dir, "figures", "main", nm)
  if (file.exists(src)) file.rename(src, file.path(legacy_dir, nm))
}
source('R/69_visual_unification.R')
source('R/72_final_visual_details.R')
if (system2(python, shQuote('R/70_sync_visual_revision.py')) != 0) stop('Final visual manuscript sync failed')
source('R/74_pairwise_forest_revision.R')
if (system2(python, shQuote('R/75_sync_pairwise_revision.py')) != 0) stop('Paired forest manuscript sync failed')
if (system2(python, shQuote('R/76_audit_zero_and_missing.py')) != 0) stop('Zero and missing evidence audit failed')
source('R/79_final_nature_visuals.R')
source('R/82_check_star_positions.R')
if (system2(python, shQuote('R/81_sync_final_composites.py')) != 0) stop('Final caption sync failed')
if (system2(python, shQuote('R/84_sync_distribution_figures.py')) != 0) stop('Distribution caption sync failed')
if (system2(python, shQuote('R/78_make_all_estimates_workbook.py')) != 0) stop('Workbook assembly failed')
# Last rendering pass prevents legacy split trees and prediction arrows from returning.
source('R/97_annotated_evidence_forests.R')
message("R workflow complete: REML results, robust variance estimates, sensitivity results, and final polished submission figures regenerated.")


