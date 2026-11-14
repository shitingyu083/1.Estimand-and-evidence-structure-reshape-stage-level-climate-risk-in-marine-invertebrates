# Supplementary numbering of 17 October 2026 (supersedes the S numbers below)

Supplementary figures and tables were renumbered so that each is numbered in order of first citation in the
manuscript, and the supplementary DOCX now presents Figures S1-S13 followed by Tables S1-S21 in that order.
Figure files in figures/extended and supplementary_figures were renamed to the new numbers; the producing
scripts, run_all.R and build_revision_documents.ps1 write the new names. S numbers in the dated sections below
(and in analysis_logs and source-data CSV names such as SupplementaryFigS4bcd_*, TableS19_*) are the earlier
numbers; data/source_data/Supplementary_numbering_map_20261017.csv lists the full mapping. Supplementary
Figure S2 (complete species topology) was redrawn as one tree in three consecutive vertical ranges
(R/20 make_opentree_three_part, 240 x 150 mm); no estimate, interval, q value or BH rule changed.

    Figures (old -> new): S1->S1, S12->S2, S10->S3, S5->S4, S11->S5, S13->S6, S9->S7, S7->S8, S8->S9, S6->S10, S2->S11, S4->S12, S3->S13
    Tables  (old -> new): S1->S1, S2->S4, S3->S3, S4->S5, S5->S20, S6->S17, S7->S21, S8->S19, S9->S16, S10->S2, S11->S8, S12->S12, S13->S18, S14->S13, S15->S14, S16->S15, S17->S6, S18->S10, S19->S9, S20->S11, S21->S7

# Main-figure layout of 17 October 2026 (supersedes the figure numbering below)

The submitted manuscript has five main figures. figures/main holds the author's manually assembled masters
(PDF, SVG, 600-dpi PNG and TIFF; 20 files). They were assembled from panels written by the scripts below,
which keep their earlier output names; re-running them writes those panel families, not the assembled masters.

    Figure 1  Fig1_classification_and_evidence_update  R/113 (author's label-line adjustments)
    Figure 2  Fig2_ecological_subgroup_responses       R/97
    Figure 3  Fig3_definition_dependent_orderings      a: R/97 f3_row2 via R/122 (stage ranks); b-c: R/121
    Figure 4  Fig4_response_definition_synthesis       R/123 (former Figure 5 a-c; former 5d summary only in S4e)
    Figure 5  Fig5_sensitivity_and_robustness          a-f, three response definitions: R/138 then R/134 (primary RVE, stage contrasts, LOSO, settlement sources, rho, balanced)

The former matched paired-contrast panel (former Figure 4b) is no longer a main figure; its values are in
Supplementary Table S10 and the Excel attachment. The former Figure 3a stage forest and the three-definition summary
are in Supplementary Figure S4e. Superseded main-figure files are in
figures/_superseded_intermediate/main_pre_20261017 (excluded from the ZIP). Source-data file names keep their
historical figure prefixes. No model, estimate, interval, q value or BH rule changed.

# Final submission figures: 8 October 2026

Main Figures 3-6 of the submitted manuscript are produced by four additional entry points, run one per R
process from the archive root (each sources the earlier scripts it needs, which may also refresh superseded
figure families in figures/main):

    Rscript --vanilla R/121_fig3_per_stage.R      -> figures/main/Fig3_per_stage_directional_response
    Rscript --vanilla R/122_fig4_merge.R          -> figures/main/Fig4_method_divergence_and_robustness
    Rscript --vanilla R/123_fig5_synthesis.R      -> figures/main/Fig5_response_definition_synthesis
    Rscript --vanilla R/124_fig6_sensitivity.R    -> figures/main/Fig6_sensitivity_analyses

Figure 2 and Supplementary Figure S4 (figures/extended/SupplementaryFigS4_reference_deviation_calibration)
come from R/97_annotated_evidence_forests.R as described below; Figure 1 is described in the 14 October
section. Set ER_ARCHIVE_ROOT to the archive root first. These scripts re-arrange archived estimates and study
cells; no model, estimate or BH rule was changed.

Figure layout of 14 October 2026: figures/main holds only the six submitted main figures, and
figures/extended holds Supplementary Figures S1-S13 (plus bibliometrics_standard). Final figure sources:

    Figure 1   R/113 -> figures/main_beautified, copied unchanged to figures/main (submitted version)
    Figure 2   R/97          Figure 3   R/121        Figure 4   R/122
    Figure 5   R/123         Figure 6   R/124
    S5         R/97          S6, S11    R/98 (sourced by R/97)
    S7         R/130_supp_figS7_polish.R (190 x 185 mm)
    S8         R/19_ecological_guild_meta.R (190 x 95 mm)
    S9         R/21a_ecological_moderator_tests.R (190 x 178 mm)
    S10        R/04 (rerouted so that it no longer overwrites the submitted S10)

save_figure() in R/00_setup.R routes any other figures/main family, and the superseded taxonomy extended
figure, to figures/_superseded_intermediate; earlier drafts were moved there as well (excluded from the ZIP).
All heatmaps (Figure 3, S7 b-c, S8 a-b, S9 a-d, S11 b) use coord_fixed() with zero expansion, so tiles are
square. Only plotting code changed; all estimates, intervals, q values and source-data tables are unchanged.
Legacy or preview scripts (R/22, R/95, R/101, R/105 and the _fig3_* previews) may still write to old folder
paths and are not part of the submitted figure set. The supplementary DOCX captions are the final wording;
the captions in build_revision_documents.ps1 may lag behind them.

Revision of 11 October 2026: Supplementary Tables S19-S21 are produced by three further entry points
(outputs in data/source_data unless noted):

    Rscript --vanilla R/125_rank_uncertainty.R               -> Rank_uncertainty_*.csv (S19; StudyID bootstrap, B = 2,000)
    Rscript --vanilla R/126_settlement_rule_sensitivity.R    -> Settlement_*.csv (S21; incl. source-audit template)
    Rscript --vanilla R/127_within_study_stage_contrasts.R   -> Within_study_stage_contrasts.csv (S20);
                                                                data/processed/Within_study_stage_block_composition.csv
    python R/128_settlement_source_corrections.py            -> source-verified settlement corrections applied to
                                                                data/processed and Review_exposure_recovery.csv;
                                                                Settlement_source_corrections.csv (S21). Run by run_all.R.
    Rscript --vanilla R/129_common_resample_contrasts.R      -> data/source_data/step3_tableA_*.csv (SI common
                                                                StudyID resample of stage contrasts; seed 20260928,
                                                                B = 2,000). Standalone; not called by run_all.R.

13 October 2026: R/121 Fig. 3 x-axis title shortened to "Directional lnRR" (the key is in the legend).

Label-only edits in this revision: R/121 (Fig3c legend wording) and R/97 (Fig4a y-axis "Rank (1st = most
affected)"). R/78_make_all_estimates_workbook.py adds the S19-S21 sheets to the workbook.

# Current final package: 26 September 2026

Read README_FINAL_20260926.md for the current verified entry point, package inventory and Windows locale fix. The 19 September instructions below are historical and are superseded where they differ, including Figure 5 content and final figure dimensions.

---

# Environmental Research: source and experimental-unit revision

## Final figure revision: 19 September 2026

The narrative asks what direction, performance and magnitude each reveal. The submitted figure order is: 1 evidence structure; 2 subgroup response means; 3 subgroup paired differences; 4 primary matched response definitions and paired differences; 5 inferential sensitivities; 6 departure distributions, calibration and a three-question conclusion matrix (panel e). Results complete the subgroup analyses, distinguish matched comparisons from broader evidence, and present directional, aligned-performance and magnitude conclusions before sensitivities. Historical source-table filenames retain their original figure prefixes; `data/source_data/Final_figure_source_map.csv` maps them to the final figures.

To reproduce the final figures from the archived numerical outputs, run `Rscript R/97_annotated_evidence_forests.R` from an ASCII-only copy of this archive. This entry point generates all six figures and invokes script 100 for the three-question summary. Run `Rscript R/94_verify_final_estimates.R` to verify 207 subgroup fits, their confidence intervals, paired means and BH corrections. Build and render documents from the canonical manuscript body. Script 78 rebuilds and round-trip checks the Excel attachment. Do not run intermediate text synchronizers after final reporting.

Figure 1 preserves all 138 species in a single continuous column, with Arial italic names. Its 210 x 430 mm master is intentionally tall; automatic reduction to a standard single-column or A4 page makes the names too small. Figures 2, 3 and 5 are 210 x 330 mm masters; Figure 4 is 210 x 280 mm and Figure 6 is 210 x 295 mm. These require full-width placement and production-size review. The final production size of Figure 1 needs editorial confirmation; no claim is made that the tall master fits a standard printed page. The current journal guide could not be retrieved during this check (HTTP 403).

Only signed comparisons receive zero-null P and BH q values. The primary paired family has 0/9 significant contrasts; the subgroup paired family has 0/69. Black stars in Figure 2 and Figure 4a test signed means against zero and must not be read as tests between definitions. Figure 4b prints q values, paired mean differences and cell counts. Finite descriptive differences are labelled `Descriptive`, not `NE`. No model estimates were changed by the final visual revision.

This secondary analysis of curated evidence incorporates source-backed corrections completed on 13 September 2026. Raw inputs are preserved. Historical source labels containing “validated” are legacy identifiers, not a claim of independent validation by this reanalysis.

## Reproduction

Use an ASCII-only working path on Windows. The local run used R 4.6.1 and Python 3.12.14. Set `ER_PYTHON` to the Python executable and run `Rscript run_all.R` from the archive root. R packages: readr, dplyr, tidyr, stringr, forcats, ggplot2, patchwork, RColorBrewer, metafor, scales, robumeta, clubSandwich, igraph, ggrepel, gridExtra, ape, jsonlite. Python: numpy, pandas, openpyxl, matplotlib, Pillow; PDF/DOCX verification additionally uses PyMuPDF and python-docx. Arial is required for the submitted figures. Cached sources permit offline reproduction.

The workflow removes 32 cross-layer copies from 2,412 input records, restores 11 dose-distinct records, repairs source pointers, applies represented-endpoint direction rules, and re-extracts experimental-unit effects. The final database contains 2,391 effects from 162 StudyIDs; the primary subset contains 2,214 effects from 149 StudyIDs. Identical outcomes at different doses remain distinct. CSV type inference examines all rows to prevent sparse source fields being silently converted to missing values.

Scripts 42–44, 49 and 51 repair sources and experimental units before fitting. Script 46 adds dose adjustment, published-consensus-only eligibility, Study 244 exclusion, restored-dose and explicit-covariance sensitivities. Script 31 produces an intermediate draft; scripts 50, 61, 64 and 67 update source-verification reporting. Scripts 69 and 72 build intermediate figures; script 74 produces the final paired-comparison figures and script 75 performs the final text synchronization after script 70. Script 71 (also invoked by script 39 with the package directory argument) checks numerical and document consistency. R produces the figure exports and previews.

On Windows, `build_revision_documents.ps1 -ArchivePath <archive> -PackagePath <output>` rebuilds six DOCX files from raw templates and final results. `render_documents.ps1 -PackagePath <output>` uses Word for PDFs. Raw templates are historical inputs, not submission documents. Script 71 checks the resulting documents; script 36 assembles the package.

## Statistical definitions and families

Directional lnRR refers to the represented endpoint. The source had already transformed mortality/abnormality to survival/normality; these effects must not be flipped again. Alignment uses published metric consensus or explicit positive represented outcomes. Ambiguous and unsupported mappings are excluded. A further sensitivity retains exact published-consensus matches only.

Figures 2 and 4a use matched rows, study-cell variances with rho = 0.8, REML and Hartung-Knapp intervals. Folded magnitudes have no zero-null P value. Supplementary Table S18 reports 27 estimable paired differences; only the nine signed aligned-minus-directional contrasts receive centered-null bootstrap P values and BH correction, and none is significant. Folded absolute contrasts are descriptive. Figure 3 applies the paired procedure to ecological subgroups: 69 signed aligned-minus-directional contrasts are tested and none is significant. Primary pairwise stage contrasts remain unsupported after correction.

Dose models use driver-specific temperature increments and log2 pCO2 treatment/control ratios with stage and control-temperature adjustment. StudyID-correlated RVE uses small-sample corrections, rank and sample-size checks, and separate BH families. Acclimation was not substituted for treatment duration. All six adjusted stage tests are retained, including non-significant results.

## Experimental units and sources

Study 243 chemistry is averaged through technical-replicate, aquarium-week and aquarium levels. Ten morphology effects use aquarium means. Behavioural individual-to-aquarium links remain unresolved.

Study 244 uses equal-header means of observed tank means and a joint 9,999-draw uncertainty calculation. Genets receive positive mean-one exponential weights within species/site, shared across treatments; headers and tanks are resampled hierarchically with common draws across species. Its complete 24-effect covariance is archived and used in an explicit-covariance sensitivity. Only two headers per treatment limit inference; an exclusion analysis is also supplied. Missing or dead colonies were not assigned zero growth.

`data/raw/boundary_sources` preserves the original workbook, published data, expert scores and raw experiments. `Boundary_source_pointer_repair.csv` records corrected locators. `Review_exposure_recovery.csv` contains 2,343 temperature contrasts, 2,335 treatment pCO2 values and 69 duration records. Study 244 separates approximately eight months of treatment from the near-one-year study and final assay; the source's eight/nine-week wording discrepancy remains documented.

Six main figures remain in `figures/main`. Figure S12 is the complete labelled tree; S13 shows exposure-adjusted slopes. Tables S14–S16 contain new sensitivities and experimental-unit analyses. `Boundary_manuscript_summary.json` and the numerical ledger trace text to results. The data archive is `ER_data_and_code_archive_taxonomy_meta_final_review_20260918.zip`.

The server dependencies use a dedicated user library. Script 53 checks primary coefficients on Linux against local results without overwriting the old server project.

## Endpoint source verification, 14 September 2026

Script 43 applies source-verified exclusions and aliases. All downstream analyses were rerun using script 66; run_all.R executes the complete workflow. Final reporting uses scripts 61, 64 and 67, in that order. Table S17 and Source_verification_resolutions_36_entries.csv retain provenance and all 36 technical decisions (14 changed direction rules). audit-output agreement is retained as originally computed; it is not human reliability. Author endorsement of the earlier audit is distinct from final approval of the subsequent source corrections. Full/primary aligned subsets now contain 882/841 effects. The paired families contain 

Current figure revision: the six-figure layout and final figure numbers are specified at the top of this README. Script 97 is the final rendering entry point. It invokes script 93 for baseline panels and script 98 for annotated supplementary forests, superseding intermediate visual exports. Script 78 exports and round-trip verifies the Excel tables, including the forest display audit.

Annotated forests show estimates and contributing study or cell counts in black. Small translucent dots are real archived study-cell observations; large symbols and intervals are summaries. Scatter is not added to adjusted regression slopes or CR2 contrasts, which have no directly corresponding raw observations. Forest axes use asinh(x/0.15) positions where stated; tick labels and estimates remain in original units. CR2 contrasts retain a linear axis. Stars use archived BH q values only. Main Figures 2, 3 and 5 are 210 x 330 mm masters; Figure 4 is 210 x 280 mm. These are full-width masters, not single-column figures. Production-size reduction must preserve readable type.

Distribution panels in Figure 6 use actual aggregated study cells, not simulated distributions from pooled confidence intervals. Figure 5 retains the inferential sensitivity analyses described by its final caption.

The clean submission includes all 13 supplementary figures as standalone vector PDFs in `supplementary_figures`, in addition to their manuscript previews. Inspect these originals for typography rather than relying on reduced DOCX previews. The S5 PDF is the complete reference-deviation calibration panel set underlying main Figure 5.

Final inference rule: signed aligned-minus-directional contrasts are tested; folded absolute contrasts are descriptive because their differences are structurally non-negative.

Revision of 30 October 2026: R/130_rank_fixed_candidate_set.R re-runs the StudyID bootstrap of R/125 with the same data, fits and seeds and records, per resample, subgroup estimability and whether both definitions had the same candidate set (Supplementary Table S9 note). Run `Rscript --vanilla R/130_rank_fixed_candidate_set.R` -> data/source_data/Rank_uncertainty_fixed_candidate_set.csv and Rank_uncertainty_subgroup_estimability.csv; the script stops unless P_top_differs reproduces Rank_uncertainty_panel_summary.csv exactly. It is not called by run_all.R.

Revision of 31 October 2026: Supplementary Figures S11 and S13 are redrawn by R/131_s11_s13_aggregate_funnels.R from the StudyID-by-stage inverse-variance aggregates that the Egger-type test in R/02_meta_models.R analyses, with the study-aggregated REML mean as reference line. Run `Rscript --vanilla R/131_s11_s13_aggregate_funnels.R` after run_all.R (R/03_figures.R draws the earlier effect-size-level versions under the same file names); the script stops unless study counts, effect counts, Egger z and REML means reproduce egger_small_study_checks.csv and main_results_REML.csv. Its check table is data/source_data/S11_S13_aggregate_check.csv.

Revision of 31 October 2026 (review round 15): R/132_bias_prediction_zero_settlement_sensitivity.R fits PET/PEESE-type small-study adjustment of the primary RVE directional stage means, bounds the omitted 0% settlement arm of StudyID 74 at -1.120 and ln(0.01) = -4.605 lnRR, and extracts 95% prediction intervals of the study-aggregated REML stage models. Run `Rscript --vanilla R/132_bias_prediction_zero_settlement_sensitivity.R` after run_all.R -> data/source_data/Review15_bias_PI_zero_sensitivity.csv (Supplementary Table S22; workbook sheet "STab22 bias PI zero"). It is not called by run_all.R.

Revision of 1 November 2026 (review round 16): R/133_settlement_candidate_addition_sensitivity.R refits the primary RVE stage models after adding three settlement effects from Bates et al. (2025), removing StudyID 23 row 1083 and omitting StudyID 23 (data/source_data/Settlement_candidate_addition_sensitivity.csv; Supplementary Table S24; per-candidate and per-study checklists Settlement_candidate_checklist_partA.csv and Settlement_final_studies_checklist_partB.csv; extraction Settlement_candidate_Bates2025_extraction.csv). R/135_definition_mixing_traceable_examples.R writes Definition_mixing_traceable_examples.csv and Definition_mixing_concern_summary.csv (Supplementary Table S23). R/134_fig5_sensitivity_synthesis.R draws main Figure 5 a-g (Fig5_round16_panels_abd_source_data.csv) and must run after R/133. Run them with `Rscript --vanilla R/133_... R/135_... R/134_...` after run_all.R; they are not called by run_all.R.

Revision of 1 November 2026 (review round 17): wording only; no estimate changed. R/134_fig5_sensitivity_synthesis.R relabels Figure 5 panels ("All-study RVE", "Endpoint-level absolute response", "95% subsampling interval"). StudyID 23 row 1083 is unchanged in the primary database. The database was frozen before Bates et al. (2025) was identified; Bates enters only the sensitivity analysis in Supplementary Table S24.

Revision of 1 November 2026 (review round 18). R/133_settlement_candidate_addition_sensitivity.R no longer pools Bates et al. (2025): its means (averaged over five trial days) and SDs (within-day between-tank) do not match, so Settlement_candidate_Bates2025_extraction.csv is kept as a qualitative record only. StudyID 23 rows 1077 (acidification only) and 1083 (warming only) are distinct treatment cells that share a reported mean (both match Alter et al. 2024); 1083 stays in the primary database and its removal is a check, not a correction. R/134 panel d redrawn without the Bates specifications. No primary estimate changed.

Revision of 1 November 2026 (review round 19): R/136_exclude_unverifiable_variance.py holds the six StudyID 23 settlement records (LatestModelRowID 1077, 1083, 1086, 1095, 1096, 1097) out of both analysis-ready datasets and writes them to data/source_data/Settlement_variance_unverifiable_StudyID23.csv. Their replicate unit is the larva (n = 135; three tanks), the compilation variance (Alter et al. 2024, Eq. 4) has no independent-unit term, and the full text needed to recompute tank-level variances was not accessible, so the variances cannot be verified. run_all.R calls the script after R/42 and again after the second R/128 call; it is idempotent. Main analyses therefore use 2,385 effects (primary 2,208; settlement/metamorphosis 35 effects from 9 StudyIDs). R/133 restores the six records (with and without row 1083) as the Supplementary Table S24 sensitivity; R/131, R/134, R/135 and R/83 were updated for the nine-study settlement stage (settlement PET/PEESE and Egger tests are not fitted below 10 StudyIDs). Run R/131, R/133, R/135 and R/134 after run_all.R as described for review round 16.

Revision of 4 November 2026 (review round 20): main Figure 5 is redrawn so that every panel compares the three response definitions (directional lnRR, endpoint-aligned lnRR, endpoint-level absolute response) in aligned columns. R/138_three_definition_archives.R refits each Figure 5 analysis for all three definitions and writes data/source_data/Fig5_three_definitions_{RVE_rho,RVE_contrasts,omnibus,within_study_contrasts,leave_one_out,settlement,signed_REML,balanced_stage}.csv; R/134_fig5_sensitivity_synthesis.R reads them and writes Fig5_three_definitions_panels_source_data.csv. The former panel g (magnitude summaries) remains Supplementary Figure S3. Run R/138 then R/134 after R/133. Directional estimates are unchanged; absolute-magnitude estimates are descriptive.

Revision of 5 November 2026 (review round 21): figure text set to black throughout. R/139_supp_figS6_standards.R and R/140_supp_figS12_standards.R redraw Supplementary Figures S6 and S12 from the same archived source data as R/98 (run after R/98; they do not write the display-audit file). Text colour in the assembled main Figures 1-4 and the other supplementary PDFs was changed in the PDF files only; no estimate changed.
