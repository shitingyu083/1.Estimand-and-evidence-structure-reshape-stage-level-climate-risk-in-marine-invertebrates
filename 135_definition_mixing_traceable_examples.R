# Review round 16: traceable database examples of how reading one response
# definition as another changes the risk interpretation, and the checks in which
# the concern did not occur. Reads archived data only; no model is refitted.
# Output:
#   data/source_data/Definition_mixing_traceable_examples.csv  (row-level, with
#     LatestModelRowID, published-source locator and reference)
#   data/source_data/Definition_mixing_concern_summary.csv     (one row per check)
source(file.path("R", "00_setup.R"))
src <- function(x) readr::read_csv(file.path(root_dir, "data/source_data", x),
                                   show_col_types = FALSE, guess_max = 100000)
d  <- readr::read_csv(file.path(root_dir, "data/processed/analysis_ready_model_dataset_deduplicated.csv"),
                      show_col_types = FALSE, guess_max = 100000)
cc <- readr::read_csv(file.path(root_dir, "data/processed/ER_endpoint_direction_row_coding.csv"),
                      show_col_types = FALSE, guess_max = 100000)
stopifnot(nrow(d) == nrow(cc), identical(d$StudyID, cc$StudyID))
xw <- src("Review_reference_StudyID_crosswalk.csv") %>% select(StudyID, PaperAuthor, PaperYear, Journal) %>%
  distinct(StudyID, .keep_all = TRUE)  # StudyIDs 18 and 103 appear twice with spelling variants
rows <- cc %>%
  mutate(LatestModelRowID = d$LatestModelRowID, DevelopmentalModeGroup = d$DevelopmentalModeGroup,
         VerifiedSourceLocators = d$VerifiedSourceLocators) %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage") %>%
  left_join(xw, by = "StudyID")
cells <- src("Fig6b_magnitude_performance_cells.csv")
stopifnot(nrow(cells) == 271)

## Example 1: alignment reverses the sign of a cell (adverse-when-increasing endpoint)
diff_cells <- cells %>% filter(abs(D - A) > 1e-12)
stopifnot(nrow(diff_cells) == 6, setequal(unique(diff_cells$StudyID), c(46, 75)))
ex1 <- rows %>% filter(StudyID == 46, EndpointDirectionClassStrict == "adverse_when_increases") %>%
  mutate(Example = "E1 alignment reverses sign",
         Reading = ifelse(lnRR < 0,
                          "Directional decrease of shell damage; aligned value positive (positive aligned response)",
                          "Directional increase of shell damage; aligned value negative (negative aligned response)"))
stopifnot(nrow(ex1) == 8, all(sign(ex1$lnRR) == -sign(ex1$aligned_lnRR_strict)))

## Example 2: settlement magnitude built from opposite-sign studies
set_rows <- rows %>% filter(ObservedStage == "SETTLEMENT/METAMORPHOSIS")
stopifnot(nrow(set_rows) == 35, n_distinct(set_rows$StudyID) == 9, all(is.na(set_rows$aligned_lnRR_strict)))
ex2 <- set_rows %>% filter(StudyID %in% c(99, 74)) %>%
  mutate(Example = "E2 settlement magnitude from opposite signs",
         Reading = ifelse(lnRR > 0, "Settlement increase; counted as a large departure under the absolute definition",
                          "Settlement decrease; counted as a large departure under the absolute definition"))
set_cells <- set_rows %>% group_by(StudyID) %>%
  summarise(yi = sum(lnRR / vi_model) / sum(1 / vi_model), .groups = "drop")
stopifnot(abs(set_cells$yi[set_cells$StudyID == 74] - (-1.0038)) < 1e-3,
          abs(set_cells$yi[set_cells$StudyID == 99] - 0.2021) < 1e-3)
loso <- src("SupplementaryTableS7b_leave_one_out_per_omission.csv")
set_loso <- loso %>% filter(ObservedStage == "SETTLEMENT/METAMORPHOSIS")
dir74 <- set_loso %>% filter(Estimand == "Directional lnRR", OmittedStudyID == 74) %>% pull(LeaveOneOutEstimate)
abs_set <- set_loso %>% filter(Estimand == "Absolute |lnRR|") %>% pull(LeaveOneOutEstimate)
abs_other_max <- loso %>% filter(Estimand == "Absolute |lnRR|", ObservedStage != "SETTLEMENT/METAMORPHOSIS") %>%
  pull(LeaveOneOutEstimate) %>% max()
# Review 19: without the StudyID 23 records, omitting StudyID 74 moves the
# directional mean towards zero but not above it, and the absolute ranking is
# not robust to that omission; report the values instead of asserting them.
abs_full <- set_loso %>% filter(Estimand == "Absolute |lnRR|") %>% pull(FullEstimate) %>% unique()
abs74 <- set_loso %>% filter(Estimand == "Absolute |lnRR|", OmittedStudyID == 74) %>% pull(LeaveOneOutEstimate)
stopifnot(length(abs_full) == 1, abs_full > abs_other_max, abs74 < abs_other_max, dir74 < 0)

## Example 3: largest magnitudes that are positive aligned responses
ex3_cells <- cells %>% filter(MagnitudeBand == "Q4", A > 0)
stopifnot(nrow(ex3_cells) == 17,
          identical(as.integer(table(ex3_cells$StressorLabel)), c(5L, 6L, 6L)))
top72 <- rows %>% filter(StudyID == 72, ObservedStage == "JUVENILE/ADULT", StressorLabel == "Acidification")
stopifnot(nrow(top72) == 1, abs(top72$lnRR - 2.195) < 1e-9)
ex3 <- ex3_cells %>% left_join(xw, by = "StudyID") %>%
  transmute(Example = "E3 top-quartile magnitude with aligned increase", StudyID, ObservedStage, StressorLabel,
            CellDirectional = D, CellAligned = A, CellMagnitude = M, MagnitudeBand, PaperAuthor, PaperYear, Journal,
            Reading = "Top-quartile departure that is a positive aligned response")

row_cols <- c("Example", "StudyID", "PaperAuthor", "PaperYear", "Journal", "LatestModelRowID", "ObservedStage",
              "StressorLabel", "Category", "Metric", "EndpointDirectionClassStrict", "lnRR", "aligned_lnRR_strict",
              "VerifiedSourceLocators", "Reading")
out <- bind_rows(ex1 %>% select(all_of(row_cols)), ex2 %>% select(all_of(row_cols)),
                 top72 %>% mutate(Example = "E3 top-quartile magnitude with aligned increase",
                                  Reading = "Largest acidification magnitude cell; growth-rate increase") %>%
                   select(all_of(row_cols)),
                 ex3)
write_csv_safe(out, "data/source_data/Definition_mixing_traceable_examples.csv")

## Summary of checks: where the concern occurred and where it did not
s10 <- src("Fig4b_estimand_target_differences.csv") %>% filter(TargetDifference == "Aligned - directional")
mag <- src("Fig2c_magnitude_definition_sensitivity.csv") %>% filter(EvidenceSet == "All eligible endpoints")
ord <- mag %>% group_by(Definition) %>% arrange(StageRank, .by_group = TRUE) %>%
  summarise(order = paste(ObservedStage, collapse = ">"), .groups = "drop")
summ <- tibble::tribble(
  ~Check, ~Occurred, ~Evidence,
  "Alignment reverses the sign of a matched cell", "Yes, rarely",
  sprintf("%d of %d matched cells differ (StudyIDs 46 and 75); StudyID 46 shell-damage cells change sign (|lnRR| up to %.3f)",
          nrow(diff_cells), nrow(cells), max(abs(ex1$lnRR))),
  "Signed definitions disagree overall", "No",
  sprintf("%d of %d matched cells identical; nine aligned-minus-directional contrasts min q = %.3f",
          sum(abs(cells$D - cells$A) <= 1e-12), nrow(cells), min(s10$FDR_q_value, na.rm = TRUE)),
  "Magnitude ranks a stage first without a signed or aligned estimate", "Yes",
  sprintf("Settlement absolute mean %.3f ranks first (other stages' LOSO max %.3f) but LOSO spans %.3f to %.3f; omitting StudyID 74 gives %.3f; StudyID 74 cell %.3f and StudyID 99 cell %.3f; omitting StudyID 74 moves the directional mean to %.3f",
          abs_full, abs_other_max, min(abs_set), max(abs_set), abs74, set_cells$yi[set_cells$StudyID == 74],
          set_cells$yi[set_cells$StudyID == 99], dir74),
  "Magnitude stage ordering depends on magnitude definition", ifelse(n_distinct(ord$order) > 1, "Yes", "No"),
  sprintf("%d distinct orderings across %d definitions", n_distinct(ord$order), nrow(ord)),
  "Top-quartile magnitude cells include positive aligned responses", "Yes",
  sprintf("%d top-quartile cells (5 acidification, 6 warming, 6 combined) from %d StudyIDs; largest StudyID 72 growth rate +%.3f",
          nrow(ex3_cells), n_distinct(ex3_cells$StudyID), top72$lnRR))
write_csv_safe(summ, "data/source_data/Definition_mixing_concern_summary.csv")
print(as.data.frame(summ), right = FALSE)
cat("ex3 StudyIDs:", paste(sort(unique(ex3_cells$StudyID)), collapse = ", "), "\n")
cat("magnitude orderings:\n"); print(as.data.frame(ord))
