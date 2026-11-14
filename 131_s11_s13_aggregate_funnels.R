# Review 14 (2026-10-31): redraw Supplementary Figs S11/S13 from the same
# StudyID-by-stage aggregates that egger_checks (R/02_meta_models.R) tests, so
# each plotted point is one study-stage cell of the "With metamorphic stage"
# subset and the reference line is the REML fit of exactly those cells.
# Run from the archive root after run_all.R (R/03_figures.R draws the earlier
# effect-size-level versions):  Rscript --vanilla R/131_s11_s13_aggregate_funnels.R
# Review 19 (2026-11-01): after the StudyID 23 settlement hold-out (R/136) the
# settlement stage has 9 studies, below the 10-study threshold of egger_checks;
# its funnel is still drawn but labelled "Egger test not run".
source(file.path("R", "00_setup.R"))
data_root <- root_dir

dat <- readr::read_csv(guess_max = 100000, file.path(data_root, "data", "processed", "analysis_ready_model_dataset_deduplicated.csv"),
                       show_col_types = FALSE, progress = FALSE) %>%
  mutate(ObservedStage = factor(ObservedStage, levels = stage_levels),
         DevelopmentalModeGroup = factor(DevelopmentalModeGroup, levels = mode_levels))
main_results <- readr::read_csv(guess_max = 100000, file.path(data_root, "data", "processed", "main_results_REML.csv"),
                                show_col_types = FALSE, progress = FALSE)
egger <- readr::read_csv(guess_max = 100000, file.path(data_root, "data", "processed", "egger_small_study_checks.csv"),
                         show_col_types = FALSE, progress = FALSE)

# Identical to R/02_meta_models.R
aggregate_for_model <- function(d, yi_col, group_cols) {
  d %>%
    filter(is.finite(.data[[yi_col]]), is.finite(vi_model), vi_model > 0) %>%
    group_by(across(all_of(c(group_cols, "StudyID")))) %>%
    summarise(yi = sum(.data[[yi_col]] / vi_model, na.rm = TRUE) / sum(1 / vi_model, na.rm = TRUE),
              vi = 1 / sum(1 / vi_model, na.rm = TRUE),
              RawEffectSizeCount = n(), .groups = "drop") %>%
    filter(is.finite(yi), is.finite(vi), vi > 0)
}
fit_agg <- function(d) metafor::rma.uni(yi = d$yi, vi = d$vi, method = "REML",
                                        control = list(maxiter = 80, stepadj = 0.5))

build <- function(yi_col, estimand) {
  agg <- aggregate_for_model(dat %>% filter(DevelopmentalModeGroup == "With metamorphic stage"),
                             yi_col, "ObservedStage")
  per <- lapply(split(agg, agg$ObservedStage, drop = TRUE), function(x) {
    fit <- fit_agg(x)
    # Same threshold as egger_checks in R/02_meta_models.R
    eg <- if (nrow(x) >= 10) metafor::regtest(fit, model = "rma", predictor = "sei") else list(zval = NA_real_, pval = NA_real_)
    list(pts = x, chk = tibble(ObservedStage = as.character(x$ObservedStage[1]),
                               k = nrow(x), n_es = sum(x$RawEffectSizeCount), ref = as.numeric(fit$b),
                               z = as.numeric(eg$zval), p = as.numeric(eg$pval)))
  })
  chk <- bind_rows(lapply(per, `[[`, "chk"))
  # Verification against archived outputs (must match exactly)
  e0 <- egger %>% filter(Estimand == estimand)
  m0 <- main_results %>% filter(GroupType == "mode_stage_all",
                                DevelopmentalModeGroup == "With metamorphic stage", Estimand == estimand)
  v <- chk %>% left_join(e0, by = "ObservedStage") %>%
    left_join(m0 %>% select(ObservedStage, MainEstimate = Estimate), by = "ObservedStage")
  print(v %>% transmute(ObservedStage, k, StudyCount, n_es, EffectSizeCount,
                        ref, MainEstimate, z, Egger_z, p, Egger_p))
  tested <- v %>% filter(!is.na(z))
  stopifnot(all(is.na(v$Egger_z) == is.na(v$z)), all(v$k[is.na(v$z)] < 10),
            all(tested$k == tested$StudyCount), all(tested$n_es == tested$EffectSizeCount),
            all(abs(tested$z - tested$Egger_z) < 1e-8), all(abs(v$ref - v$MainEstimate) < 1e-6))
  list(pts = bind_rows(lapply(per, `[[`, "pts")) %>% mutate(SE = sqrt(vi)), chk = chk)
}

panel <- function(b, xlab, title, contours) {
  pts <- b$pts %>% mutate(ObservedStage = factor(as.character(ObservedStage), levels = stage_levels))
  refs <- b$chk %>% mutate(ObservedStage = factor(ObservedStage, levels = stage_levels))
  se_max <- max(pts$SE) * 1.04
  x_lo <- min(pts$yi, refs$ref); x_hi <- max(pts$yi, refs$ref)
  pad <- 0.04 * (x_hi - x_lo); x_lo <- x_lo - pad; x_hi <- x_hi + pad
  refs <- refs %>% mutate(lab = ifelse(is.na(z), sprintf("'%d studies; Egger test not run (< 10 studies)'", k),
                                        sprintf("'%d studies; Egger '*italic(z) == '%s'*'; '*italic(P)*' %s'",
                                        k, formatC(z, 2, format = "f"),
                                        ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, 3, format = "f"))))))
  bg <- lapply(seq_len(nrow(refs)), function(i) {
    st <- refs$ObservedStage[i]; rf <- refs$ref[i]
    if (contours) {
      lay <- funnel_contours(ref = rf, se_max = se_max, xlim = c(x_lo, x_hi))
      if (i > 1L) lay <- lay[!vapply(lay, inherits, logical(1), what = "Scale")]
    } else {
      se <- seq(0, se_max, length.out = 120)
      ci <- data.frame(se = se, lo = rf - 1.96 * se, hi = rf + 1.96 * se)  # clipped by coord_cartesian
      lay <- list(
        geom_line(data = ci, aes(x = lo, y = se), inherit.aes = FALSE, linewidth = 0.3,
                  linetype = "dashed", colour = col("grey_dark")),
        geom_line(data = ci, aes(x = hi, y = se), inherit.aes = FALSE, linewidth = 0.3,
                  linetype = "dashed", colour = col("grey_dark")),
        geom_vline(xintercept = rf, linewidth = 0.34, colour = col("grey_dark")))
    }
    lapply(lay, function(l) { if (!is.null(l$data) && is.data.frame(l$data)) l$data$ObservedStage <- st; l })
  })
  p <- ggplot(pts, aes(yi, SE)) + bg +
    geom_point(shape = 21, fill = col("blue"), colour = "white", stroke = 0.25, size = 1.6, alpha = 0.75) +
    geom_label(data = refs, aes(x = x_lo, y = se_max, label = lab), parse = TRUE, inherit.aes = FALSE,
               hjust = 0, vjust = 1, linewidth = 0, fill = alpha("white", 0.8),
               label.padding = unit(0.6, "mm"), size = (er_base_size - 1.8) / .pt, colour = col("grey_dark")) +
    facet_wrap(~ ObservedStage, nrow = 2, labeller = as_labeller(stage_labels_1l)) +
    scale_y_reverse(expand = expansion(mult = c(0.06, 0.02))) +
    labs(x = xlab, y = "Standard error of study-stage mean", title = title) +
    coord_cartesian(xlim = c(x_lo, x_hi), clip = "on") +
    theme(panel.spacing = unit(4, "mm"), plot.margin = margin(6, 8, 6, 6))
  if (contours) p <- p + guides(fill = guide_legend(override.aes = list(colour = col("grey_light")))) +
    theme(legend.position = "right", legend.key.size = unit(3, "mm"))
  p
}

bD <- build("lnRR", "Directional lnRR")
bA <- build("abs_lnRR", "Absolute |lnRR|")
pS11 <- panel(bD, "Study-stage mean directional lnRR",
              "Contour-enhanced funnels; directional response, study-stage means", TRUE)
pS13 <- panel(bA, "Study-stage mean |lnRR|",
              "Precision funnels; absolute response, study-stage means", FALSE)
save_figure(pS11, "figures/extended/SupplementaryFigS11_directional_funnel_plots", 180, 134)
save_figure(pS13, "figures/extended/SupplementaryFigS13_absolute_funnel_plots", 180, 144)
readr::write_csv(bind_rows(bD$chk %>% mutate(Estimand = "Directional lnRR"),
                           bA$chk %>% mutate(Estimand = "Absolute |lnRR|")),
                 file.path(root_dir, "data", "source_data", "S11_S13_aggregate_check.csv"))
