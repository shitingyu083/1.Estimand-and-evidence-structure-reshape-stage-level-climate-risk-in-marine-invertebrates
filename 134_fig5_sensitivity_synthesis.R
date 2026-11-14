# Figure 5 (review round 20): primary stage estimates and their sensitivity,
# every panel comparing the three response definitions in the same column order
# (Directional | Endpoint-aligned | Absolute magnitude), as in Figs 2-4.
#   a = primary RVE stage estimates (rho = 0.8)
#   b = stage differences from embryo: all-study RVE and within-study
#       (block fixed effects) contrasts, with the CR2 omnibus tests
#   c = leave-one-study-out jackknife (study-aggregated REML)
#   d = settlement/metamorphosis: source and data-integrity specifications
#   e = signed-REML working-correlation (rho) shift
#   f = equal-weight (balanced-cell) stage response (raincloud)
# Round 19 panel g (four magnitude summaries of one definition) is not a
# three-definition comparison and remains as Supplementary Fig. S3.
# The absolute column uses the endpoint-level absolute response (abs_lnRR), the
# form the Methods confine to sensitivity analysis; it carries no zero-null test.
# Inputs: archives written by R/138 (same engines as R/12, R/24, R/127, R/133 run
# on all three columns) plus the published settlement-rule files from R/133.
# Columns share one x scale per quantity: a, c and d use the same signed and
# absolute limits; panels are stacked full width so all columns align.
source(file.path("R", "00_setup.R"))
suppressPackageStartupMessages({library(ggplot2); library(ggdist); library(patchwork)})
src <- function(x) readr::read_csv(file.path(root_dir, "data/source_data", x),
                                   show_col_types = FALSE, guess_max = 100000)
prc <- function(x) readr::read_csv(file.path(root_dir, "data/processed", x),
                                   show_col_types = FALSE, guess_max = 100000)
DEFS <- c("Directional", "Endpoint-aligned", "Absolute magnitude")
def_f <- function(x) factor(x, levels = DEFS)
stage_y <- function(x) factor(x, levels = rev(stage_levels))
stg_short <- c(EMBRYO = "Embryo", LARVA = "Larva", `SETTLEMENT/METAMORPHOSIS` = "Settl./met.",
               `JUVENILE/ADULT` = "Juv./adult")
vline0 <- geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.32, colour = col("grey"))
note_col <- "black"
fs <- 6.5 / .pt
# Per-column x limits (signed columns share one range, absolute its own).
lim_frame <- function(signed, absolute) tibble::tibble(
  Definition = def_f(rep(DEFS, each = 2)), x = c(signed, signed, absolute))
blank_lims <- function(lf) geom_blank(data = lf, aes(x = x), inherit.aes = FALSE)
lab_minus <- function(x) sub("^-", "−", format(x, trim = TRUE, drop0trailing = TRUE))
brk_fun <- function(signed_brk, abs_brk) function(l) if (min(l) >= -1e-9) abs_brk else signed_brk
clamp <- function(df, lf) df %>% left_join(
  lf %>% group_by(Definition) %>% summarise(xlo = min(x), xhi = max(x), .groups = "drop"),
  by = "Definition") %>%
  mutate(lo = pmax(CI_low, xlo), hi = pmin(CI_high, xhi),
         clipped = CI_low < xlo | CI_high > xhi)
def_strip <- facet_grid(. ~ Definition, scales = "free_x", drop = FALSE)
panel_theme <- grid_x + theme(
  strip.background = element_blank(), strip.text = element_text(face = "bold", size = 7, colour = "black"),
  panel.spacing.x = unit(5, "mm"), legend.position = "bottom",
  legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-4, 0, 0, 0),
  legend.key.size = unit(3, "mm"), legend.text = element_text(size = 6.5),
  plot.margin = margin(2, 4, 2, 2))
LIM_SIGNED <- c(-0.50, 0.35); LIM_ABS <- c(0, 0.62)
lf_est <- lim_frame(LIM_SIGNED, LIM_ABS)
brk_est <- brk_fun(c(-0.4, -0.2, 0, 0.2), c(0, 0.2, 0.4, 0.6))
status_shapes <- c(`BH q < 0.05` = 16, `Not significant` = 1, `Descriptive (no zero-null test)` = 18,
                   `Alternative settlement rule` = 2)
y4 <- scale_y_continuous(breaks = 1:4, labels = rev(unname(stg_short[stage_levels])),
                         limits = c(0.6, 4.45), expand = c(0, 0))
miss_cell <- function(stages) tibble::tibble(Definition = def_f("Endpoint-aligned"),
                                             y = as.numeric(stage_y(stages)))
miss_txt <- function(df, x = 0) geom_text(data = df, aes(x = x, y = y), label = "not estimable",
                                          fontface = "italic", size = fs, colour = "black",
                                          inherit.aes = FALSE)

## ---- a : primary RVE stage estimates --------------------------------------
rv <- src("Fig5_three_definitions_RVE_rho.csv") %>% filter(Rho == 0.8)
alt <- src("Settlement_rule_sensitivity.csv") %>%
  filter(grepl("alternative rule", Specification), Stage == "SETTLEMENT/METAMORPHOSIS") %>%
  transmute(Definition = "Endpoint-aligned", ObservedStage = Stage, Estimate, CI_low, CI_high,
            P_value, FDR_q_value, StudyCount = StageStudyCount, EffectSizeCount = StageEffectSizeCount)
stopifnot(nrow(rv) == 11, nrow(alt) == 1,
          !any(rv$Definition == "Endpoint-aligned" & rv$ObservedStage == "SETTLEMENT/METAMORPHOSIS"))
pa_dat <- bind_rows(rv %>% mutate(Rule = "primary"), alt %>% mutate(Rule = "alternative")) %>%
  mutate(Definition = def_f(Definition), y = as.numeric(stage_y(ObservedStage)),
         Status = case_when(Rule == "alternative" ~ "Alternative settlement rule",
                            Definition == "Absolute magnitude" ~ "Descriptive (no zero-null test)",
                            FDR_q_value < 0.05 ~ "BH q < 0.05", TRUE ~ "Not significant"),
         lab = case_when(Rule == "alternative" ~ paste0("alt. rule, m = ", StudyCount, ", P = ",
                                                         formatC(P_value, 2, format = "f")),
                         Definition == "Absolute magnitude" ~ paste0("m = ", StudyCount),
                         TRUE ~ paste0("m = ", StudyCount, ", q = ", formatC(FDR_q_value, 3, format = "f")))) %>%
  clamp(lf_est)
pa <- ggplot(pa_dat) + vline0 + blank_lims(lf_est) +
  geom_errorbar(aes(xmin = lo, xmax = hi, y = y, colour = Definition), orientation = "y",
                width = 0, linewidth = 0.6) +
  geom_point(aes(Estimate, y, colour = Definition, shape = Status), size = 2.0, fill = "white", stroke = 0.7) +
  geom_label(aes(Estimate, y + 0.34, label = lab), size = fs, colour = note_col, fill = "white",
             linewidth = 0, label.padding = unit(0.4, "mm")) +
  def_strip + y4 +
  scale_x_continuous(breaks = brk_est, labels = lab_minus, expand = c(0, 0)) +
  scale_colour_manual(values = estimand_pal, guide = "none") +
  scale_shape_manual(values = status_shapes, name = NULL, drop = FALSE) +
  labs(x = "Primary RVE pooled response (ρ = 0.8; lnRR, 95% CI)", y = NULL) + panel_theme

## ---- b : stage differences from embryo -------------------------------------
btw <- src("Fig5_three_definitions_RVE_contrasts.csv") %>% filter(ReferenceStage == "EMBRYO") %>%
  transmute(Definition, ComparisonStage, Contrast, CI_low, CI_high, P_value, df,
            Design = "All-study RVE")
wth <- src("Fig5_three_definitions_within_study_contrasts.csv") %>%
  filter(Design == "Within-study (block fixed effects)", ReferenceStage == "EMBRYO") %>%
  transmute(Definition, ComparisonStage, Contrast,
            CI_low = Contrast - qt(0.975, df) * SE, CI_high = Contrast + qt(0.975, df) * SE,
            P_value, df, Design = "Within-study (block FE)", PairedStudyCount)
omn <- src("Fig5_three_definitions_omnibus.csv")
stopifnot(nrow(btw) == 8, nrow(wth) == 8, nrow(omn) == 3)
lim_b <- c(-0.8, 0.8)
y3 <- scale_y_continuous(breaks = 1:3, labels = rev(unname(stg_short[stage_levels[-1]])),
                         limits = c(0.4, 3.75), expand = c(0, 0))
pb_dat <- bind_rows(btw, wth) %>%
  mutate(Definition = def_f(Definition),
         y = as.numeric(factor(ComparisonStage, levels = rev(stage_levels[-1]))) +
           ifelse(Design == "All-study RVE", 0.13, -0.13),
         lo = pmax(CI_low, lim_b[1]), hi = pmin(CI_high, lim_b[2]),
         clipped = CI_low < lim_b[1] | CI_high > lim_b[2])
omn_lab <- omn %>% transmute(
  Definition = def_f(Definition),
  lab = ifelse(Definition == "Absolute magnitude", "Descriptive; differences of |lnRR|",
               sprintf("Omnibus F(%d, %.1f) = %.2f, P = %.3f", df_num, df_denom, Fstat, P_value)))
pb <- ggplot(pb_dat) + vline0 +
  geom_errorbar(aes(xmin = lo, xmax = hi, y = y, colour = Definition), orientation = "y",
                width = 0, linewidth = 0.55) +
  geom_point(aes(Contrast, y, colour = Definition, shape = Design), size = 1.8, fill = "white", stroke = 0.7) +
  geom_text(data = filter(pb_dat, clipped), aes(x = lim_b[2] - 0.02, y = y - 0.24,
            label = sprintf("CI truncated; df %.1f", df)), hjust = 1, size = fs, colour = note_col) +
  geom_label(data = omn_lab, aes(x = 0, y = 3.55, label = lab), size = fs, colour = note_col, fill = "white",
             linewidth = 0, label.padding = unit(0.4, "mm")) +
  miss_txt(tibble::tibble(Definition = def_f("Endpoint-aligned"), y = 2), x = 0) +
  facet_grid(. ~ Definition, drop = FALSE) + y3 +
  scale_x_continuous(breaks = c(-0.6, -0.3, 0, 0.3, 0.6), labels = lab_minus) +
  coord_cartesian(xlim = lim_b, expand = FALSE) +
  scale_colour_manual(values = estimand_pal, guide = "none") +
  scale_shape_manual(values = c(`All-study RVE` = 16, `Within-study (block FE)` = 22), name = NULL) +
  labs(x = "Difference from embryo (lnRR, 95% CI)", y = NULL) + panel_theme

## ---- c : leave-one-study-out jackknife --------------------------------------
per <- src("Fig5_three_definitions_leave_one_out.csv") %>%
  mutate(Definition = def_f(Definition), y = as.numeric(stage_y(ObservedStage)))
stopifnot(nrow(per) == 437, !anyNA(per$Definition))
full <- per %>% distinct(Definition, y, FullEstimate)
flag <- per %>% group_by(Definition, y) %>%
  summarise(cross = min(LeaveOneOutEstimate) < 0 & max(LeaveOneOutEstimate) > 0, .groups = "drop") %>%
  filter(cross)
pc <- ggplot(per, aes(LeaveOneOutEstimate, y)) + vline0 + blank_lims(lf_est) +
  geom_violin(aes(group = y, fill = Definition), scale = "width", width = 0.7, alpha = 0.28,
              colour = NA, orientation = "y") +
  geom_point(aes(colour = Definition), size = 0.5, alpha = 0.65,
             position = position_jitter(width = 0, height = 0.08, seed = 20261101)) +
  geom_point(data = full, aes(FullEstimate, y), shape = 23, size = 2.1, fill = "white",
             colour = "#3B335F", stroke = 0.6) +
  geom_text(data = flag, aes(x = 0, y = y + 0.34), label = "sign not robust", size = fs,
            colour = note_col) +
  miss_txt(miss_cell("SETTLEMENT/METAMORPHOSIS")) +
  def_strip + y4 +
  scale_x_continuous(breaks = brk_est, labels = lab_minus, expand = c(0, 0)) +
  scale_fill_manual(values = estimand_pal, guide = "none") +
  scale_colour_manual(values = estimand_pal, guide = "none") +
  labs(x = "Leave-one-study-out pooled response (study-aggregated REML; diamond = full sample)", y = NULL) +
  panel_theme

## ---- d : settlement/metamorphosis specifications ----------------------------
spec_lab <- c(`Main data (k = 35, m = 9)` = "Main data\n(k = 35, m = 9)",
              `StudyID 23 restored (k = 41, m = 10)` = "StudyID 23 restored\n(k = 41, m = 10)",
              `StudyID 23 restored, row 1083 removed` = "Restored, row 1083\nremoved (k = 40, m = 10)")
set3 <- src("Fig5_three_definitions_settlement.csv") %>%
  filter(Stage == "SETTLEMENT/METAMORPHOSIS") %>%
  transmute(Specification, Definition, Estimate, CI_low, CI_high, P_value, Rule = "primary")
add_alt <- src("Settlement_candidate_addition_sensitivity.csv") %>%
  filter(Stage == "SETTLEMENT/METAMORPHOSIS", Definition != "Directional") %>%
  transmute(Specification = unname(setNames(names(spec_lab),
              c("Main dataset (35 effects, 9 studies)", "StudyID 23 settlement records restored",
                "StudyID 23 restored without row 1083"))[Specification]),
            Definition = "Endpoint-aligned", Estimate, CI_low, CI_high, P_value, Rule = "alternative")
stopifnot(nrow(set3) == 6, nrow(add_alt) == 3, !anyNA(add_alt$Specification),
          setequal(set3$Definition, c("Directional", "Absolute magnitude")))
pd_dat <- bind_rows(set3, add_alt) %>%
  mutate(Definition = def_f(Definition),
         y = as.numeric(factor(Specification, levels = rev(names(spec_lab)))),
         Status = case_when(Rule == "alternative" ~ "Alternative settlement rule",
                            Definition == "Absolute magnitude" ~ "Descriptive (no zero-null test)",
                            P_value < 0.05 ~ "BH q < 0.05", TRUE ~ "Not significant")) %>%
  clamp(lf_est)
pd <- ggplot(pd_dat) + vline0 + blank_lims(lf_est) +
  geom_errorbar(aes(xmin = lo, xmax = hi, y = y, colour = Definition), orientation = "y",
                width = 0, linewidth = 0.55) +
  geom_point(aes(Estimate, y, colour = Definition, shape = Status), size = 1.9, fill = "white", stroke = 0.7) +
  def_strip +
  scale_y_continuous(breaks = 1:3, labels = rev(unname(spec_lab)), limits = c(0.55, 3.45), expand = c(0, 0)) +
  scale_x_continuous(breaks = brk_est, labels = lab_minus, expand = c(0, 0)) +
  scale_colour_manual(values = estimand_pal, guide = "none") +
  scale_shape_manual(values = status_shapes, guide = "none") +
  labs(x = "Settlement/metamorphosis RVE response (lnRR, 95% CI)", y = NULL) + panel_theme +
  theme(axis.text.y = element_text(lineheight = 0.85))

## ---- e : working-correlation (rho) shift in signed REML ----------------------
sr <- src("Fig5_three_definitions_signed_REML.csv") %>% filter(EvidenceSet == "All eligible endpoints")
stopifnot(nrow(sr) == 33)
pe_dat <- sr %>% group_by(Definition, ObservedStage) %>%
  mutate(Shift = Estimate - Estimate[Rho == 0]) %>% ungroup() %>%
  mutate(Definition = def_f(Definition), y = as.numeric(stage_y(ObservedStage)),
         RhoF = factor(formatC(Rho, 1, format = "f"), levels = c("0.0", "0.5", "0.8")))
LIM_E <- c(-0.035, 0.035)
stopifnot(max(abs(pe_dat$Shift)) < LIM_E[2])
pe_max <- pe_dat %>% group_by(Definition, y) %>%
  summarise(m = max(abs(Shift)), .groups = "drop") %>%
  mutate(lab = paste0("max ", formatC(m, 3, format = "f")))
pe <- ggplot(pe_dat, aes(Shift, y)) + vline0 +
  geom_line(aes(group = interaction(Definition, y), colour = Definition), linewidth = 0.35, alpha = 0.6) +
  geom_point(aes(colour = Definition, shape = RhoF), size = 1.6, fill = "white", stroke = 0.6) +
  miss_txt(miss_cell("SETTLEMENT/METAMORPHOSIS")) +
  facet_grid(. ~ Definition, drop = FALSE) + y4 +
  scale_x_continuous(breaks = c(-0.03, 0, 0.03), labels = lab_minus) +
  coord_cartesian(xlim = LIM_E, expand = FALSE) +
  scale_colour_manual(values = estimand_pal, guide = "none") +
  scale_shape_manual(values = c(`0.0` = 16, `0.5` = 17, `0.8` = 15), name = "Working ρ") +
  labs(x = "Change in signed-REML stage estimate from ρ = 0 (lnRR)", y = NULL) + panel_theme

## ---- f : equal-weight stage response over study-stage cells ------------------
coded <- prc("ER_endpoint_direction_row_coding.csv")
dset <- readr::read_csv(file.path(root_dir, "data/processed/analysis_ready_model_dataset_deduplicated.csv"),
                        show_col_types = FALSE, guess_max = 100000)
stopifnot(nrow(coded) == nrow(dset), identical(coded$StudyID, dset$StudyID))
base <- coded %>% mutate(DevelopmentalModeGroup = dset$DevelopmentalModeGroup,
                         abs_val = abs(lnRR)) %>%
  filter(DevelopmentalModeGroup == "With metamorphic stage", is.finite(vi_model), vi_model > 0,
         !is.na(StudyID), !is.na(ObservedStage))
cell_of <- function(v, def) base %>% filter(is.finite(.data[[v]])) %>%
  group_by(ObservedStage, StudyID) %>%
  summarise(yi = sum(.data[[v]] / vi_model) / sum(1 / vi_model), .groups = "drop") %>%
  mutate(Definition = def)
cells <- bind_rows(cell_of("lnRR", "Directional"), cell_of("aligned_lnRR_strict", "Endpoint-aligned"),
                   cell_of("abs_val", "Absolute magnitude")) %>%
  mutate(Definition = def_f(Definition), stg = stage_y(ObservedStage))
cnt <- function(d) as.integer(table(factor(cells$ObservedStage[cells$Definition == d], stage_levels)))
stopifnot(identical(cnt("Directional"), c(25L, 38L, 9L, 101L)),
          identical(cnt("Endpoint-aligned"), c(20L, 26L, 0L, 45L)),
          identical(cnt("Absolute magnitude"), c(25L, 38L, 9L, 101L)))
bal <- src("Fig5_three_definitions_balanced_stage.csv") %>%
  transmute(Definition = def_f(Definition), stg = stage_y(ObservedStage), Estimate = EqualWeightEstimate,
            CI_low = BalancedCI_low, CI_high = BalancedCI_high, Common = CommonBalancedStudyCount)
stopifnot(nrow(bal) == 11)
lf_f <- lim_frame(c(-1.4, 1.1), c(0, 1.8))
pf <- ggplot() + vline0 + geom_blank(data = lf_f, aes(x = x), inherit.aes = FALSE) +
  ggdist::stat_slab(data = cells, aes(x = yi, y = stg, fill = Definition), side = "top", scale = 0.60,
                    justification = -0.06, adjust = 0.85, colour = NA, alpha = 0.45) +
  geom_boxplot(data = cells, aes(x = yi, y = stg), width = 0.11, outlier.shape = NA, fill = "white",
               linewidth = 0.34, colour = col("grey_dark"), position = position_nudge(y = -0.02)) +
  ggdist::stat_dots(data = cells, aes(x = yi, y = stg, fill = Definition), side = "bottom", scale = 0.40,
                    justification = 1.14, dotsize = 0.9, colour = NA) +
  geom_errorbar(data = bal, aes(xmin = CI_low, xmax = CI_high, y = stg), orientation = "y", width = 0,
                linewidth = 0.6, colour = col("grey_dark"), na.rm = TRUE) +
  geom_point(data = bal, aes(Estimate, stg), shape = 23, size = 2.1, fill = "white", colour = "#3B335F",
             stroke = 0.6) +
  geom_text(data = tibble::tibble(Definition = def_f("Endpoint-aligned"),
                                  stg = stage_y("SETTLEMENT/METAMORPHOSIS")),
            aes(x = 0, y = stg), label = "not estimable", fontface = "italic", size = fs,
            colour = "black", inherit.aes = FALSE) +
  def_strip +
  scale_y_discrete(labels = stg_short, drop = FALSE) +
  scale_x_continuous(breaks = brk_fun(c(-1, -0.5, 0, 0.5, 1), c(0, 0.5, 1, 1.5)),
                     labels = lab_minus, expand = c(0, 0)) +
  scale_fill_manual(values = estimand_pal, guide = "none") +
  labs(x = "Study-stage cell response (lnRR; |lnRR| for absolute magnitude)\ndiamond = equal-weight mean, 95% subsampling interval",
       y = NULL) + panel_theme

## ---- assemble ---------------------------------------------------------------
fig <- pa / pb / pc / pd / pe / pf +
  plot_layout(heights = c(1, 0.85, 1, 0.8, 0.9, 1.25)) +
  plot_annotation(tag_levels = "a")
out_base <- Sys.getenv("FIG5_OUT", "figures/review16/Fig5_sensitivity_and_robustness")
save_figure(fig, out_base, 183, 235, 600)

sd_out <- bind_rows(
  pa_dat %>% transmute(Panel = "a", Definition = as.character(Definition), Row = as.character(ObservedStage), Estimate, CI_low, CI_high,
                       Note = Status),
  pb_dat %>% transmute(Panel = "b", Definition = as.character(Definition), Row = paste(ComparisonStage, Design),
                                            Estimate = Contrast, CI_low, CI_high, Note = ""),
  per %>% transmute(Panel = "c", Definition = as.character(Definition), Row = paste0(ObservedStage, " omit study ", OmittedStudyID),
                    Estimate = LeaveOneOutEstimate, CI_low = NA_real_, CI_high = NA_real_,
                    Note = paste0("full = ", FullEstimate)),
  pd_dat %>% transmute(Panel = "d", Definition = as.character(Definition), Row = Specification, Estimate, CI_low, CI_high, Note = Status),
  pe_dat %>% transmute(Panel = "e", Definition = as.character(Definition), Row = paste0(ObservedStage, " rho ", Rho), Estimate = Shift,
                       CI_low = NA_real_, CI_high = NA_real_, Note = paste0("estimate = ", Estimate)),
  bal %>% transmute(Panel = "f", Definition = as.character(Definition), Row = as.character(stg), Estimate, CI_low, CI_high,
                    Note = paste0("common balanced count = ", Common)),
  cells %>% transmute(Panel = "f", Definition = as.character(Definition), Row = paste0(ObservedStage, " study ", StudyID), Estimate = yi,
                      CI_low = NA_real_, CI_high = NA_real_, Note = "study-stage cell"))
readr::write_csv(sd_out, file.path(root_dir, "data/source_data/Fig5_three_definitions_panels_source_data.csv"))
