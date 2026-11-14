options(stringsAsFactors = FALSE)
source(file.path("R", "05_fig1_schematic.R"))
source(file.path("R", "20_opentree_phylogeny.R"))
bibliometrics_out <- file.path(root_dir, "figures", "extended", "bibliometrics_standard")
bibliometrics_tmp <- tempfile(pattern = "er_bibliometrics_")
dir.create(bibliometrics_tmp, recursive = TRUE, showWarnings = FALSE)
er_palette_saved <- pal
Sys.setenv(BIBLIOMETRICS_DATA_DIR = file.path(root_dir, "data", "source_data"),
           BIBLIOMETRICS_OUT_DIR = bibliometrics_tmp)
source(file.path("R", "bibliometrics_standard.R"))
pal <- er_palette_saved
dir.create(bibliometrics_out, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(bibliometrics_tmp, full.names = TRUE), bibliometrics_out, overwrite = TRUE)
unlink(bibliometrics_tmp, recursive = TRUE, force = TRUE)

# Keep the standard bibliometric panels in the extended context figure.
# The main figure now prioritizes the OpenTree phylogeny and the evidence
# structure panels that directly feed the estimand argument.
p_e <- p_b
p_f <- p_emergence + labs(subtitle = "Colour = theme; line = first-to-last occurrence; circle = peak year; size = frequency") + guides(colour = "none", size = "none")
p1a_opentree <- make_opentree_plot(compact = FALSE)
fig1_final <- (p1a_opentree / (p1b | p1c | p1d)) +
  patchwork::plot_layout(heights = c(1.45, 1.0)) +
  patchwork::plot_annotation(tag_levels = "a")

save_figure(fig1_final, "figures/main/Fig1_classification_and_evidence_update", 190, 220, dpi = 1000)
fig1_bibliometrics <- (p_e | p_f) +
  patchwork::plot_layout(widths = c(1.05, 1.15)) +
  patchwork::plot_annotation(tag_levels = "a")
save_figure(fig1_bibliometrics, "figures/extended/Fig1_standard_bibliometric_context", 190, 92, dpi = 1000)
writeLines(c(
  "Figure 1 logic: OpenTree species topology -> evidence coverage -> study-stage overlap -> endpoint interpretability.",
  "Panel a is the archived OpenTree synthetic induced subtree for TNRS-matched accepted species names; unit branches show topology only.",
  "The OpenAlex bibliometric context is rendered separately in the extended figure and does not define quantitative-study eligibility.",
  "All bibliometric calculations use openalex_records.csv (360 deduplicated records); display thresholds affect only network readability."
), file.path(root_dir, "analysis_logs", "Fig1_bibliometric_vector_render_log.txt"))
