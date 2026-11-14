# Final main-figure assembly. This script changes visual organization only and
# reads all numerical results from the archived source-data tables.
source(file.path("R", "00_setup.R"))
source(file.path("R", "20_opentree_phylogeny.R"))

src <- function(x) readr::read_csv(guess_max=100000, file.path(root_dir, "data", "source_data", x), show_col_types = FALSE)
proc <- function(x) readr::read_csv(guess_max=100000, file.path(root_dir, "data", "processed", x), show_col_types = FALSE)
oi <- c(blue = "#0072B2", orange = "#E3864A", green = "#6F63A8", vermillion = "#D55E00",
        purple = "#6F63A8", sky = "#5A9FD0", black = "#3B335F", grey = "#978DC6")
driver_cols <- c("Acidification" = unname(oi["blue"]), "Warming" = unname(oi["vermillion"]),
                 "Warming + acidification" = unname(oi["purple"]))
target_cols <- c("Directional" = unname(oi["blue"]), "Aligned performance" = unname(oi["green"]),
                 "Absolute magnitude" = unname(oi["vermillion"]))

# Figure 1: standard rectangular topology plus the bibliometric and evidence
# structure panels that motivate the inferential problem.
p1a <- make_opentree_plot(compact = TRUE) +
  labs(x = "OpenTree topological depth (unit branches)") +
  theme(legend.position = "bottom", plot.margin = margin(2, 3, 1, 2))

# Keyword co-occurrence is descriptive and is kept separate from quantitative
# eligibility. Display thresholds reduce label collisions; full tables remain
# in data/source_data.
keyword_regex <- paste(c(
  "acidification", "warming", "invertebrate", "climate", "carbon", "larva",
  "embryo", "development", "settlement", "metamorphosis", "life history",
  "recruit", "growth", "survival", "calcif", "physiol", "reproduction", "benthic"
), collapse = "|")
keyword_group <- function(x) {
  case_when(
    str_detect(x, "acidification|warming|invertebrate|climate|carbon") ~ "Climate driver / system",
    str_detect(x, "larva|embryo|development|settlement|metamorphosis|life|recruit") ~ "Life-history stage",
    TRUE ~ "Response / process"
  )
}
theme_cols <- c("Climate driver / system" = unname(oi["blue"]),
                "Life-history stage" = unname(oi["vermillion"]),
                "Response / process" = unname(oi["purple"]))
kws <- src("bibliometrics_keywords.csv")
edges <- src("bibliometrics_keyword_edges.csv")
nodes <- kws %>% filter(document_count >= 3, str_detect(keyword, keyword_regex))
edges_display <- edges %>%
  filter(keyword_1 %in% nodes$keyword, keyword_2 %in% nodes$keyword, cooccurrence >= 2)
g <- igraph::graph_from_data_frame(edges_display, directed = FALSE,
                                   vertices = select(nodes, keyword))
g <- igraph::delete_vertices(g, igraph::V(g)[igraph::degree(g) == 0])
g <- igraph::set_vertex_attr(g, "freq", value = nodes$document_count[
  match(igraph::V(g)$name, nodes$keyword)])
set.seed(20260906)
lay <- igraph::layout_with_fr(g, weights = igraph::E(g)$cooccurrence, niter = 1500)
lay[, 1] <- scales::rescale(lay[, 1], to = c(-1, 1))
lay[, 2] <- scales::rescale(lay[, 2], to = c(-1, 1))
rownames(lay) <- igraph::V(g)$name
edge_names <- igraph::ends(g, igraph::E(g), names = TRUE)
edge_df <- tibble(
  keyword_1 = edge_names[, 1], keyword_2 = edge_names[, 2],
  x = lay[edge_names[, 1], 1], y = lay[edge_names[, 1], 2],
  xend = lay[edge_names[, 2], 1], yend = lay[edge_names[, 2], 2],
  cooccurrence = igraph::E(g)$cooccurrence
)
node_df <- tibble(
  keyword = igraph::V(g)$name, x = lay[, 1], y = lay[, 2],
  document_count = igraph::vertex_attr(g, "freq")
) %>%
  mutate(group = keyword_group(keyword)) %>%
  group_by(group) %>%
  mutate(label = if_else(rank(-document_count, ties.method = "first") <= 4,
                         stringr::str_wrap(str_to_title(keyword), 22), NA_character_)) %>%
  ungroup()
write_csv_safe(select(node_df, keyword, document_count, group, x, y, label),
               "data/source_data/Fig1b_keyword_network_nodes.csv")
write_csv_safe(edge_df,
               "data/source_data/Fig1b_keyword_network_edges_displayed.csv")
label_df <- node_df %>% filter(!is.na(label)) %>% arrange(x) %>%
  mutate(side=if_else(row_number()<=ceiling(n()/2),-1,1)) %>%
  group_by(side) %>% arrange(y,.by_group=TRUE) %>%
  mutate(label_y=seq(-1.2,1.2,length.out=n()), label_x=side*1.2,
         align=if_else(side<0,1,0), label=stringr::str_wrap(keyword,16)) %>% ungroup()
p1b <- ggplot() +
  geom_segment(data = edge_df,
               aes(x, y, xend = xend, yend = yend, linewidth = cooccurrence),
               colour = "#C3BDDF", alpha = .42, lineend = "round") +
  geom_point(data = node_df, aes(x, y, size = document_count, colour = group), alpha = .92) +
  geom_segment(data=label_df,aes(x,y,xend=label_x,yend=label_y),linewidth=.15,colour="#4A417A") +
  geom_text(data=label_df,aes(label_x,label_y,label=label,hjust=align),
            colour=oi["black"],size=2.3,lineheight=.95) +
  scale_linewidth(range = c(.10, .65), guide = "none") +
  scale_size(range = c(1.8, 6.6), name = "Documents", breaks = c(20, 60, 180)) +
  scale_colour_manual(values = theme_cols, name = "Theme",
                      labels = c("Climate", "Life stage", "Response/process")) +
  coord_cartesian(xlim = c(-3.4,3.4), ylim = c(-1.5,1.5),
              expand = FALSE, clip = "off") +
  labs(x = NULL, y = NULL) + theme_void(base_size = er_base_size) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "vertical", legend.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size = 5.8, face = "bold"),
        legend.text = element_text(size = 5.6),
        plot.margin = margin(4, 5, 3, 4))

stage_count <- src("Fig1a_stage_driver_counts.csv") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = stage_levels),
         StressorLabel = factor(StressorLabel, levels = stressor_levels))
p1c <- ggplot(stage_count, aes(ObservedStage, EffectSizeCount, fill = StressorLabel)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.15) +
  geom_text(aes(label = ifelse(EffectSizeCount>=30,as.character(EffectSizeCount),"")), position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 1.65) +
  scale_fill_manual(values = driver_cols, labels=c("Acidification","Warming","Combined exposure"), name = NULL) +
  scale_x_discrete(labels = stage_labels) +
  labs(x = NULL, y = "Effect sizes") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1))

records <- src("openalex_records.csv")
term_year <- records %>%
  select(year, keywords) %>%
  separate_rows(keywords, sep = ";") %>%
  transmute(keyword = str_to_lower(str_trim(keywords)), year = as.integer(year)) %>%
  filter(is.finite(year), keyword %in% nodes$keyword)
peak_year <- term_year %>% count(keyword, year, name = "annual_count") %>%
  group_by(keyword) %>% slice_max(annual_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(keyword, peak_year = year)
recent_terms <- term_year %>% filter(year >= 2020) %>%
  count(keyword, name = "recent_count")
emergence <- term_year %>%
  group_by(keyword) %>%
  summarise(first_year = min(year), last_year = max(year), .groups = "drop") %>%
  left_join(peak_year, by = "keyword") %>%
  left_join(select(kws, keyword, document_count), by = "keyword") %>%
  left_join(recent_terms, by = "keyword") %>%
  mutate(recent_count = coalesce(recent_count, 0L),
         recent_share = recent_count / document_count,
         group = keyword_group(keyword)) %>%
  group_by(group) %>%
  arrange(desc(recent_share), desc(document_count), .by_group = TRUE) %>%
  slice_head(n = 4) %>%
  ungroup() %>%
  mutate(keyword = str_replace(keyword, fixed("effects of global warming on oceans"),
                               "global warming (ocean)"),
         keyword = str_replace(keyword, fixed("carbon dioxide in earth's atmosphere"),
                               "atmospheric carbon dioxide"),
         keyword = factor(str_to_title(keyword), levels = rev(str_to_title(keyword))))
write_csv_safe(mutate(emergence, keyword = as.character(keyword)),
               "data/source_data/Fig1d_keyword_temporal_emergence.csv")
p1d <- ggplot(emergence, aes(y = keyword, colour = group)) +
  geom_segment(aes(x = first_year, xend = last_year, yend = keyword),
               linewidth = .48, alpha = .78, lineend = "round") +
  geom_point(aes(x = peak_year, size = document_count), shape = 21,
             fill = "white", stroke = .65) +
  scale_colour_manual(values = theme_cols, guide = "none") +
  scale_size(range = c(1.7, 4.8), name = "Documents", guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025),
                     expand = expansion(mult = c(.01, .025))) +
  labs(x = "Publication year", y = NULL,
       caption = "Line: first-last occurrence; point: peak year") +
  grid_x + theme(axis.text.y = element_text(size = 5.7),
                 plot.caption = element_text(size = 5.6, hjust = 0))

bal <- proc("ER_evidence_balance_by_stage.csv") %>%
  transmute(ObservedStage = factor(ObservedStage, levels = stage_levels),
            `Direction-interpretable` = AlignedEffectSizes,
            `Context-dependent` = ContextDependentEffectSizes,
            Total = EffectSizeCount) %>%
  pivot_longer(c(`Direction-interpretable`, `Context-dependent`), names_to = "Type", values_to = "n") %>%
  mutate(share = n / Total)
p1e <- ggplot(bal, aes(ObservedStage, share, fill = Type)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.15) +
  geom_text(aes(label = ifelse(n > 0, as.character(n), "")), position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 1.55) +
  scale_fill_manual(values = c("Direction-interpretable" = "#0072B2",
                               "Context-dependent" = "#D55E00"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .02))) +
  scale_x_discrete(labels = stage_labels) + labs(x = NULL, y = "Share of effects") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1))

tree_parts <- lapply(list(c(93,138),c(47,92),c(1,46)),function(r) {
  make_opentree_plot(compact=FALSE,tip_range=r)+
    labs(x=NULL,colour=NULL,caption=paste0("Tips ",r[1],"-",r[2]))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
      axis.line.x=element_blank(),legend.position="bottom",plot.margin=margin(3,1,2,1),
      legend.key.height=unit(2.5,"mm"),legend.title=element_text(size=6.5))+
    guides(colour=guide_legend(nrow=1))
})
p1a <- patchwork::wrap_plots(tree_parts,nrow=1,guides="collect")+
  plot_annotation(title="a   Species represented in the OpenTree topology",
    caption="One tree shown in three consecutive vertical ranges; dotted lines connect tips to names.",
    theme=theme(plot.title=element_text(size=8,face="bold"),plot.caption=element_text(size=6.5))) & theme(legend.position="bottom")
p1a <- patchwork::wrap_elements(full=patchwork::patchworkGrob(p1a))
fig1 <- p1a / ((p1b+labs(tag="b")) | (p1c+labs(tag="c"))) / ((p1d+labs(tag="d")) | (p1e+labs(tag="e"))) +
  plot_layout(heights = c(2.45, .70, .70))
save_figure(fig1, "figures/main/Fig1_classification_and_evidence_update", 190, 310, 600)

# Figure 2: the principal directional signal, its biological alignment, and the
# evidence support. Incomparable magnitude definitions remain supplementary.
rve <- src("Fig5a_RVE_rho_sensitivity.csv") %>% filter(Rho == .8) %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         Target = recode(Estimand, `Raw directional` = "Directional",
                         `Aligned performance` = "Aligned performance"))
forest_stage <- function(d, target, colour, show_y = TRUE) {
  dd <- filter(d, Target == target)
  ggplot(dd, aes(Estimate, ObservedStage)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .14,
                  linewidth = .52, colour = colour) +
    geom_point(shape = 21, size = 2.25, fill = colour, colour = "white", stroke = .28) +
    geom_text(aes(label = sprintf("%.2f [%.2f, %.2f]", Estimate, CI_low, CI_high)),
              nudge_y = .23, size = 1.75, colour = oi["black"], check_overlap = TRUE) +
    scale_y_discrete(labels = if (show_y) stage_labels_1l else NULL) +
    labs(x = paste0(target, " lnRR (95% CI)"), y = if (show_y) "Life-history stage" else NULL) + grid_x
}
p2a <- forest_stage(rve, "Directional", target_cols["Directional"], TRUE)
p2b <- forest_stage(rve, "Aligned performance", target_cols["Aligned performance"], TRUE)
ev <- src("Fig2_evidence_map.csv") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         StressorLabel = factor(StressorLabel, levels = stressor_levels),
         lab = sprintf("s=%s\nk=%s", StudyCount, EffectSizeCount))
# The folded magnitude target is shown descriptively alongside the two signed
# targets. It is deliberately not assigned a zero-null P value.
abs_stage <- src("Fig4a_common_subset_estimand_driver_estimates.csv") %>%
  filter(PanelLabel == "Absolute magnitude") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         StressorLabel = factor(StressorLabel, levels = stressor_levels))
p2c <- ggplot(abs_stage, aes(Estimate, ObservedStage, colour = StressorLabel)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
  geom_errorbar(aes(xmin = PI_low, xmax = PI_high), orientation = "y", width = 0,
                linewidth = .16, colour = "#978DC6", na.rm = TRUE) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .10,
                linewidth = .48, na.rm = TRUE) +
  geom_point(size = 1.95, shape = 15, na.rm = TRUE) +
  geom_text(aes(x = CI_high, label = sprintf("%.2f", Estimate)),
            hjust = -.18, size = 1.65, colour = oi["black"], check_overlap = TRUE, na.rm = TRUE) +
  facet_grid(. ~ StressorLabel, labeller = as_labeller(c(
    "Acidification" = "Acid.", "Warming" = "Warm.",
    "Warming + acidification" = "Combined"))) +
  scale_colour_manual(values = driver_cols, guide = "none") +
  scale_y_discrete(labels = stage_labels_1l) +
  scale_x_continuous(limits = c(-.08, .72), breaks = c(0, .50), labels = c("0", "0.5"),
                     expand = expansion(mult = c(.01, .02))) +
  labs(x = "Absolute departure from control (95% CI)", y = "Life-history stage",
       subtitle = "Folded magnitude is descriptive; no zero-null test is assigned") +
  grid_x + theme(plot.subtitle = element_text(size = 5.8, hjust = 0),
                 panel.spacing = unit(5, "mm"))
p2d <- ggplot(ev, aes(StressorLabel, ObservedStage, fill = StudyCount)) +
  geom_tile(colour = "white", linewidth = .22) +
  geom_text(aes(label = lab, colour = StudyCount > median(StudyCount, na.rm = TRUE)),
            size = 1.85, lineheight = .86, fontface = "bold") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#3B335F"), guide = "none") +
  scale_fill_gradientn(colours = c("#F4F2F9", "#A9CBE6", "#0072B2", "#0B4F7F"),
                       name = "Independent\nstudies (s)") +
  scale_y_discrete(labels = stage_labels_1l) +
  scale_x_discrete(labels = c("Acidification", "Warming", "Combined")) +
  labs(title = "Evidence support for stage-by-driver estimates",
       subtitle = "Colour encodes independent studies; labels show studies (s) and effects (k)",
       x = NULL, y = "Life-history stage") +
  theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "right")
fig2 <- (p2a | p2b) / (p2c | p2d) +
  plot_layout(heights = c(1.0, 1.0), widths = c(1.0, 1.0)) +
  plot_annotation(tag_levels = "a")
save_figure(fig2, "figures/legacy_exports_20260910/Fig3_stage_signal_evidence_audit", 190, 175, 1000)

# Figure 3: panel a displays target-specific estimates; panel b performs the
# paired inferential comparison. A forest layout makes all 27 q values visible.
est <- src("Fig4a_common_subset_estimand_driver_estimates.csv") %>%
  mutate(Target = recode(PanelLabel, Directional = "Directional",
                         `Aligned performance` = "Aligned performance",
                         `Absolute magnitude` = "Absolute magnitude"),
         Target = factor(Target, levels = c("Directional", "Aligned performance", "Absolute magnitude")),
         StageIndex = recode(ObservedStage, `JUVENILE/ADULT` = 1, `SETTLEMENT/METAMORPHOSIS` = 2,
                             LARVA = 3, EMBRYO = 4),
         TargetOffset = recode(as.character(Target), Directional = .24,
                               `Aligned performance` = 0, `Absolute magnitude` = -.24),
         PlotY = StageIndex + TargetOffset,
         StressorLabel = factor(StressorLabel, levels = stressor_levels))
p3a <- ggplot(est, aes(Estimate, PlotY, colour = Target, shape = Target)) +
  annotate("text",x=0,y=2,label="NE: no eligible endpoints",size=2.1,colour="#4A417A") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
  geom_errorbar(aes(xmin = PI_low, xmax = PI_high), orientation = "y", width = 0,
                linewidth = .16, colour = "#978DC6", na.rm = TRUE) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .12,
                linewidth = .48, na.rm = TRUE) +
  geom_point(size = 1.95, fill = "white", stroke = .30, na.rm = TRUE) +
  facet_grid(. ~ StressorLabel, labeller = as_labeller(c(
    "Acidification" = "Acidification", "Warming" = "Warming",
    "Warming + acidification" = "Combined exposure"))) +
  scale_colour_manual(values = target_cols, name = NULL,
    labels=c(Directional='Directional', `Aligned performance`='Endpoint-aligned', `Absolute magnitude`='Absolute aggregated')) +
  scale_shape_manual(values = c("Directional" = 16, "Aligned performance" = 17,
                                "Absolute magnitude" = 15), name = NULL,
    labels=c(Directional='Directional', `Aligned performance`='Endpoint-aligned', `Absolute magnitude`='Absolute aggregated')) +
  scale_y_continuous(breaks = 1:4,
                     labels = c("Juvenile/adult", "Settlement/metamorphosis", "Larva", "Embryo"),
                     limits = c(.62, 4.40)) +
  labs(x = "Estimate (95% CI; fine line: 95% prediction interval)",
       y = "Life-history stage",
       caption = "Matched endpoint rows; alignment follows coding rules; absolute aggregation describes magnitude, not harm.") +
  grid_x + theme(legend.position = "top", panel.spacing = unit(5, "mm"),
                 plot.caption = element_text(hjust = 0, size = 5.8))

dif <- src("Fig4b_estimand_target_differences.csv") %>%
  filter(!is.na(FDR_q_value)) %>%
  mutate(Contrast = recode(TargetDifference,
                           `Aligned - directional` = "Aligned minus directional",
                           `Absolute - directional` = "Magnitude minus directional",
                           `Absolute - aligned` = "Magnitude minus aligned"),
         Contrast = factor(Contrast, levels = c("Aligned minus directional",
                                               "Magnitude minus directional", "Magnitude minus aligned")),
         Row = paste(stage_labels_1l[ObservedStage],
                     recode(StressorLabel, `Warming + acidification` = "Combined"), sep = " | "),
         Row = factor(Row, levels = rev(unique(Row))),
         qlab = ifelse(is.na(FDR_q_value), "NE",
                       paste0(ifelse(coalesce(Significance, "") == "", "", paste0(Significance, "  ")),
                              "q=", ifelse(FDR_q_value < .001, "<0.001", sprintf("%.3f", FDR_q_value)))),
         SignificantFDR = coalesce(as.logical(SignificantFDR), FALSE),
         Fontface = ifelse(SignificantFDR, "bold", "plain"))
xmax_d <- max(dif$CI_high, na.rm = TRUE); xmin_d <- min(dif$CI_low, na.rm = TRUE)
support_counts <- dif %>% group_by(Contrast) %>% summarise(n=n(),sig=sum(SignificantFDR),.groups="drop")
contrast_labs <- setNames(paste0(as.character(support_counts$Contrast),"\n",support_counts$sig,"/",support_counts$n," significant"),as.character(support_counts$Contrast))
support_caption <- sprintf("%d / %d numerical contrasts supported; %d involve folding. * q<0.05, ** q<0.01, *** q<0.001.",sum(dif$SignificantFDR),nrow(dif),sum(dif$SignificantFDR & dif$TargetDifference!="Aligned - directional"))
p3b <- ggplot(dif, aes(Estimate, Row)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high, colour = SignificantFDR),
                orientation = "y", width = .10, linewidth = .44, na.rm = TRUE) +
  geom_point(aes(fill = SignificantFDR), shape = 21, size = 1.85, colour = "white", stroke = .24, na.rm = TRUE) +
  geom_text(aes(x = Inf, label = qlab, fontface = Fontface), hjust = 1.04, size = 1.65) +
  facet_grid(. ~ Contrast, labeller = as_labeller(contrast_labs)) +
  # Significance is a property of the paired contrast, not an estimand.
  # Therefore it uses a dedicated teal/grey scale rather than the orange
  # magnitude colour used in panel a.
  scale_colour_manual(values = c(`TRUE` = "#6F63A8", `FALSE` = "#978DC6"), guide = "none") +
  scale_fill_manual(values = c(`TRUE` = "#6F63A8", `FALSE` = "#C3BDDF"), guide = "none") +
  scale_discrete_identity(aesthetics = "fontface", guide = "none") +
  scale_x_continuous(limits = c(xmin_d - .04 * (xmax_d - xmin_d), xmax_d + .30 * (xmax_d - xmin_d))) +
  labs(x = "Mean paired difference (95% bootstrap CI)", y = NULL,
       caption = support_caption) +
  grid_x + theme(panel.spacing = unit(4, "mm"), plot.caption = element_text(hjust = 0, size = 6.1))
# Dedicated significance panel: every estimable paired contrast is shown as a
# q-value cell. This makes the inferential result visible without reading the
# confidence intervals or the facet headers in panel b.
sig <- dif %>%
  mutate(
    StageDriver = factor(paste(stage_labels_1l[ObservedStage],
      recode(StressorLabel, `Warming + acidification` = "Combined"), sep = " | "),
      levels = rev(unique(paste(stage_labels_1l[ObservedStage],
        recode(StressorLabel, `Warming + acidification` = "Combined"), sep = " | ")))),
    q_label = ifelse(FDR_q_value < .001, "***",
      ifelse(FDR_q_value < .01, "**", ifelse(FDR_q_value < .05, "*", "ns"))),
    q_plot = -log10(pmax(FDR_q_value, 1e-4)),
    ContrastShort = recode(as.character(Contrast),
      `Aligned minus directional` = "Aligned - directional",
      `Magnitude minus directional` = "Magnitude - directional",
      `Magnitude minus aligned` = "Magnitude - aligned"),
    ContrastShort = factor(ContrastShort, levels = c("Aligned - directional",
      "Magnitude - directional", "Magnitude - aligned"))
  )
p3c <- ggplot(sig, aes(q_plot, StageDriver, colour = SignificantFDR)) +
  geom_vline(xintercept = -log10(.05), linetype = "dashed", linewidth = .28,
             colour = "#978DC6") +
  geom_segment(aes(x = 0, xend = q_plot, yend = StageDriver),
               linewidth = .30, colour = "#EAE7F3") +
  geom_point(aes(shape = SignificantFDR), size = 2.0, stroke = .25) +
  geom_text(aes(label = q_label), hjust = -0.35, size = 1.85,
            fontface = ifelse(sig$SignificantFDR, "bold", "plain"),
            colour = "#3B335F") +
  facet_grid(. ~ ContrastShort, scales = "free_x", space = "free_x") +
  scale_colour_manual(values = c(`TRUE` = "#6F63A8", `FALSE` = "#978DC6"), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  scale_x_continuous(limits = c(0, max(sig$q_plot, na.rm = TRUE) + .55),
                     breaks = c(0, -log10(.05), -log10(.01), -log10(.001)),
                     labels = c("0", "1.30", "2", "3"),
                     expand = expansion(mult = c(0, .02))) +
  labs(x = "−log10(q)", y = NULL,
       subtitle = support_caption) +
  theme_publication(base_size = 7) +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 5.2),
        strip.text = element_text(size = 5.3, face = "bold"),
        strip.background = element_blank(), legend.position = "none",
        plot.subtitle = element_text(size = 6, colour = col("grey_dark")))
sig <- sig %>% mutate(CellLabel=paste0(sprintf("Difference = %.3f",Estimate),"\n",
    ifelse(FDR_q_value<.001,"q < 0.001",sprintf("q = %.3f",FDR_q_value)),"  ",q_label),
    Support=factor(ifelse(SignificantFDR,"BH q < 0.05","Not significant"),levels=c("BH q < 0.05","Not significant")))
p3c <- ggplot(sig,aes(ContrastShort,StageDriver))+
  geom_tile(aes(fill=Support),width=.96,height=.91,colour="white",linewidth=.2)+
  geom_text(aes(label=CellLabel,colour=Support),size=2.4,lineheight=1.05)+
  scale_fill_manual(values=c("BH q < 0.05"="#6F63A8","Not significant"="#F4F2F9"),name=NULL)+
  scale_colour_manual(values=c("BH q < 0.05"="white","Not significant"="#4A417A"),guide="none")+
  scale_x_discrete(position="top",labels=setNames(unname(contrast_labs),gsub(" minus "," - ",names(contrast_labs))))+
  labs(x=NULL,y=NULL,caption=paste0(support_caption,"\nDifferences compare numerical targets, not biological harm."))+
  theme_publication(base_size=8)+theme(axis.line=element_blank(),axis.ticks=element_blank(),
    panel.grid=element_blank(),legend.position="bottom",axis.text.x=element_text(face="bold",size=7),
    axis.text.y=element_text(size=7),plot.caption=element_text(size=6.5,hjust=0))
write_csv_safe(sig,"data/source_data/Fig3c_direct_significance_matrix.csv")
fig3 <- p3a / p3b / p3c + plot_layout(heights = c(.8, 1.2, 1.3)) + plot_annotation(tag_levels = "a")
save_figure(fig3, "figures/main/Fig3_directional_and_absolute_stage_responses", 190, 255, 600)

# Figure 4: level-specific ecological forests and an information-rich moderator
# heatmap. Non-estimable groups are stated in the caption, not piled onto axes.
class_d <- src("Fig3c_taxonomic_class_subgroups.csv") %>%
  filter(Rho == .8, is.finite(Estimate)) %>% mutate(Group = Class)
guild_d <- src("Fig3d_ecological_guild_subgroups.csv") %>%
  filter(Rho == .8, is.finite(Estimate)) %>% mutate(Group = EcologicalGuild)
subgroup_plot <- function(dd, ylab) {
  dd <- dd %>% mutate(Group = factor(Group, levels = rev(sort(unique(Group)))),
                      Count = sprintf("s=%d; k=%d", StudyCount, EffectSizeCount),
                      StressorLabel = factor(StressorLabel, levels = stressor_levels))
  lim <- range(c(dd$PI_low, dd$PI_high, dd$CI_low, dd$CI_high, 0), na.rm = TRUE)
  span <- diff(lim)
  ggplot(dd, aes(Estimate, Group, colour = StressorLabel)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
    geom_errorbar(aes(xmin = PI_low, xmax = PI_high), orientation = "y", width = 0,
                  linewidth = .15, colour = "#C3BDDF", na.rm = TRUE) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .12,
                  linewidth = .44, na.rm = TRUE) +
    geom_point(size = 1.8) +
    geom_text(aes(x = Inf, label = Count), hjust = 1.04,
              colour = oi["black"], size = 1.55) +
    facet_grid(. ~ StressorLabel, labeller = as_labeller(c(
      "Acidification" = "Acidification", "Warming" = "Warming",
      "Warming + acidification" = "Combined"))) +
    scale_colour_manual(values = driver_cols, guide = "none") +
    scale_x_continuous(limits = c(lim[1] - .03 * span, lim[2] + .18 * span)) +
    labs(x = "Directional lnRR (95% CI; fine line: 95% prediction interval)", y = ylab) +
    grid_x + theme(panel.spacing = unit(4, "mm"), axis.text.y = element_text(size = 6.2))
}
p4a <- subgroup_plot(class_d, "Taxonomic class")
p4b <- subgroup_plot(guild_d, "Ecological guild")
om <- src("Fig3e_moderator_omnibus_tests.csv") %>%
  mutate(Moderator = factor(Moderator, levels = rev(c("Phylum", "Taxonomic class", "Ecological guild",
                                                     "Endpoint pathway", "Life-history stage"))),
         StressorLabel = factor(StressorLabel, levels = stressor_levels),
         score = -log10(pmax(FDR_q_value, 1e-7)),
         label = paste0(ifelse(coalesce(Significance, "") == "", "ns", Significance), "\nq=",
                        ifelse(FDR_q_value < .001, "<0.001", sprintf("%.3f", FDR_q_value)),
                        "\n", EligibleLevelCount, " levels; ", StudyCount, " studies"),
         dark = score > 1.8,
         SignificantFDR = coalesce(as.logical(SignificantFDR), FALSE),
         Fontface = ifelse(SignificantFDR, "bold", "plain"))
p4c <- ggplot(om, aes(StressorLabel, Moderator, fill = score)) +
  geom_tile(colour = "white", linewidth = .24) +
  geom_text(aes(label = label, colour = dark, fontface = Fontface), size = 1.75, lineheight = .88) +
  scale_fill_gradientn(colours = c("#F4F2F9", "#A9CBE6", "#0072B2", "#0B4F7F"),
                       name = expression(-log[10](q))) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#3B335F"), guide = "none") +
  scale_discrete_identity(aesthetics = "fontface", guide = "none") +
  scale_x_discrete(labels = c("Acidification", "Warming", "Combined exposure")) +
  labs(x = NULL, y = "Ecological moderator") +
  theme(axis.text.x = element_text(angle = 0), legend.position = "right")
fig4 <- p4a / (p4b | p4c) + plot_layout(heights = c(1.35, 1.0), widths = c(1.05, 1.0)) +
  plot_annotation(tag_levels = "a")
save_figure(fig4, "figures/legacy_exports_20260910/Fig2_directional_subgroup_audit", 190, 215, 1000)

# Figure 5: sensitivity is shown as change from rho=0, development-only effects
# as a conventional forest, contrasts as a forest, and precision as visible data.
rho <- src("Fig5a_RVE_rho_sensitivity.csv") %>%
  group_by(Estimand, ObservedStage) %>% mutate(Delta = Estimate - Estimate[Rho == 0]) %>% ungroup() %>%
  filter(Rho > 0) %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         DeltaMicro = Delta,
         RhoLabel = factor(paste0("rho = ", Rho), levels = c("rho = 0.5", "rho = 0.8")),
         Estimand = recode(Estimand, `Raw directional` = "Directional",
                           `Aligned performance` = "Aligned performance"))
p5a <- ggplot(rho, aes(DeltaMicro, ObservedStage, colour = RhoLabel, shape = RhoLabel)) +
  geom_vline(xintercept = 0, linewidth = .20, colour = oi["grey"]) +
  geom_segment(aes(x = 0, xend = DeltaMicro, yend = ObservedStage), linewidth = .24, colour = "#C3BDDF") +
  geom_point(size = 1.95) + facet_grid(. ~ Estimand, scales = "free_x") +
  scale_colour_manual(values = c("rho = 0.5" = "#E3864A", "rho = 0.8" = "#6F63A8"), name = NULL) +
  scale_shape_manual(values = c("rho = 0.5" = 17, "rho = 0.8" = 15), name = NULL) +
  scale_y_discrete(labels = stage_labels_1l) +
  labs(x = "Change in coefficient from rho = 0", y = "Life-history stage") +
  grid_x + theme(legend.position = "top")

dev <- src("Fig5b_development_by_stressor_directional.csv") %>%
  mutate(ObservedStage = factor(ObservedStage, levels = rev(stage_levels)),
         StressorLabel = factor(StressorLabel, levels = stressor_levels))
p5b <- ggplot(dev, aes(Estimate, ObservedStage, colour = StressorLabel)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .10, linewidth = .46) +
  geom_point(size = 1.9) + facet_grid(. ~ StressorLabel) +
  scale_colour_manual(values = driver_cols, guide = "none") +
  scale_y_discrete(labels = stage_labels_1l) +
  labs(x = "Development-endpoint directional lnRR (95% CI)", y = "Life-history stage") + grid_x +
  theme(panel.spacing.x = unit(5, "mm"))

con <- src("Fig5d_RVE_stage_contrasts.csv") %>%
  mutate(Estimand = recode(Estimand, `Raw directional` = "Directional",
                           `Aligned performance` = "Aligned performance"),
         ContrastLabel = factor(ContrastLabel, levels = rev(unique(ContrastLabel))))
p5c <- ggplot(con, aes(Contrast, ContrastLabel, colour = Estimand)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = .20, colour = oi["grey"]) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y", width = .12,
                linewidth = .44, position = position_dodge(width = .34)) +
  geom_point(position = position_dodge(width = .34), size = 1.85) +
  scale_colour_manual(values = c("Directional" = "#0072B2", "Aligned performance" = "#6F63A8"), name = NULL) +
  labs(x = "Pairwise stage contrast in lnRR (95% CI)", y = NULL) + grid_x +
  theme(legend.position = "top", axis.text.y = element_text(size = 5.8))

fun <- src("Fig5c_signed_funnel_by_stressor_source_data.csv") %>%
  filter(is.finite(yi), is.finite(SE))
ref <- weighted.mean(fun$yi, 1 / fun$vi, na.rm = TRUE)
se_seq <- seq(0, max(fun$SE), length.out = 100)
funnel_lines <- tibble(SE = se_seq, lo = ref - 1.96 * se_seq, hi = ref + 1.96 * se_seq)
p5d <- ggplot(fun, aes(yi, SE, fill = StressorLabel)) +
  geom_ribbon(data = funnel_lines, aes(xmin = lo, xmax = hi, y = SE), inherit.aes = FALSE,
              fill = "#EAE7F3", colour = NA) +
  geom_line(data = funnel_lines, aes(lo, SE), inherit.aes = FALSE, linetype = "dashed", linewidth = .22, colour = oi["grey"]) +
  geom_line(data = funnel_lines, aes(hi, SE), inherit.aes = FALSE, linetype = "dashed", linewidth = .22, colour = oi["grey"]) +
  geom_vline(xintercept = ref, linewidth = .28, colour = oi["black"]) +
  geom_point(shape = 21, size = 1.65, alpha = .82, colour = oi["black"], stroke = .16) +
  scale_fill_manual(values = driver_cols, name = NULL) +
  scale_y_reverse() + labs(x = "Study-stage directional lnRR", y = "Standard error") +
  theme(legend.position = "top")
fig5 <- p5a / p5b / (p5c | p5d) + plot_layout(heights = c(.72, .9, 1.15)) +
  plot_annotation(tag_levels = "a")
save_figure(fig5, "figures/main/Fig5_robustness_and_inference_limits", 190, 205, 1000)

writeLines(c(
  "Final main figures rebuilt from archived source data.",
  "Figure 1 includes OpenTree topology with explicit vertical connectors, a keyword co-occurrence network, and descriptive keyword activity intervals.",
  "Figure 3 separates the three estimands vertically; current paired-bootstrap counts and BH-FDR results are recorded in Fig4b_estimand_target_differences.csv.",
  "Figure 4 excludes non-estimable levels from crowded forest axes and reports them in the caption.",
  "Figure 5 funnel points use filled driver colours with dark outlines."
), file.path(root_dir, "analysis_logs", "main_figure_rebuild_20260909.txt"))
