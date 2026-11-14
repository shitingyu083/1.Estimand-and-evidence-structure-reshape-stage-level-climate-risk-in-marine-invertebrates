# Round 21: Supplementary Fig. S12 re-rendered to journal conventions (same archived diagnostics as R/98).
# Changes from R/98: series named 'Endpoint |lnRR|' in the script (replaces the round-13 PDF text edit),
# directional listed first, size-legend keys neutral grey instead of an unused purple, legible
# colour-legend keys, duplicate legend in panel a removed, all text black and the footnote at 6.5 pt.
source('R/93_submission_visual_final.R')
ex <- parse('R/97_annotated_evidence_forests.R')
keep <- c('src', 'proc', 'number', 'qq', 'forest_info', 'display_audit', 'pidx')
for (e in ex) if (is.call(e) && as.character(e[[1]]) %in% c('<-', '=') && as.character(e[[2]]) %in% keep) eval(e, globalenv())
lev <- c('Directional lnRR', 'Endpoint |lnRR|')
rn <- function(z) z %>% mutate(Estimand = factor(recode(Estimand, `Absolute |lnRR|` = 'Endpoint |lnRR|'), levels = lev)) %>% arrange(Estimand)
eg <- rn(src('SupplementaryFigS4a_egger_diagnostics.csv'))
ht <- rn(src('SupplementaryFigS4bcd_heterogeneity_diagnostics.csv'))
pal <- c('Directional lnRR' = '#0072B2', 'Endpoint |lnRR|' = '#D55E00')
blk <- theme(axis.text = element_text(colour = 'black'), axis.title = element_text(colour = 'black'),
             legend.text = element_text(colour = 'black'), legend.title = element_text(colour = 'black'),
             strip.text = element_text(colour = 'black'))
p1 <- ggplot(eg, aes(Egger_z, factor(ObservedStage, levels = rev(stage_levels)), colour = Estimand)) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 2, linewidth = .2, colour = '#978DC6') + geom_point(position = position_dodge(.5), size = 2) +
  scale_colour_manual(values = pal, breaks = lev) + scale_y_discrete(labels = stage_labels_1l) +
  labs(x = 'Egger z (exploratory diagnostic)', y = NULL) + theme_publication(8) + blk + theme(legend.position = 'none')
p2 <- ggplot(ht, aes(I2_percent, Tau2, colour = Estimand, size = StudyCount)) + geom_point() +
  scale_colour_manual(values = pal, breaks = lev) + labs(x = expression(italic(I)^2 ~ '(%)'), y = expression(tau^2), size = 'Studies') +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 2.2)),
         size = guide_legend(order = 2, override.aes = list(colour = 'grey45'))) + theme_publication(8) + blk
h <- ht %>% mutate(Row = ObservedStage, Series = as.character(Estimand), Facet = 'Stage summaries', FDR_q_value = NA_real_,
                   Info = paste0(number(Estimate), '; s=', StudyCount, '; k=', EffectSizeCount))
p3 <- forest_info(h, rows = rev(stage_levels), palette = pal, title = 'Model estimate', info_width = .9) + blk +
  theme(strip.text = element_text(size = 7.5, face = 'bold', colour = 'black'))
p4 <- ggplot(ht %>% distinct(ObservedStage, StudyCount, EffectSizeCount), aes(StudyCount, EffectSizeCount)) + geom_point(size = 2, colour = '#0072B2') +
  ggrepel::geom_text_repel(aes(label = stage_labels_1l[ObservedStage]), colour = 'black', size = 2.3, seed = 19) +
  labs(x = 'Studies (shared by both summaries)', y = 'Effects') + theme_publication(8) + blk
out <- Sys.getenv('FIGS12_OUT', 'figures/extended/SupplementaryFigS12_small_study_and_heterogeneity')
save_figure((p1 | p2) / p3 / p4 + plot_layout(heights = c(1, 1.3, 1)) +
              plot_annotation(tag_levels = 'a', caption = 'Summary labels: estimate, studies (s), effects (k). Folded-target diagnostics are descriptive. No new tests.',
                              theme = theme(plot.caption = element_text(size = 6.5, colour = 'black'))),
            out, 190, 270, 600)
