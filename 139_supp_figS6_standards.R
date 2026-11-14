# Round 21: Supplementary Fig. S6 re-rendered to journal conventions (same archived estimates as R/98).
# Changes from R/98: columns ordered Directional | Endpoint-aligned (global definition order and name),
# CO2 written with a true subscript (plotmath; the Arial device font lacks U+2082), all text black.
source('R/93_submission_visual_final.R')
ex <- parse('R/97_annotated_evidence_forests.R')
keep <- c('src', 'proc', 'number', 'qq', 'forest_info', 'display_audit', 'pidx')
for (e in ex) if (is.call(e) && as.character(e[[1]]) %in% c('<-', '=') && as.character(e[[2]]) %in% keep) eval(e, globalenv())
rows <- c('Temperature increment', 'Control temperature', 'CO2 increment')
dose <- src('Boundary_dose_meta_regression.csv') %>% filter(!grepl('^Stage', Term)) %>%
  mutate(Row = recode(Term, DoseT = 'Temperature increment', DoseCO2 = 'CO2 increment', BaselineT = 'Control temperature'),
         Series = recode(Driver, `Warming + acidification` = 'Combined'),
         Facet = factor(recode(Target, Aligned = 'Endpoint-aligned'), levels = c('Directional', 'Endpoint-aligned')),
         Info = paste0(number(Estimate), '; s=', m, '; k=', k, '\n', qq(FDR_q_value), '; df=', sprintf('%.1f', df)))
pd <- forest_info(dose, rows = rows, palette = setNames(unname(driver_pal[stressor_levels]), c('Acidification', 'Warming', 'Combined')),
                  title = 'Adjusted slope', info_width = 1.2)
ylab <- c(expression('Temperature increment'), 'Control temperature', expression(CO[2] ~ 'increment'))
pd <- suppressMessages(pd + scale_y_continuous(breaks = seq_along(rows), labels = ylab, limits = c(.5, length(rows) + .5), expand = c(0, 0)))
pd <- pd + theme(strip.text = element_text(size = 7.5, face = 'bold', colour = 'black'),
                 axis.text = element_text(colour = 'black'), axis.title = element_text(colour = 'black'),
                 legend.text = element_text(colour = 'black'), plot.caption = element_text(colour = 'black'))
out <- Sys.getenv('FIGS6_OUT', 'figures/extended/SupplementaryFigS6_exposure_adjusted_meta_regression')
save_figure(pd + labs(caption = 'Labels: coefficient, studies (s), effects (k), BH q and small-sample df.\nSlopes with df < 4 are unstable. No observed-cell scatter is superimposed on adjusted model coefficients.'),
            out, 190, 160, 600)
