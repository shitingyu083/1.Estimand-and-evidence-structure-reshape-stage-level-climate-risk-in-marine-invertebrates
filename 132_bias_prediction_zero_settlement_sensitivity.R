# Three sensitivity summaries for the primary subset (taxa with a metamorphic stage):
# (1) PET/PEESE-type small-study adjustment of each directional stage mean, fitted
#     with the primary RVE settings (CORR weights, rho = 0.8, small-sample correction);
# (2) bounds for the warming-only settlement arm of StudyID 74 (Pitts et al., 2020),
#     omitted because 0% settlement after total larval mortality gives an undefined
#     lnRR. Control and treatment means are not stored for this study, so the arm is
#     imputed at two bounds rather than by continuity correction: the settlement lnRR
#     of the same study's combined-exposure arm (-1.120, also the most severe settlement
#     lnRR in the primary subset), and a hypothetical stress value of ln(0.01) = -4.605
#     (settlement at 1% of control). Both use the largest sampling variance in the study;
# (3) 95% prediction intervals of the companion REML stage models.
source('R/00_setup.R')
suppressPackageStartupMessages({library(robumeta)})
d <- read_csv(guess_max=100000, 'data/processed/analysis_ready_model_dataset_deduplicated.csv',show_col_types=FALSE)
cc <- read_csv(guess_max=100000, 'data/processed/ER_endpoint_direction_row_coding.csv',show_col_types=FALSE)
stopifnot(nrow(d)==nrow(cc),identical(d$StudyID,cc$StudyID))
d$Aligned <- cc$aligned_lnRR_strict
d <- d %>% filter(DevelopmentalModeGroup=='With metamorphic stage', is.finite(vi_model), vi_model>0,
                  !is.na(StudyID), !is.na(ObservedStage))
is_set <- d$ObservedStage=='SETTLEMENT/METAMORPHOSIS'

fit_stage <- function(x,label) {
  x <- x %>% filter(is.finite(yi))
  stages <- x %>% distinct(StudyID,ObservedStage) %>% count(ObservedStage) %>% filter(n>=3) %>% pull(ObservedStage)
  x <- x %>% filter(ObservedStage %in% stages) %>% mutate(ObservedStage=droplevels(factor(ObservedStage,levels=stage_levels)))
  f <- robu(yi~0+ObservedStage,data=x,studynum=StudyID,var.eff.size=vi_model,modelweights='CORR',rho=.8,small=TRUE)
  as_tibble(f$reg_table) %>% transmute(Analysis=label,Stage=levels(x$ObservedStage),Term='Stage mean',
    Estimate=b.r,SE=SE,df=dfs,CI_low=CI.L,CI_high=CI.U,P_value=prob,
    StudyCount=vapply(levels(x$ObservedStage),function(s) n_distinct(x$StudyID[x$ObservedStage==s]),integer(1)),
    EffectSizeCount=vapply(levels(x$ObservedStage),function(s) sum(x$ObservedStage==s),integer(1)))
}

# (1) Small-study adjustment, one RVE meta-regression per stage; the intercept is the
# mean predicted at SE = 0 (PET) or variance = 0 (PEESE).
dir <- d %>% filter(is.finite(lnRR))
pet <- bind_rows(lapply(stage_levels, function(s) {
  x <- dir %>% filter(ObservedStage==s) %>% mutate(sei=sqrt(vi_model))
  if (n_distinct(x$StudyID) < 10) return(NULL)
  bind_rows(lapply(c(PET='sei',PEESE='vi_model'), function(m) {
    f <- robu(stats::as.formula(paste('lnRR ~',m)),data=x,studynum=StudyID,var.eff.size=vi_model,
              modelweights='CORR',rho=.8,small=TRUE)
    rt <- as_tibble(f$reg_table)
    tibble(Analysis=paste0('Directional, ',ifelse(m=='sei','PET (moderator SE)','PEESE (moderator variance)')),
           Stage=s,Term=c('Adjusted mean (intercept)','Slope'),Estimate=rt$b.r,SE=rt$SE,df=rt$dfs,
           CI_low=rt$CI.L,CI_high=rt$CI.U,P_value=rt$prob,StudyCount=n_distinct(x$StudyID),EffectSizeCount=nrow(x))
  }))
}))

# (2) StudyID 74 warming-only settlement arm.
template <- d %>% filter(StudyID==74, is_set) %>% slice_max(vi_model, n=1, with_ties=FALSE)
bounds <- c(`same-study combined exposure` = d$lnRR[d$StudyID==74 & is_set & d$StressorLabel=='Warming + acidification'],
            `hypothetical 1% of control settlement` = log(0.01))
zero <- bind_rows(lapply(names(bounds), function(b) {
  add <- template %>% mutate(StressorLabel='Warming', lnRR=bounds[[b]], Aligned=NA_real_,
                             LatestModelRowID=NA_real_)
  dd <- bind_rows(d, add); s <- dd$ObservedStage=='SETTLEMENT/METAMORPHOSIS'
  bind_rows(
    fit_stage(dd %>% mutate(yi=lnRR), paste0('Directional; StudyID 74 warming arm imputed at ',round(bounds[[b]],3),' (',b,')')),
    fit_stage(dd %>% mutate(yi=ifelse(s,lnRR,Aligned)), paste0('Endpoint-aligned alternative rule; StudyID 74 warming arm imputed at ',round(bounds[[b]],3),' (',b,')'))
  ) %>% filter(Stage=='SETTLEMENT/METAMORPHOSIS')
}))
ref <- bind_rows(fit_stage(d %>% mutate(yi=lnRR),'Directional (primary, arm omitted)'),
                 fit_stage(d %>% mutate(yi=ifelse(is_set,lnRR,Aligned)),'Endpoint-aligned alternative rule (arm omitted)'))

# (3) REML prediction intervals (archived companion models).
pi <- read_csv('data/processed/main_results_REML.csv',show_col_types=FALSE) %>%
  filter(DevelopmentalModeGroup=='With metamorphic stage', StressorLabel=='All stressors') %>%
  transmute(Analysis=paste0('Study-aggregated REML, ',Estimand,' (95% prediction interval in PI columns)'),
            Stage=ObservedStage,Term='Stage mean',Estimate,SE,df=NA_real_,CI_low,CI_high,P_value,
            StudyCount,EffectSizeCount,PI_low,PI_high,Tau2)

out <- bind_rows(ref, pet, zero, pi) %>% mutate(across(where(is.double), ~round(.x, 4)))
write_csv_safe(out,'data/source_data/Review15_bias_PI_zero_sensitivity.csv')
print(as.data.frame(out %>% select(Analysis,Stage,Term,Estimate,CI_low,CI_high,P_value,StudyCount,EffectSizeCount,PI_low,PI_high)))
