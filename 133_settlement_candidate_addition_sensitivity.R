# Settlement/metamorphosis: source-integrity sensitivity (review rounds 16, 18 and 19).
# Review round 19: the six StudyID 23 settlement records (rows 1077, 1083, 1086,
# 1095-1097) are held out of the main dataset by R/136, because their extracted
# SEs are smaller than the binomial SE implied by the means and n = 135 larvae, so
# the variances cannot be verified. The main analysis has 2,385 effects (35
# settlement effects, 9 studies). This script restores the six records from
# data/source_data/Settlement_variance_unverifiable_StudyID23.csv as a
# sensitivity analysis, with and without row 1083 (warming-only cell sharing the
# acidification-only cell's reported mean). Bates et al. (2025) is a qualitative
# record only (review round 18: means and SDs do not match).
# Settlement rows use the alternative rule (higher = beneficial) for the
# endpoint-aligned definition, and the recorded lnRR for the directional definition.
source('R/00_setup.R')
suppressPackageStartupMessages({library(robumeta)})
d <- read_csv(guess_max=100000, 'data/processed/analysis_ready_model_dataset_deduplicated.csv',show_col_types=FALSE)
cc <- read_csv(guess_max=100000, 'data/processed/ER_endpoint_direction_row_coding.csv',show_col_types=FALSE)
stopifnot(nrow(d)==nrow(cc),identical(d$StudyID,cc$StudyID))
d$Aligned <- cc$aligned_lnRR_strict
d <- d %>% filter(DevelopmentalModeGroup=='With metamorphic stage') %>%
  select(LatestModelRowID,StudyID,ObservedStage,StressorLabel,lnRR,Aligned,vi_model)
held <- read_csv(guess_max=100000,'data/source_data/Settlement_variance_unverifiable_StudyID23.csv',show_col_types=FALSE) %>%
  filter(DevelopmentalModeGroup=='With metamorphic stage') %>%
  transmute(LatestModelRowID,StudyID,ObservedStage,StressorLabel,lnRR,Aligned=NA_real_,vi_model)
stopifnot(nrow(held)==6, all(held$StudyID==23), !any(held$LatestModelRowID %in% d$LatestModelRowID))
restored <- bind_rows(d,held)

fit_stage <- function(x,label,def){
  x$yi <- if (def=='Directional') x$lnRR else ifelse(x$ObservedStage=='SETTLEMENT/METAMORPHOSIS', x$lnRR, x$Aligned)
  stages <- x %>% filter(is.finite(yi)) %>% distinct(StudyID,ObservedStage) %>% count(ObservedStage) %>%
    filter(n>=3) %>% pull(ObservedStage)
  x <- x %>% filter(is.finite(yi),ObservedStage %in% stages) %>%
    mutate(ObservedStage=droplevels(factor(ObservedStage,levels=stage_levels)))
  f <- robu(yi~0+ObservedStage,data=x,studynum=StudyID,var.eff.size=vi_model,modelweights='CORR',rho=.8,small=TRUE)
  cnt <- x %>% group_by(ObservedStage) %>% summarise(m=n_distinct(StudyID),k=n(),.groups='drop')
  as_tibble(f$reg_table) %>% transmute(Specification=label,Definition=def,Stage=levels(x$ObservedStage),
    Estimate=b.r,SE=SE,df=dfs,CI_low=CI.L,CI_high=CI.U,P_value=prob,
    StageStudyCount=cnt$m,StageEffectSizeCount=cnt$k)
}
specs <- list(
  'Main dataset (35 effects, 9 studies)' = d,
  'StudyID 23 settlement records restored' = restored,
  'StudyID 23 restored without row 1083' = restored %>% filter(LatestModelRowID!=1083))
out <- bind_rows(lapply(names(specs), function(s) bind_rows(
  fit_stage(specs[[s]],s,'Directional'), fit_stage(specs[[s]],s,'Endpoint-aligned (alternative settlement rule)'))))
write_csv_safe(out,'data/source_data/Settlement_candidate_addition_sensitivity.csv')
print(out %>% filter(Stage=='SETTLEMENT/METAMORPHOSIS') %>% select(Specification,Definition,Estimate,CI_low,CI_high,P_value,StageStudyCount,StageEffectSizeCount), width=200)
