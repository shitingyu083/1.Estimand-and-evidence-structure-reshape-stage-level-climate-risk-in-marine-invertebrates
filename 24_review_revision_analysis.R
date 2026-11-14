# Reviewer-requested analyses, 12 September 2026. Exploratory additions, not preregistered.
source('R/00_setup.R')
suppressPackageStartupMessages({library(robumeta); library(clubSandwich)})
rv <- function(path) read_csv(guess_max=100000, file.path(root_dir,path),show_col_types=FALSE)
d <- rv('data/processed/analysis_ready_model_dataset_deduplicated.csv')
coding <- rv('data/processed/ER_endpoint_direction_row_coding.csv')
stopifnot(nrow(d)==nrow(coding), identical(d$StudyID,coding$StudyID),
          identical(d$Metric,coding$Metric), isTRUE(all.equal(d$lnRR,coding$lnRR)))
d$aligned <- coding$aligned_lnRR_strict
d$DirectionRule <- coding$EndpointDirectionClassStrict
direction_sources <- rv('data/source_data/Boundary_endpoint_source_rules.csv')
d <- d %>% left_join(direction_sources %>% select(LatestModelRowID,DirectionEvidence,PublishedConsensus),by='LatestModelRowID')

# Preserve every effect and reference link, including missing source locators.
provenance <- d %>% select(LatestModelRowID, RowKey, StudyID, REF, PaperYear,
  PaperAuthor, Journal, DatasetVersion, DatasetDate, UpdateLayer, SourceDataset,
  SourceRow, OriginalSourceRows, SourceFile, ExtractionControl, ExtractionTreatment,
  ExtractionNotes, SpeciesID, ObservedStage, StressorLabel, Metric, lnRR, vi_model) %>%
  mutate(DuplicateRowKey=duplicated(RowKey)|duplicated(RowKey,fromLast=TRUE),
    LocatorAvailable=(!is.na(SourceRow)) | (!is.na(OriginalSourceRows)&OriginalSourceRows!=''),
    ExperimentIDStatus='Not consistently recorded; StudyID is the dependence cluster')
write_csv_safe(provenance,'data/source_data/Review_effect_provenance.csv')
crosswalk <- d %>% group_by(StudyID,REF,PaperYear,PaperAuthor,Journal) %>% summarise(
  Effects=n(), PrimaryEffects=sum(DevelopmentalModeGroup=='With metamorphic stage'),
  Stages=n_distinct(ObservedStage), SourceDatasets=paste(sort(unique(na.omit(SourceDataset))),collapse='; '),
  .groups='drop')
write_csv_safe(crosswalk,'data/source_data/Review_reference_StudyID_crosswalk.csv')
raw_check <- d %>% filter(is.finite(ControlMean),is.finite(TreatmentMean)) %>%
  mutate(RecalculatedLnRR=log(TreatmentMean/ControlMean),
    RecalculatedVariance=TreatmentSD^2/(TreatmentN*TreatmentMean^2)+ControlSD^2/(ControlN*ControlMean^2),
    LnRRDifference=RecalculatedLnRR-lnRR,UnflooredVarianceDifference=RecalculatedVariance-Variance,
    ModelVarianceDifference=RecalculatedVariance-vi_model) %>%
  select(LatestModelRowID,StudyID,Metric,ControlMean,ControlSD,ControlN,TreatmentMean,TreatmentSD,TreatmentN,
    lnRR,Variance,vi_model,RecalculatedLnRR,RecalculatedVariance,LnRRDifference,UnflooredVarianceDifference,ModelVarianceDifference)
stopifnot(max(abs(raw_check$LnRRDifference))<1e-10,max(abs(raw_check$UnflooredVarianceDifference))<1e-10)
write_csv_safe(raw_check,'data/source_data/Review_added_effect_recalculation.csv')
scope_counts <- function(x,label) tibble(Scope=label,k=nrow(x),m=n_distinct(x$StudyID),
  StudyStageCells=nrow(distinct(x,StudyID,ObservedStage)),
  StudyDriverCells=nrow(distinct(x,StudyID,StressorLabel)),
  StudyStageDriverCells=nrow(distinct(x,StudyID,ObservedStage,StressorLabel)))
primary <- d %>% filter(DevelopmentalModeGroup=='With metamorphic stage')
counts <- bind_rows(scope_counts(d,'Full database'),scope_counts(primary,'Primary metamorphic taxa'),
  scope_counts(d %>% filter(is.finite(aligned)),'Full database: endpoint-aligned'),
  scope_counts(primary %>% filter(is.finite(aligned)),'Primary: endpoint-aligned'))
write_csv_safe(counts,'data/source_data/Review_analysis_population_counts.csv')
write_csv_safe(primary %>% group_by(ObservedStage) %>% summarise(k=n(),m=n_distinct(StudyID),
  Aligned_k=sum(is.finite(aligned)),Aligned_m=n_distinct(StudyID[is.finite(aligned)]),.groups='drop'),
  'data/source_data/Review_primary_stage_counts.csv')

# Row-specific decision rationale; confidence describes the rule, not independent validation.
et <- tolower(paste(d$Metric,d$TransformedMetric,sep=' | '))
d$DirectOutcome <- grepl('survival|viability|mortality|\\bdead\\b|\\bdeath\\b|hatching success|hatch success|settlement success|metamorph.*success|fertiliz.*success|fecundity|egg production|reproductive success',et)
d$DirectOutcome <- d$DirectOutcome & is.finite(d$aligned)
decision <- d %>% mutate(
    BiologicalRationale=DirectionEvidence,
  Confidence=case_when(DirectionRule=='context_dependent' ~ 'Excluded: ambiguous or unsupported represented endpoint',grepl('Published five',DirectionEvidence) ~ 'Inherited published expert consensus; mapping not independently recoded',TRUE ~ 'Explicit represented outcome; source transformation preserved'),
  ExcludedFromAligned=!is.finite(aligned),IndependentDuplicateCoding='Not performed') %>%
  select(LatestModelRowID,StudyID,SpeciesID,ObservedStage,StressorLabel,Category,Metric,TransformedMetric,
         DirectionRule,BiologicalRationale,Confidence,ExcludedFromAligned,DirectOutcome,IndependentDuplicateCoding)
write_csv_safe(decision,'data/source_data/Review_endpoint_direction_decisions.csv')
write_csv_safe(decision %>% group_by(Category,Metric,TransformedMetric,DirectionRule,BiologicalRationale,
  Confidence,ExcludedFromAligned,DirectOutcome) %>% summarise(k=n(),m=n_distinct(StudyID),.groups='drop'),
  'data/source_data/Review_endpoint_dictionary.csv')

# Direct-outcome restriction is a rule sensitivity, not a second human coder.
direct <- d %>% filter(DevelopmentalModeGroup=='With metamorphic stage',DirectOutcome)
eligible <- direct %>% distinct(StudyID,ObservedStage) %>% count(ObservedStage) %>% filter(n>=3) %>% pull(ObservedStage)
dd <- direct %>% filter(ObservedStage %in% eligible) %>% mutate(ObservedStage=factor(ObservedStage,levels=stage_levels[stage_levels %in% eligible]))
fit <- robumeta::robu(aligned ~ 0+ObservedStage,data=dd,studynum=StudyID,var.eff.size=vi_model,modelweights='CORR',rho=.8,small=TRUE)
direct_results <- as_tibble(fit$reg_table) %>% mutate(ObservedStage=levels(dd$ObservedStage),FDR_q_value=p.adjust(prob,'BH')) %>%
  left_join(dd %>% group_by(ObservedStage) %>% summarise(k=n(),m=n_distinct(StudyID),.groups='drop'),by='ObservedStage')
write_csv_safe(direct_results,'data/source_data/Review_direct_outcome_RVE.csv')

# Normal random-effects model fitted to signed effects. Its implied folded mean
# concerns latent absolute response, unlike an error-offset descriptive index.
foldmean <- function(mu,tau) if(tau<1e-12) abs(mu) else
  tau*sqrt(2/pi)*exp(-mu^2/(2*tau^2))+mu*(2*pnorm(mu/tau)-1)
signed_rows <- list(); trunc_rows <- list(); z<-1L; zt<-1L
for(scope in c('All eligible endpoints','Development only','Endpoint-aligned subset')) {
  sub <- primary
  if(scope=='Development only') sub<-filter(sub,Category=='Development')
  if(scope=='Endpoint-aligned subset') sub<-filter(sub,is.finite(aligned))
  for(rho in c(0,.5,.8)) {
    cells <- sub %>% group_by(StudyID,ObservedStage) %>% summarise(
      yi=sum(lnRR/vi_model)/sum(1/vi_model),
      vi=((1-rho)*sum(1/vi_model)+rho*sum(1/sqrt(vi_model))^2)/sum(1/vi_model)^2,
      k=n(),.groups='drop')
    for(st in stage_levels) {
      x<-filter(cells,ObservedStage==st); if(nrow(x)<3) next
      f<-metafor::rma.uni(yi=x$yi,vi=x$vi,method='REML',test='knha')
      mu<-as.numeric(f$b); tau<-sqrt(f$tau2)
      signed_rows[[z]]<-tibble(EvidenceSet=scope,Rho=rho,ObservedStage=st,k=sum(x$k),m=nrow(x),
        Estimate=mu,CI_low=f$ci.lb,CI_high=f$ci.ub,P_value=f$pval,Tau2=f$tau2,
        ImpliedLatentAbsoluteMean=foldmean(mu,tau),
        CenteredHeterogeneityAbsoluteMean=foldmean(0,tau),
        MeanShiftIncrement=foldmean(mu,tau)-foldmean(0,tau))
      z<-z+1L
      set.seed(241200+zt)
      raw<-mean(x$yi^2-x$vi)
      bs<-replicate(4000,{i<-sample.int(nrow(x),nrow(x),TRUE);mean(x$yi[i]^2-x$vi[i])})
      trunc_rows[[zt]]<-tibble(EvidenceSet=scope,Rho=rho,ObservedStage=st,k=sum(x$k),m=nrow(x),
        MeanSquaredMinusVariance=raw,RMS=sqrt(max(raw,0)),PointEstimateTruncated=raw<=0,
        CellNegativeContributions=sum(x$yi^2-x$vi<=0),BootstrapResamples=4000,
        BootstrapTruncated=sum(bs<=0),BootstrapTruncationFraction=mean(bs<=0),
        UntruncatedMomentCI_low=unname(quantile(bs,.025)),UntruncatedMomentCI_high=unname(quantile(bs,.975)))
      zt<-zt+1L
    }
  }
}
sr<-bind_rows(signed_rows) %>% group_by(EvidenceSet,Rho) %>% mutate(FDR_q_value=p.adjust(P_value,'BH')) %>% ungroup()
write_csv_safe(sr,'data/source_data/Review_signed_REML_distribution_sensitivity.csv')
write_csv_safe(bind_rows(trunc_rows),'data/source_data/Review_RMS_truncation_audit.csv')

# Carry CR2 results into the canonical Figure 5 source; retain all three rho fits.
cr<-rv('data/processed/ER_RVE_stage_contrasts.csv')
cp<-cr %>% filter(Rho==.8) %>% mutate(Estimand=if_else(grepl('Raw',Estimand),'Raw directional','Aligned performance'),
  ContrastLabel=paste(stage_labels_1l[ComparisonStage],'-',stage_labels_1l[ReferenceStage]),
  InferenceMethod='CR2, contrast-specific Satterthwaite df; BH within estimand and rho')
write_csv_safe(cp,'data/source_data/Fig5d_RVE_stage_contrasts.csv')
write_csv_safe(cp,'data/source_data/Fig5c_CR2_stage_contrasts.csv')

# New supplementary figure uses the original white-background plotting style.
p1<-ggplot(sr %>% filter(EvidenceSet=='All eligible endpoints'),
  aes(Estimate,factor(ObservedStage,levels=rev(stage_levels)),colour=factor(Rho)))+
  geom_vline(xintercept=0,linewidth=.2,linetype=2)+
  geom_errorbar(aes(xmin=CI_low,xmax=CI_high),orientation='y',width=0,linewidth=.35,position=position_dodge(.5))+
  geom_point(size=1.6,position=position_dodge(.5))+
  scale_y_discrete(labels=stage_labels_1l)+scale_colour_manual(values=c('#0072B2','#D55E00','#6F63A8'),name='Within-cell rho')+
  labs(x='Signed REML mean (95% HKSJ CI)',y=NULL)+theme_publication(base_size=8)+theme(panel.grid=element_blank(),legend.position='top')
p2<-ggplot(bind_rows(trunc_rows) %>% filter(EvidenceSet=='All eligible endpoints'),
  aes(factor(Rho),factor(ObservedStage,levels=rev(stage_levels)),fill=BootstrapTruncationFraction))+
  geom_tile(colour='white',linewidth=.2)+geom_text(aes(label=sprintf('%.1f%%',100*BootstrapTruncationFraction)),size=2.5)+
  scale_y_discrete(labels=stage_labels_1l)+scale_fill_gradient(low='#F4F2F9',high='#E3864A',limits=c(0,1),name='Fraction')+
  labs(x='Within-cell rho',y=NULL)+theme_publication(base_size=8)+theme(panel.grid=element_blank(),axis.line=element_blank(),axis.ticks=element_blank())
save_figure(p1/p2+plot_annotation(tag_levels='a'),'figures/extended/SupplementaryFigS5_signed_model_and_truncation',190,170,dpi=600)
writeLines(capture.output(sessionInfo()),'analysis_logs/sessionInfo_review_revision.txt')
print(counts); print(direct_results); print(sr %>% filter(EvidenceSet=='All eligible endpoints',Rho==0))
