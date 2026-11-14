# Additional analyses specified on 13 September 2026 after the source audit.
# These are exploratory; BH families below are fixed before model fitting.
source('R/00_setup.R')
suppressPackageStartupMessages({library(robumeta);library(clubSandwich)})
d <- read_csv(guess_max=100000, 'data/processed/analysis_ready_model_dataset_deduplicated.csv',show_col_types=FALSE)
cc <- read_csv(guess_max=100000, 'data/processed/ER_endpoint_direction_row_coding.csv',show_col_types=FALSE)
rules <- read_csv(guess_max=100000, 'data/source_data/Boundary_endpoint_source_rules.csv',show_col_types=FALSE)
stopifnot(nrow(d)==nrow(cc),identical(d$StudyID,cc$StudyID))
d$Aligned <- cc$aligned_lnRR_strict
d$External <- rules$ExternalConsensusEligible
d <- d %>% filter(DevelopmentalModeGroup=='With metamorphic stage')

fit_stage <- function(x,label) {
  stages <- x %>% filter(is.finite(yi)) %>% distinct(StudyID,ObservedStage) %>% count(ObservedStage) %>% filter(n>=3) %>% pull(ObservedStage)
  x <- x %>% filter(is.finite(yi),ObservedStage %in% stages) %>% mutate(ObservedStage=droplevels(factor(ObservedStage,levels=stage_levels)))
  f <- robu(yi~0+ObservedStage,data=x,studynum=StudyID,var.eff.size=vi_model,modelweights='CORR',rho=.8,small=TRUE)
  as_tibble(f$reg_table) %>% transmute(Specification=label,Stage=levels(x$ObservedStage),Estimate=b.r,SE=SE,df=dfs,CI_low=CI.L,CI_high=CI.U,P_value=prob,StudyCount=n_distinct(x$StudyID),EffectSizeCount=nrow(x))
}
sens <- bind_rows(
  fit_stage(d %>% mutate(yi=lnRR),'Repaired directional'),
  fit_stage(d %>% filter(StudyID!=244) %>% mutate(yi=lnRR),'Directional: exclude Study 244'),
  fit_stage(d %>% mutate(yi=Aligned),'Source-based alignment'),
  fit_stage(d %>% filter(External) %>% mutate(yi=Aligned),'Published consensus only'),
  fit_stage(d %>% filter(!DoseRestoredRecord) %>% mutate(yi=lnRR),'Directional: omit restored dose rows')
) %>% group_by(Specification) %>% mutate(FDR_q_value=p.adjust(P_value,'BH')) %>% ungroup()
write_csv_safe(sens,'data/source_data/Boundary_stage_sensitivities.csv')

# Recover the full Study 244 covariance when aggregating its 24 signed effects.
j <- d %>% filter(StudyID==244) %>% arrange(LatestModelRowID)
Vj <- read.csv('data/source_data/Boundary_Study244_sampling_covariance.csv',check.names=FALSE,row.names=1)
Vj <- as.matrix(Vj[as.character(j$LatestModelRowID),as.character(j$LatestModelRowID)])
stopifnot(max(abs(diag(Vj)-j$vi_model))<1e-10,min(eigen(Vj,symmetric=TRUE)$values)>-1e-10)
cells <- d %>% group_by(StudyID,ObservedStage) %>% summarise(yi=weighted.mean(lnRR,1/vi_model),vi=(.2*sum(1/vi_model)+.8*sum(1/sqrt(vi_model))^2)/sum(1/vi_model)^2,.groups='drop')
w <- (1/j$vi_model)/sum(1/j$vi_model)
cells$vi[cells$StudyID==244] <- as.numeric(t(w)%*%Vj%*%w)
fits <- bind_rows(lapply(split(cells,cells$ObservedStage),function(x){
  f<-metafor::rma.uni(yi=x$yi,vi=x$vi,method='REML',test='knha')
  tibble(Stage=x$ObservedStage[1],Estimate=as.numeric(f$b),CI_low=f$ci.lb,CI_high=f$ci.ub,P_value=f$pval,StudyCount=nrow(x))
})) %>% mutate(FDR_q_value=p.adjust(P_value,'BH'))
write_csv_safe(fits,'data/source_data/Boundary_explicit_covariance_stage_REML.csv')

# Dose regressions: driver-specific exposure units, StudyID-correlated effects,
# stage and control-temperature adjustment. Never use acclimation as duration.
coef_rows<-list();test_rows<-list();diag_rows<-list();z<-1
for(target in c('Directional','Aligned')) for(driver in stressor_levels) {
  x<-d %>% filter(StressorLabel==driver) %>% mutate(yi=if(target=='Directional') lnRR else Aligned,
    DoseT=DeltaTemperature_C,DoseCO2=log2(Treatment_pCO2_uatm/Control_pCO2_uatm),BaselineT=ControlTemperature_C)
  terms<-c(if(driver!='Acidification') 'DoseT',if(driver!='Warming') 'DoseCO2','BaselineT')
  x<-x %>% filter(is.finite(yi),if_all(all_of(terms),is.finite))
  eligible<-x %>% distinct(StudyID,ObservedStage) %>% count(ObservedStage) %>% filter(n>=3) %>% pull(ObservedStage)
  x<-x %>% filter(ObservedStage %in% eligible) %>% mutate(Stage=droplevels(factor(ObservedStage,levels=stage_levels)))
  form<-as.formula(paste('yi ~ 0 + Stage +',paste(terms,collapse='+')))
  X<-model.matrix(form,x); rank<-qr(X)$rank
  diag_rows[[z]]<-tibble(Target=target,Driver=driver,k=nrow(x),m=n_distinct(x$StudyID),Parameters=ncol(X),Rank=rank,ConditionNumber=kappa(scale(X[,terms,drop=FALSE])),MinimumDoseT=if('DoseT'%in%terms)min(x$DoseT) else NA_real_,MaximumDoseT=if('DoseT'%in%terms)max(x$DoseT) else NA_real_,MinimumLog2CO2=if('DoseCO2'%in%terms)min(x$DoseCO2) else NA_real_,MaximumLog2CO2=if('DoseCO2'%in%terms)max(x$DoseCO2) else NA_real_)
  if(rank<ncol(X) || n_distinct(x$StudyID)<10*ncol(X)) {z<-z+1;next}
  f<-robu(form,data=x,studynum=StudyID,var.eff.size=vi_model,modelweights='CORR',rho=.8,small=TRUE)
  tab<-as_tibble(f$reg_table) %>% transmute(Target=target,Driver=driver,Term=labels,Estimate=b.r,SE=SE,df=dfs,CI_low=CI.L,CI_high=CI.U,P_value=prob,StableSmallSampleDF=dfs>=4,k=nrow(x),m=n_distinct(x$StudyID))
  coef_rows[[z]]<-tab
  ns<-nlevels(x$Stage); C<-matrix(0,ns-1,nrow(f$reg_table))
  for(ii in 2:ns){C[ii-1,1]<- -1;C[ii-1,ii]<-1}
  wt<-Wald_test(f,constraints=C,vcov='CR2',test='HTZ')
  test_rows[[z]]<-tibble(Target=target,Driver=driver,F_stat=wt$Fstat,df_num=wt$df_num,df_denom=wt$df_denom,P_value=wt$p_val)
  z<-z+1
}
coefs<-bind_rows(coef_rows) %>% group_by(Target) %>% mutate(FDR_q_value=p.adjust(P_value,'BH')) %>% ungroup()
omni<-bind_rows(test_rows) %>% mutate(FDR_q_value=p.adjust(P_value,'BH'))
write_csv_safe(coefs,'data/source_data/Boundary_dose_meta_regression.csv')
write_csv_safe(omni,'data/source_data/Boundary_dose_adjusted_stage_tests.csv')
write_csv_safe(bind_rows(diag_rows),'data/source_data/Boundary_dose_model_diagnostics.csv')
plotd<-coefs %>% filter(!grepl('^Stage',Term)) %>% mutate(Term=recode(Term,DoseT='Warming increment (per degree C)',DoseCO2='CO2 increment (per doubling)',BaselineT='Control temperature (per degree C)'))
p<-ggplot(plotd,aes(Estimate,Term,colour=Driver))+geom_vline(xintercept=0,linewidth=.2,linetype=2)+
  geom_errorbar(aes(xmin=CI_low,xmax=CI_high),orientation='y',width=0,linewidth=.35,position=position_dodge(.55))+
  geom_point(size=1.6,position=position_dodge(.55))+facet_grid(Target~.,scales='free_y')+
  scale_colour_manual(values=driver_pal)+labs(x='Adjusted slope (95% small-sample CI)',y=NULL)+theme_publication(base_size=8)+theme(panel.grid=element_blank(),legend.position='top')
save_figure(p,'figures/extended/SupplementaryFigS6_exposure_adjusted_meta_regression',190,135,dpi=600)
print(sens);print(omni)
