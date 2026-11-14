source("R/00_setup.R")
dd <- read_csv(guess_max=100000, "data/processed/analysis_ready_model_dataset_deduplicated.csv",show_col_types=FALSE)
cc <- read_csv(guess_max=100000, "data/processed/ER_endpoint_direction_row_coding.csv",show_col_types=FALSE)
stopifnot(nrow(dd)==nrow(cc),identical(dd$StudyID,cc$StudyID))
dd$aligned <- cc$aligned_lnRR_strict
dd$rule <- cc$EndpointDirectionClassStrict
balance <- dd %>% group_by(ObservedStage) %>% summarise(BeneficialEffectSizes=sum(rule=="beneficial_when_increases"),AdverseEffectSizes=sum(rule=="adverse_when_increases"),ContextDependentEffectSizes=sum(rule=="context_dependent"),EffectSizeCount=n(),StudyCount=n_distinct(StudyID),SpeciesCount=n_distinct(SpeciesID),DriverCount=n_distinct(StressorLabel),CategoryCount=n_distinct(Category),AlignedEffectSizes=sum(is.finite(aligned)),ContextDependentShare=mean(rule=="context_dependent"),.groups="drop")
write_csv_safe(balance,"data/processed/ER_evidence_balance_by_stage.csv")
rr <- read_csv(guess_max=100000, "data/processed/main_results_REML.csv",show_col_types=FALSE) %>% filter(DevelopmentalModeGroup=="With metamorphic stage",GroupType=="mode_stage_all") %>% mutate(Estimand=recode(Estimand,"Directional lnRR"="Raw directional lnRR","Absolute |lnRR|"="Exploratory response magnitude |lnRR|"))
cells <- dd %>% filter(DevelopmentalModeGroup=="With metamorphic stage",is.finite(aligned)) %>% group_by(StudyID,ObservedStage) %>% summarise(yi=sum(aligned/vi_model)/sum(1/vi_model),vi=1/sum(1/vi_model),k=n(),.groups="drop")
ar <- bind_rows(lapply(split(cells,cells$ObservedStage),function(x){
 if(nrow(x)<3) return(NULL)
 f<-metafor::rma.uni(yi=x$yi,vi=x$vi,method="REML")
 tibble(Estimand="Biologically aligned performance lnRR",ObservedStage=x$ObservedStage[1],Estimate=as.numeric(f$b),SE=f$se,CI_low=f$ci.lb,CI_high=f$ci.ub,P_value=f$pval,StudyCount=nrow(x),EffectSizeCount=sum(x$k),I2_percent=f$I2,Tau2=f$tau2)
})) %>% mutate(FDR_q_value=p.adjust(P_value,"BH"))
write_csv_safe(bind_rows(rr,ar),"data/processed/ER_robustness_summary.csv")

