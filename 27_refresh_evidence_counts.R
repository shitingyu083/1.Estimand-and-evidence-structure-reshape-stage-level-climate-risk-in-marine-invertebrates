# Rebuild derived support tables; no inherited model summaries are reused.
source("R/00_setup.R")
dd <- read_csv(guess_max=100000, "data/processed/analysis_ready_model_dataset_deduplicated.csv",show_col_types=FALSE)
primary_dd <- filter(dd,DevelopmentalModeGroup=="With metamorphic stage")
write_csv_safe(primary_dd %>% count(StudyID,ObservedStage) %>% count(StudyID,name="StageCount") %>% count(StageCount,name="StudyCount"),"data/source_data/Fig1c_study_stage_overlap_counts.csv")
write_csv_safe(primary_dd %>% group_by(ObservedStage,StressorLabel) %>% summarise(EffectSizeCount=n(),StudyCount=n_distinct(StudyID),.groups="drop"),"data/source_data/Fig2_evidence_map.csv")
write_csv_safe(dd %>% count(Category,ObservedStage,StressorLabel,name="EffectSizeCount") %>% group_by(Category) %>% mutate(CategoryTotal=sum(EffectSizeCount)) %>% ungroup(),"data/source_data/ExtendedData_category_stage_map.csv")

