"""Recover species-level measured exposures; aggregate morphology by aquarium.

Individual behavioural IDs have no proven aquarium crosswalk and are explicitly
flagged, not assigned a fabricated shared-control covariance.
"""
from pathlib import Path
import pandas as pd,numpy as np,json
root=Path(__file__).resolve().parents[1];src=root/'data/raw/boundary_sources'
d=pd.read_csv(root/'data/processed/analysis_ready_model_dataset_deduplicated.csv')
chem=pd.read_csv(src/'Williams2024_CO2calc_chemistry.csv',encoding='utf-8-sig')
chem.columns=chem.columns.str.strip();chem['species']=chem.species.str.strip();chem['climate']=chem.climate.str.strip()
# Technical duplicates -> aquarium/week mean -> aquarium mean -> species mean.
fields=['temperature_C','pH','pCO2_uAtm']
for c in fields: chem[c]=pd.to_numeric(chem[c],errors='coerce')
a=chem.groupby(['species','climate','aquarium','week'])[fields].mean().reset_index()
tanks=a.groupby(['species','climate','aquarium'])[fields].mean().reset_index()
means=tanks.groupby(['species','climate'])[fields].mean()
tanks.to_csv(root/'data/source_data/Boundary_Williams_measured_exposure.csv',index=False)
morph=pd.read_csv(src/'Williams2024_Macrofaunal_final_morphology.csv')
morph['species']=morph.species.str.strip()
morph['species']=morph.species.replace({'Aquiyoldia eightsi':'Aequiyoldia eightsi'})
# The morphology file misspells the genus; chemistry and the archived extraction
# use Aequiyoldia for the same species and aquarium IDs. Original CSV is retained.
repair=[]
for i,r in d[d.StudyID.eq(243)].iterrows():
    sp=str(r.SpeciesID).strip()
    if (sp,'Ambient') in means.index and (sp,'Future') in means.index:
        for f,dst in [('temperature_C','Temperature_C'),('pH','_pH'),('pCO2_uAtm','_pCO2_uatm')]:
            d.loc[i,'Control'+dst]=means.loc[(sp,'Ambient'),f]
            d.loc[i,'Treatment'+dst]=means.loc[(sp,'Future'),f]
        d.loc[i,'DeltaTemperature_C']=d.loc[i,'TreatmentTemperature_C']-d.loc[i,'ControlTemperature_C']
        d.loc[i,'Delta_pCO2_uatm']=d.loc[i,'Treatment_pCO2_uatm']-d.loc[i,'Control_pCO2_uatm']
        d.loc[i,'Delta_pH']=d.loc[i,'Treatment_pH']-d.loc[i,'Control_pH']
        d.loc[i,'pHScale']='Not resolved from chemistry table; not pooled across scales'
    source=str(r.SourceFile)
    if 'morphology' not in source.lower():
        d.loc[i,'ExperimentalCovarianceStatus']='Mesocosm ID present for ecosystem endpoints; behaviour-to-aquarium crosswalk not established; StudyID clustering retained'
        continue
    # The source extraction retained its column name in Metric/notes.
    metric=str(r.Metric).lower()
    column_map={'body length':'Length_mm','body height':'Height_mm','body width':'Width_mm','arm length':'Arm_length_mm','pit length':'Pit_length_mm','tube cone length':'Cone_length_mm','anterior aperture':'Anterior_aperture_mm'}
    candidates=[column_map[metric]] if metric in column_map else []
    if len(candidates)!=1:
        repair.append({'LatestModelRowID':r.LatestModelRowID,'Metric':r.Metric,'Status':'Column mapping unresolved','Candidates':str(candidates)});continue
    c=candidates[0]
    print('Morphology mapping:',r.LatestModelRowID,sp,c,morph.loc[morph.species.eq(sp),'Climate'].unique())
    x=morph[morph.species.eq(sp)].groupby(['Climate','Aquarium'])[c].mean().dropna()
    cvals=x.loc['Ambient'].to_numpy();tvals=x.loc['Future'].to_numpy()
    cm,tm=cvals.mean(),tvals.mean();cs,ts=cvals.std(ddof=1),tvals.std(ddof=1)
    y=np.log(tm/cm); v=ts**2/(len(tvals)*tm**2)+cs**2/(len(cvals)*cm**2)
    repair.append({'LatestModelRowID':r.LatestModelRowID,'Metric':r.Metric,'Status':'Aquarium-aggregated','OldLnRR':r.lnRR,'Estimate':y,'OldVariance':r.Variance,'Variance':v,'ControlAquaria':len(cvals),'TreatmentAquaria':len(tvals)})
    for name,val in {'lnRR':y,'EffectSize':y,'Variance':v,'VarianceUsedInModel':max(v,1e-4),'VarianceModel':max(v,1e-4),'vi_raw':v,'vi_model':max(v,1e-4),'abs_lnRR':abs(y),'abs_ei':abs(y),'AbsoluteLnRR':abs(y),'ControlMean':cm,'TreatmentMean':tm,'ControlSD':cs,'TreatmentSD':ts,'ControlN':len(cvals),'TreatmentN':len(tvals)}.items():d.loc[i,name]=val
    d.loc[i,'ExtractionNotes']='Individual morphology averaged within aquarium before mean/SD/N and lnRR calculation; shared endpoints clustered by StudyID'
    d.loc[i,'ExperimentalCovarianceStatus']='Aquarium aggregation completed; cross-endpoint clustering at StudyID'
pd.DataFrame(repair).to_csv(root/'data/source_data/Boundary_Williams_morphology_repair.csv',index=False)
print(pd.DataFrame(repair).to_string(index=False))
for name in ['analysis_ready_model_dataset.csv','analysis_ready_model_dataset_deduplicated.csv']:d.to_csv(root/'data/processed'/name,index=False)
