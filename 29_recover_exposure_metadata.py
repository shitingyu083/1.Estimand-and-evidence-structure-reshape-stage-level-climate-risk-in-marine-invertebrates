"""Recover only exposure values supported by archived labels or public source metadata."""
from pathlib import Path
from urllib.request import urlopen
import io, re, hashlib, json
import numpy as np
import pandas as pd
ROOT=Path(__file__).resolve().parents[1]
cache=ROOT/"data/source_data/exposure_source_cache"
cache.mkdir(parents=True,exist_ok=True)
sources={}
for ident in (973022,964218,964219):
    p=cache/f"PANGAEA_{ident}.tsv"
    if not p.exists():
        p.write_bytes(urlopen(f"https://doi.pangaea.de/10.1594/PANGAEA.{ident}?format=textfile",timeout=40).read())
    sources[ident]=p.read_text(encoding="utf-8")
d=pd.read_csv(ROOT/"data/processed/analysis_ready_model_dataset_deduplicated.csv")
cols=["ControlTemperature_C","TreatmentTemperature_C","DeltaTemperature_C","Control_pH","Treatment_pH","Delta_pH","Control_pCO2_uatm","Treatment_pCO2_uatm","DurationValue"]
out=d[["LatestModelRowID","StudyID","SpeciesID","StressorLabel","Metric","ExtractionControl","ExtractionTreatment","SourceFile"]].copy()
for c in cols: out[c]=np.nan
for c in ["DurationUnit","ExposureSource","ExposureLocator","DoseType","SharedControlStatus"]: out[c]=""
out["SharedControlStatus"]="Not resolved; no covariance imputed"
for c in cols:
    if c in d.columns: out[c]=d[c]
for c in ['pHScale','AcclimationDays','ReportedTankCount','SourceControlN','SourceTreatmentN','Delta_pCO2_uatm','VerifiedSourceLocators']:
    if c in d.columns: out[c]=d[c]
inherited=d.VerifiedSourceLocators.fillna('').ne('')
out.loc[inherited,'ExposureSource']='Archived original workbook / Alter et al. (2024), Zenodo 10223034'
out.loc[inherited,'ExposureLocator']=d.loc[inherited,'VerifiedSourceLocators']
out.loc[inherited,'DoseType']='Source-reported treatment values; pH scale retained separately'
out['StudyDurationValue']=np.nan; out['StudyDurationUnit']=''
out['AssayDurationValue']=np.nan; out['AssayDurationUnit']=''
for i,r in out.iterrows():
    control,treat=str(r.ExtractionControl),str(r.ExtractionTreatment)
    if r.StudyID==243:
        out.loc[i,['ExposureSource','ExposureLocator','DoseType','DurationValue','DurationUnit']]=['https://doi.org/10.5285/7adc7b14-abae-4ab9-b60b-b9b6e0e9f320','CO2calc_chemistry.csv: technical replicate, aquarium/week and aquarium means; repository reports 92-day experiment','Measured chemistry; pH scale unresolved',92,'day (repository experimental duration)']
        out.loc[i,'SharedControlStatus']=d.loc[i,'ExperimentalCovarianceStatus']
    if r.StudyID==245:
        if re.fullmatch(r"[0-9.]+ C",control) and re.fullmatch(r"[0-9.]+ C",treat):
            a,b=float(control[:-2]),float(treat[:-2])
            out.loc[i,["ControlTemperature_C","TreatmentTemperature_C","DeltaTemperature_C"]]=[a,b,b-a]
        if control.startswith("pH ") and treat.startswith("pH "):
            a,b=float(control[3:]),float(treat[3:])
            out.loc[i,["Control_pH","Treatment_pH","Delta_pH"]]=[a,b,b-a]
        out.loc[i,["ExposureSource","ExposureLocator","DoseType"]]=[r.SourceFile,"Archived ExtractionControl / ExtractionTreatment labels","Nominal; original table not independently re-extracted"]
    if r.StudyID==244:
        assert "+2" in sources[973022] and "1 year" in sources[973022]
        out.loc[i,["DeltaTemperature_C","Delta_pH","DurationValue","DurationUnit"]]=[2 if r.StressorLabel!="Acidification" else 0,-.2 if r.StressorLabel!="Warming" else 0,8,"month (approximately)"]
        out.loc[i,['StudyDurationValue','StudyDurationUnit','AssayDurationValue','AssayDurationUnit']]=[1,'year (nearly)',9,'week (approximately; full-text methods)']
        out.loc[i,["ExposureSource","ExposureLocator","DoseType"]]=["https://doi.org/10.1098/rspb.2024.1161","Methods: eight months treatment exposure; near-one-year study; final nine-week calcification assay (Dryad description says eight weeks)","Nominal treatment offsets; experimental durations distinguished"]
        out.loc[i,"SharedControlStatus"]="Shared controls, header/tank and genet dependence re-extracted; 24 by 24 bootstrap covariance archived"
        environment=pd.read_csv(io.StringIO(sources[973022].split('*/',1)[1].strip()),sep='\t')
        ec=['Temp [°C]','pHT (total scale)','pCO2water_SST_wet [µatm]']
        environment=environment[['Treat','ID (Header)','ID (Tank)']+ec].drop_duplicates()
        em=environment.groupby(['Treat','ID (Header)'])[ec].mean().groupby('Treat').mean()
        tr={'Acidification':'Ocean acidification','Warming':'ocean warming','Warming + acidification':'combined future ocean treatments'}[r.StressorLabel]
        for c,control_col,treatment_col in zip(ec,['ControlTemperature_C','Control_pH','Control_pCO2_uatm'],['TreatmentTemperature_C','Treatment_pH','Treatment_pCO2_uatm']):
            out.loc[i,control_col]=em.loc['Control',c]; out.loc[i,treatment_col]=em.loc[tr,c]
        out.loc[i,'DeltaTemperature_C']=out.loc[i,'TreatmentTemperature_C']-out.loc[i,'ControlTemperature_C']
        out.loc[i,'Delta_pH']=out.loc[i,'Treatment_pH']-out.loc[i,'Control_pH']
        out.loc[i,'Delta_pCO2_uatm']=out.loc[i,'Treatment_pCO2_uatm']-out.loc[i,'Control_pCO2_uatm']
        out.loc[i,'pHScale']='total'
        out.loc[i,'DoseType']='Measured treatment chemistry, equal header weighting; nominal offsets +2 C / -0.2 pH archived in source'
    if r.StudyID==246:
        ident=964218 if "964218" in str(r.SourceFile) else 964219
        raw=pd.read_csv(io.StringIO(sources[ident].split("*/",1)[1].strip()),sep="\t")
        assert set(raw["Exp duration [h]"].dropna())=={48}
        out.loc[i,["Control_pH","Treatment_pH","Delta_pH","Control_pCO2_uatm","Treatment_pCO2_uatm","DurationValue","DurationUnit"]]=[8.01,7.63,-.38,400,1000,48,"hour"]
        out.loc[i,["ExposureSource","ExposureLocator","DoseType"]]=[f"https://doi.org/10.1594/PANGAEA.{ident}","Dataset abstract and Exp duration [h] column","Nominal pCO2 and pHT"]
        out.loc[i,"SharedControlStatus"]="Same experimental series; cross-endpoint covariance not estimated"
out.to_csv(ROOT/"data/source_data/Review_exposure_recovery.csv",index=False)
summary=[]
for c in cols:
    x=out[out[c].notna()]
    summary.append(dict(Field=c,RecoveredEffects=len(x),StudyIDs=x.StudyID.nunique(),MissingEffects=len(out)-len(x)))
pd.DataFrame(summary).to_csv(ROOT/"data/source_data/Review_exposure_recovery_summary.csv",index=False)
(cache/"manifest.json").write_text(json.dumps({str(k):{"url":f"https://doi.pangaea.de/10.1594/PANGAEA.{k}?format=textfile","sha256":hashlib.sha256((cache/f"PANGAEA_{k}.tsv").read_bytes()).hexdigest()} for k in sources},indent=2),encoding="utf-8")
print(pd.DataFrame(summary).to_string(index=False))
