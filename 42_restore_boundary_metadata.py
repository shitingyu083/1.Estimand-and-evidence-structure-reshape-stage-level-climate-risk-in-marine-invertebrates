"""Restore source pointers and dose-distinct records before fitting any model.

Original workbook row numbers in the legacy table are one-based data rows.
The new source locators explicitly use physical spreadsheet/CSV rows, header=1.
Do not infer an experimental replicate or exposure duration from acclimation.
"""
from pathlib import Path
import pandas as pd, numpy as np, re, json, hashlib
root=Path(__file__).resolve().parents[1]; src=root/'data/raw/boundary_sources'
d=pd.read_csv(root/'data/processed/analysis_ready_model_dataset_deduplicated.csv')
old=pd.read_excel(src/'original_invertebrates.xlsx'); pub=pd.read_csv(src/'Alter2024_DataInverts.csv')
old.columns=old.columns.str.strip(); pub.columns=pub.columns.str.strip()
def norm(v): return re.sub(r'\s+',' ',str(v).strip().lower())
def key(r,source=False):
    return tuple(norm(r[c]) for c in ['REF','Species','LifeStage','Stressor','Metric','TransformedMetric','Category'])+(round(float(r['ei' if source else 'lnRR']),6),round(float(r['vei' if source else 'Variance']),8))
look={}
for label,df in [('original_invertebrates.xlsx',old),('Alter2024_DataInverts.csv',pub)]:
    for i,r in df.iterrows():
        try: k=key(r,True)
        except (ValueError,TypeError): continue
        look.setdefault(k,[]).append((label,i+2,r))
mapping={'CO2ppmC':'Control_pCO2_uatm','CO2ppmI':'Treatment_pCO2_uatm','TempC':'ControlTemperature_C','TempI':'TreatmentTemperature_C','pH C':'Control_pH','Ph I':'Treatment_pH','pH scale':'pHScale','AcclimationDays':'AcclimationDays','NbTank':'ReportedTankCount','ControlN':'SourceControlN','TreatmentN':'SourceTreatmentN'}
dose=['TempC','TempI','CO2ppmC','CO2ppmI','pH C','Ph I','pH scale']
rows=[]; audit=[]; nextid=2413
for _,r in d.iterrows():
    matches=look.get(key(r),[])
    originals=[m for m in matches if m[0]=='original_invertebrates.xlsx']
    candidates=originals or matches
    by_dose={}
    for m in candidates: by_dose.setdefault(tuple(norm(m[2].get(c)) for c in dose),[]).append(m)
    groups=list(by_dose.values()) or [[]]
    for j,g in enumerate(groups):
        z=r.to_dict(); z['ParentModelRowID']=int(r.LatestModelRowID)
        z['LegacyOriginalSourceRows']=r.OriginalSourceRows
        z['DoseRestoredRecord']=j>0
        if j:
            z['LatestModelRowID']=nextid; nextid+=1
            z['RowKey']=str(r.RowKey)+'||restored_dose_'+str(j+1)
        z['VerifiedSourceLocators']='; '.join(f'{a}:physical_row_{b}' for a,b,_ in g)
        z['SourceMatchStatus']='Exact endpoint and rounded effect/variance; dose-specific' if g else 'Update-source provenance retained; not in inherited database'
        z['PublishedSourceRows']=';'.join(str(b) for a,b,_ in matches if a=='Alter2024_DataInverts.csv' and (not g or tuple(norm(_ .get(c)) for c in dose)==tuple(norm(g[0][2].get(c)) for c in dose)))
        if matches:
            z['OriginalSourceRows']=';'.join(str(b-1) for a,b,_ in g if a=='original_invertebrates.xlsx')
            z['OriginalFileRows']=sum(a=='original_invertebrates.xlsx' for a,b,_ in g)
            z['PresentInOriginalFile']=bool(z['OriginalFileRows'])
            z['SourceDataset']=g[0][0]; z['SourceRow']=g[0][1]
        for a,b in mapping.items():
            vals={norm(m[2].get(a)):m[2].get(a) for m in g if pd.notna(m[2].get(a))}
            z[b]=next(iter(vals.values())) if len(vals)==1 else np.nan
            z[b+'_Conflict']=len(vals)>1
        z['DeltaTemperature_C']=z['TreatmentTemperature_C']-z['ControlTemperature_C'] if pd.notna(z['TreatmentTemperature_C']) and pd.notna(z['ControlTemperature_C']) else np.nan
        z['Delta_pCO2_uatm']=z['Treatment_pCO2_uatm']-z['Control_pCO2_uatm'] if pd.notna(z['Treatment_pCO2_uatm']) and pd.notna(z['Control_pCO2_uatm']) else np.nan
        z['Delta_pH']=z['Treatment_pH']-z['Control_pH'] if pd.notna(z['Treatment_pH']) and pd.notna(z['Control_pH']) else np.nan
        z['ExperimentalCovarianceStatus']='Not recovered; StudyID cluster used'
        rows.append(z)
        audit.append({k:z[k] for k in ['LatestModelRowID','ParentModelRowID','StudyID','LegacyOriginalSourceRows','OriginalSourceRows','PublishedSourceRows','VerifiedSourceLocators','SourceMatchStatus','DoseRestoredRecord']})
result=pd.DataFrame(rows).sort_values('LatestModelRowID').reset_index(drop=True)
assert result.LatestModelRowID.is_unique and result.RowKey.is_unique
assert result.DoseRestoredRecord.sum()==11
for name in ['analysis_ready_model_dataset.csv','analysis_ready_model_dataset_deduplicated.csv']:
    result.to_csv(root/'data/processed'/name,index=False)
pd.DataFrame(audit).to_csv(root/'data/source_data/Boundary_source_pointer_repair.csv',index=False)
coverage=[{'Field':c,'Effects':int(result[c].notna().sum()),'Studies':int(result.loc[result[c].notna(),'StudyID'].nunique()),'Conflicts':int(result[c+'_Conflict'].sum())} for c in mapping.values()]
pd.DataFrame(coverage).to_csv(root/'data/source_data/Boundary_metadata_coverage.csv',index=False)
(root/'data/source_data/Boundary_restoration_summary.json').write_text(json.dumps({'input_deduplicated':len(d),'restored_dose_rows':11,'final_effects':len(result),'primary_effects':int(result.DevelopmentalModeGroup.eq('With metamorphic stage').sum())},indent=2))
print('Restored 11 dose-distinct records in 10 prior collapsed groups; final effects',len(result))
