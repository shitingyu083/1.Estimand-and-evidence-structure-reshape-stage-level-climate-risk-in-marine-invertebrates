"""Independent count, BH-family and coding-alignment checks of final outputs."""
from pathlib import Path
import json
import numpy as np
import pandas as pd

ROOT=Path(__file__).resolve().parents[1]
def read(p): return pd.read_csv(ROOT/p)
checks=[]
def check(name,condition,detail):
    checks.append(dict(Check=name,Passed=bool(condition),Detail=detail))
    if not condition: raise AssertionError((name,detail))

d=read('data/processed/analysis_ready_model_dataset_deduplicated.csv')
raw=read('data/raw/analysis_ready_model_dataset_before_deduplication.csv')
primary=d[d.DevelopmentalModeGroup.eq('With metamorphic stage')]
key=['StudyID','SpeciesID','ObservedStage','StressorLabel','Metric','TransformedMetric','Category','lnRR','Variance','ControlTemperature_C','TreatmentTemperature_C','Control_pCO2_uatm','Treatment_pCO2_uatm','Control_pH','Treatment_pH']
norm=d[key].astype(str).apply(lambda s:s.str.replace(r'\s+',' ',regex=True).str.strip())
check('Exact duplicate removal and dose restoration',len(raw)-len(d)==32-11 and not norm.duplicated().any(),'2412 input; 32 cross-layer copies removed; 11 dose-distinct records restored')
check('Primary population',len(primary)==2214 and primary.StudyID.nunique()==149,'2214 effects; 149 StudyIDs')
check('Full population',len(d)==2391 and d.StudyID.nunique()==162,'2391 effects; 162 StudyIDs')
canonical=read('data/processed/analysis_ready_model_dataset.csv')
check('Canonical alias',canonical.equals(d),'Canonical and deduplicated files contain identical data')
coding=read('data/processed/ER_endpoint_direction_row_coding.csv')
check('Coding row alignment',len(coding)==len(d) and coding[['StudyID','SpeciesID']].astype(str).equals(d[['StudyID','SpeciesID']].astype(str)),'Row counts, StudyID and SpeciesID agree')
for filename in ['Fig4b_estimand_target_differences.csv','Fig3_subgroup_estimand_paired_differences.csv']:
    x=read('data/source_data/'+filename)
    # Only signed aligned-minus-directional contrasts have a zero-null test.
    # Folded absolute contrasts are retained as descriptive intervals with NA
    # P/q values because their differences are structurally non-negative.
    if filename.startswith('Fig3b'):
        x=x[x.TargetDifference.eq('Aligned - directional') & x.P_value.notna()].copy()
    else:
        x=x[x.Contrast.eq('Endpoint-aligned response - directional change') & x.P_value.notna()].copy()
    p=x.P_value.to_numpy(); order=np.argsort(p)
    ranked=np.minimum(1,np.minimum.accumulate((p[order]*len(p)/np.arange(1,len(p)+1))[::-1])[::-1])
    q=np.empty(len(p)); q[order]=ranked
    n=len(x); ns=int((q<.05).sum())
    check(filename+' BH',np.allclose(q,x.FDR_q_value,atol=1e-12),f'{n} estimable tests; independent BH calculation')
    expected=np.where(q<.001,'***',np.where(q<.01,'**',np.where(q<.05,'*','')))
    check(filename+' stars',np.array_equal(expected,x.Significance.fillna('').to_numpy()) and (q<.05).sum()==ns,f'{ns} significant; all stars agree with adjusted q')
dictionary=read('data/source_data/Review_endpoint_dictionary.csv')
forest=read('data/source_data/Fig2_subgroup_three_estimands.csv')
for (moderator,target),g in forest.groupby(['Moderator','EstimandCode']):
    x=g.dropna(subset=['P_value'])
    if x.empty:
        check(f'{moderator}/{target} no unsupported zero-null tests',g.FDR_q_value.isna().all(),'Folded or non-estimable targets have no q value')
        continue
    pv=x.P_value.to_numpy();order=np.argsort(pv)
    ranked=np.minimum(1,np.minimum.accumulate((pv[order]*len(pv)/np.arange(1,len(pv)+1))[::-1])[::-1])
    expected=np.empty(len(pv));expected[order]=ranked
    check(f'{moderator}/{target} forest BH',np.allclose(expected,x.FDR_q_value),f'{len(pv)} individual q values verified; no scalar recycling')
for field in ['IndependentReviewer','IndependentDirection','SourceQuotation','PageOrTable','DisagreementResolution','ReviewDate']:
    dictionary[field]=''
dictionary['ReviewStatus']='Pending independent human review'
if (ROOT/'data/raw/independent_review_attestation.json').exists():
    import subprocess,sys
    subprocess.run([sys.executable,str(ROOT/'R/56_independent_review_record.py')],check=True)
else:
    dictionary.to_csv(ROOT/'data/source_data/Review_independent_coding_worksheet.csv',index=False)
pd.DataFrame(checks).to_csv(ROOT/'analysis_logs/revision_quality_checks.csv',index=False)
print(json.dumps(checks,indent=2))
