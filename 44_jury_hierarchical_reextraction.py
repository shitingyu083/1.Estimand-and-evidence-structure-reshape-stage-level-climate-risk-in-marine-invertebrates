"""Study 244: shared-control, header/tank and crossed-genet bootstrap.

Use mean-one exponential genet multipliers within species/site jointly across
treatments (positive weights retain sparse observed tank cells), then resample the two
headers per treatment and tanks within each sampled header. Species share the
same header/tank draws. Means give equal weight to tanks and headers. This is
a design sensitivity with only two headers per treatment, not large-sample
validation of the experimental treatment effect.
"""
from pathlib import Path
import pandas as pd, numpy as np, io, json
root=Path(__file__).resolve().parents[1]
raw=pd.read_csv(io.StringIO((root/'data/raw/boundary_sources/pangaea973022').read_text(encoding='utf-8').split('*/',1)[1].strip()),sep='\t')
raw['Species UID']=raw['Species UID'].str.strip()
col='Calc rate CaCO3 [mg/g/day]'; raw[col]=pd.to_numeric(raw[col],errors='coerce')
d=pd.read_csv(root/'data/processed/analysis_ready_model_dataset_deduplicated.csv')
j=d[d.StudyID.eq(244)].copy(); rng=np.random.default_rng(20260913); B=9999
treats=list(raw.Treat.unique()); tanks=raw['ID (Tank)'].unique(); ti={t:i for i,t in enumerate(tanks)}
design=raw[['Treat','ID (Header)','ID (Tank)']].drop_duplicates()
sampled={}; fixed={}
for tr in treats:
    x=design[design.Treat.eq(tr)]; headers=x['ID (Header)'].unique()
    assert len(headers)==2
    hdraw=rng.integers(0,2,size=(B,2)); weights=np.zeros((B,len(tanks)))
    fw=np.zeros(len(tanks))
    for hidx,h in enumerate(headers):
        ts=x.loc[x['ID (Header)'].eq(h),'ID (Tank)'].to_numpy()
        fw[[ti[t] for t in ts]]=1/(2*len(ts))
        for occurrence in range(2):
            cnt=rng.multinomial(len(ts),np.full(len(ts),1/len(ts)),size=B)/(2*len(ts))
            cnt*=hdraw[:,occurrence,None]==hidx
            weights[:,[ti[t] for t in ts]]+=cnt
    sampled[tr]=weights; fixed[tr]=fw
means={}; boots={}; records=[]
for species,x in raw.groupby('Species UID',sort=False):
    x=x.copy(); genets=x['ID (Colony)'].unique(); gi={g:i for i,g in enumerate(genets)}
    gw=np.zeros((B,len(genets)))
    for _,z in x[['Site','ID (Colony)']].drop_duplicates().groupby('Site'):
        ids=[gi[g] for g in z['ID (Colony)']]
        w=rng.exponential(size=(B,len(ids)))
        gw[:,ids]=w/w.mean(axis=1,keepdims=True)
    tm=np.full(len(tanks),np.nan); bm=np.full((B,len(tanks)),np.nan)
    for tank,z in x.groupby('ID (Tank)'):
        z=z[z[col].notna()]; y=z[col].to_numpy()
        if not len(y): continue
        w=gw[:,[gi[g] for g in z['ID (Colony)']]]
        denom=w.sum(axis=1)
        bm[:,ti[tank]]=np.divide(w@y,denom,out=np.full(B,np.nan),where=denom>0)
        tm[ti[tank]]=y.mean()
    for tr in treats:
        point=0.; v=np.zeros(B)
        for _,hh in design[design.Treat.eq(tr)].groupby('ID (Header)'):
            ix=[ti[t] for t in hh['ID (Tank)']]
            point+=np.nanmean(tm[ix])/2
            w=sampled[tr][:,ix]; vals=bm[:,ix]
            mass=w.sum(axis=1)
            denom=np.sum(w*np.isfinite(vals),axis=1)
            numer=np.sum(w*np.nan_to_num(vals),axis=1)
            hm=np.divide(numer,denom,out=np.full(B,np.nan),where=denom>0)
            v+=np.where(mass>0,mass*hm,0)
        means[(species,tr)]=point
        boots[(species,tr)]=v
trmap={'Acidification':'Ocean acidification','Warming':'Ocean warming','Warming + acidification':'Ocean warming and acidification'}
print('Treatments:',treats)
# Resolve exact public treatment labels without relying on manuscript shorthand.
for tr in treats:
    if ('warming' in tr.lower() and 'acidification' in tr.lower()) or 'combined' in tr.lower(): trmap['Warming + acidification']=tr
    elif 'warming' in tr.lower(): trmap['Warming']=tr
    elif 'acidification' in tr.lower(): trmap['Acidification']=tr
matrix=[]
for idx,r in j.iterrows():
    sp=str(r.SpeciesID).strip(); tr=trmap[r.StressorLabel]
    c,t=means[(sp,'Control')],means[(sp,tr)]
    bc,bt=boots[(sp,'Control')],boots[(sp,tr)]
    with np.errstate(divide='ignore',invalid='ignore'): bs=np.log(bt/bc)
    bs[(bt<=0)|(bc<=0)]=np.nan
    matrix.append(bs)
    records.append({'LatestModelRowID':r.LatestModelRowID,'SpeciesID':sp,'StressorLabel':r.StressorLabel,'OldLnRR':r.lnRR,'OldVariance':r.Variance,'ControlHeaderMean':c,'TreatmentHeaderMean':t,'Estimate':np.log(t/c),'BootstrapInvalidFraction':float(np.mean(~np.isfinite(bs)))})
mat=np.asarray(matrix).T; valid=np.all(np.isfinite(mat),axis=1)
print('Joint valid bootstrap fraction:',valid.mean())
print(pd.DataFrame(records)[['SpeciesID','StressorLabel','ControlHeaderMean','TreatmentHeaderMean','BootstrapInvalidFraction']].to_string(index=False))
assert valid.mean()>.95, 'Too many undefined log ratios: source-specific review required'
cov=np.cov(mat[valid],rowvar=False,ddof=1); result=pd.DataFrame(records)
result['Variance']=np.diag(cov); result['CI_low']=np.quantile(mat[valid],.025,axis=0); result['CI_high']=np.quantile(mat[valid],.975,axis=0)
result['BootstrapValidJointDraws']=int(valid.sum()); result['BootstrapResamples']=B
result.to_csv(root/'data/source_data/Boundary_Study244_hierarchical_effects.csv',index=False)
pd.DataFrame(cov,index=j.LatestModelRowID,columns=j.LatestModelRowID).rename_axis('LatestModelRowID').to_csv(root/'data/source_data/Boundary_Study244_sampling_covariance.csv')
raw.to_csv(root/'data/source_data/Boundary_Study244_experimental_units.csv',index=False)
for _,r in result.iterrows():
    m=d.LatestModelRowID.eq(r.LatestModelRowID)
    for field in ['lnRR','EffectSize']: d.loc[m,field]=r.Estimate
    for field in ['AbsoluteLnRR','abs_ei','abs_lnRR']: d.loc[m,field]=abs(r.Estimate)
    for field in ['Variance','VarianceUsedInModel','VarianceModel','vi_raw','vi_model']: d.loc[m,field]=r.Variance
    for field in ['ControlMean','ControlSD','ControlN','TreatmentMean','TreatmentSD','TreatmentN']:
        d.loc[m,'LegacyNubbin_'+field]=d.loc[m,field]
        d.loc[m,field]=np.nan
    d.loc[m,'ExtractionNotes']='Re-extracted: equal-header/tank means; crossed-genet + nested header/tank bootstrap; see Boundary_Study244_hierarchical_effects.csv'
    d.loc[m,'ExperimentalCovarianceStatus']='Full 24-effect bootstrap covariance archived; primary RVE clusters StudyID; explicit covariance sensitivity supplied'
for name in ['analysis_ready_model_dataset.csv','analysis_ready_model_dataset_deduplicated.csv']: d.to_csv(root/'data/processed'/name,index=False)
print(result[['LatestModelRowID','OldLnRR','Estimate','OldVariance','Variance']].to_string(index=False))
