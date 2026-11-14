"""Descriptive agreement of two non-blinded rule audits, not human reliability."""
from pathlib import Path
import json, hashlib
import pandas as pd

root=Path(__file__).resolve().parent
ref=pd.read_csv(root.parent/'data/source_data/Review_endpoint_dictionary.csv')
ref.insert(0,'ReviewItemID',[f'END-{i:03d}' for i in range(1,len(ref)+1)])
fields=['DirectionRule','ExcludedFromAligned','DirectOutcome']
frames={k:pd.read_csv(root/f'coder_{k}.csv',keep_default_na=False) for k in ['a','b']}
for k,d in frames.items():
    assert len(d)==266 and d.ReviewItemID.is_unique
    assert set(d.ReviewItemID)==set(ref.ReviewItemID)
    assert not d[fields].astype(str).eq('').any().any()
merged=ref.merge(frames['a'],on='ReviewItemID',suffixes=('_reference','_a'),validate='one_to_one')
merged=merged.merge(frames['b'].rename(columns={c:c+'_b' for c in frames['b'] if c!='ReviewItemID'}),on='ReviewItemID',validate='one_to_one')
def metrics(x,y):
    x=x.astype(str).str.lower();y=y.astype(str).str.lower()
    levels=sorted(set(x)|set(y)); po=float((x==y).mean())
    pe=sum(float((x==v).mean())*float((y==v).mean()) for v in levels)
    return {'n':len(x),'agreements':int((x==y).sum()),'agreement':po,'expected_agreement':pe,'kappa':(po-pe)/(1-pe) if pe<1 else None,'levels':levels}
rows=[]
for field in fields:
    for one,two in [('a','b'),('a','reference'),('b','reference')]:
        rows.append({'field':field,'comparison':one+' vs '+two,**metrics(merged[field+'_'+one],merged[field+'_'+two])})
    merged[field+'_audit_disagreement']=merged[field+'_a'].astype(str).str.lower()!=merged[field+'_b'].astype(str).str.lower()
    for k in ['a','b']:
        merged[field+'_'+k+'_reference_difference']=merged[field+'_'+k].astype(str).str.lower()!=merged[field+'_reference'].astype(str).str.lower()
merged.to_csv(root/'itemwise_dual_audit_results.csv',index=False,encoding='utf-8-sig')
flags=[c for c in merged if c.endswith('_audit_disagreement') or c.endswith('_reference_difference')]
merged.loc[merged[flags].any(axis=1)].to_csv(root/'coding_discrepancy_entries.csv',index=False,encoding='utf-8-sig')
pd.DataFrame(rows).to_csv(root/'agreement_statistics.csv',index=False,encoding='utf-8-sig')
summary={'audit_type':'Two non-blinded rule audits; not independent human duplicate coding','unit':'266 endpoint dictionary entries, unweighted by effect count','no_human_agreement_estimated':True,'results':rows,'input_sha256':{k:hashlib.sha256((root/f'coder_{k}.csv').read_bytes()).hexdigest() for k in frames}}
(root/'agreement_summary.json').write_text(json.dumps(summary,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps(summary,ensure_ascii=False,indent=2))
