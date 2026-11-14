"""Code the represented endpoint, never flip an already transformed outcome twice."""
from pathlib import Path
import pandas as pd, numpy as np, re
root=Path(__file__).resolve().parents[1]
d=pd.read_csv(root/'data/processed/analysis_ready_model_dataset_deduplicated.csv')
scores=pd.read_csv(root/'data/raw/boundary_sources/published_five_expert_scores.csv')
def norm(v): return re.sub(r'\s+',' ',str(v).strip().lower())
lookup={}
for _,r in scores.iterrows(): lookup.setdefault(norm(r.metric),set()).add(norm(r['Final score']))
direct={'normal','survival','developmental success','hatching success','hatch success','viability','fertilization success','metamorphosis success','egg production','fecundity'}
# Source-verification revision, 14 September 2026; original effect values retained.
# Harmonized names alone cannot override original-metric ambiguity or units.
unsupported={('Biomechanics','Microhardness'),('Biomechanics','Skeletal microdensity'),
 ('Development','calcified larvae'),('Growth','Arm length'),('Growth','Egg volume'),
 ('Growth','Tissue biomass'),('Growth','Tissue production'),('Growth','change in volume'),
 ('Survival','Density'),('Survival','Health score')}
aliases={'condition index':'condition factor, fulton index, condition index',
 'condition factor':'condition factor, fulton index, condition index',
 'force of total byssal threads':'force of byssal threads'}
out=[]
for _,r in d.iterrows():
    target=norm(r.TransformedMetric) if pd.notna(r.TransformedMetric) and norm(r.TransformedMetric) else norm(r.Metric)
    consensus=lookup.get(target,set())
    rule='context_dependent'; basis='No source-backed direction for represented endpoint'
    if consensus=={'positive'}:
        rule='beneficial_when_increases'; basis='Published five-expert consensus for represented endpoint'
    elif consensus=={'negative'}:
        rule='adverse_when_increases'; basis='Published five-expert consensus for represented endpoint'
    elif consensus:
        basis='Published ambiguous or inconsistent ratings; excluded'
    elif target in direct:
        rule='beneficial_when_increases'; basis='Explicit represented success/survival endpoint; source transformation preserved'
    if (str(r.Category),str(r.Metric)) in unsupported:
        rule='context_dependent';basis='Source audit: original-metric ambiguity or unsupported harmonization; excluded';consensus=set()
    original=norm(r.Metric)
    if original in aliases and lookup.get(aliases[original],set())=={'positive'}:
        rule='beneficial_when_increases';basis='Published five-expert consensus with source-verified metric alias';consensus={'positive'}
    if original=='normal embryonic development' and target=='normal embryos':
        rule='beneficial_when_increases';basis='Explicit represented normal-embryo proportion, PANGAEA 964219';consensus=set()
    if original in {'arm assymetry','arm asymmetry','asymmetry'} and target=='normal':
        basis='Source database explicitly inverted adverse asymmetry ratio; inherited orientation, not normal proportion';consensus=set()
    out.append({'LatestModelRowID':r.LatestModelRowID,'StudyID':r.StudyID,'Metric':r.Metric,'TransformedMetric':r.TransformedMetric,'RepresentedEndpoint':target,'SourceDirectionRule':rule,'DirectionEvidence':basis,'PublishedConsensus':';'.join(sorted(consensus)), 'ExternalConsensusEligible':basis.startswith('Published five'), 'IndependentHumanReview':'Human endorsement of rule audit reported; see S17'})
out=pd.DataFrame(out)
out.to_csv(root/'data/source_data/Boundary_endpoint_source_rules.csv',index=False)
print(out.groupby(['SourceDirectionRule','DirectionEvidence']).size().to_string())
