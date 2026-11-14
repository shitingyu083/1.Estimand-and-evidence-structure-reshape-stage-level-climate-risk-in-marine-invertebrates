from pathlib import Path
import sys
sys.stdout.reconfigure(encoding='utf-8')
import pandas as pd, io
root=Path(__file__).resolve().parents[1]
d=pd.read_csv(root/'data/processed/analysis_ready_model_dataset.csv')
ids=[362,685,688,695,1538,1540,1778,1816,2204,2228,81]
print(d.loc[d.LatestModelRowID.isin(ids),['LatestModelRowID','StudyID','OriginalSourceRows','OriginalFileRows','SourceDataset','SourceRow','Metric','TransformedMetric','lnRR','Variance']].to_string(index=False))
print('STUDY244',d.loc[d.StudyID.eq(244),['LatestModelRowID','SpeciesID','Metric','TransformedMetric','StressorLabel','ControlN','TreatmentN','ControlMean','TreatmentMean']].to_string(index=False))
raw=pd.read_csv(io.StringIO(Path('E:/ER_boundary_audit_20260913/sources/pangaea973022').read_text(encoding='utf-8').split('*/',1)[1].strip()),sep='\t')
print('RAW244',raw.shape,raw.columns.tolist())
print(raw[['Treat','Species UID','ID (Header)','ID (Tank)']].drop_duplicates().head(16).to_string(index=False))
print('METRICMAP')
x=pd.read_csv('E:/ER_boundary_audit_20260913/published_expert_dictionary_crosswalk.csv')
print(x.loc[x.PublishedScore.isin(['positive','negative']),['Metric','TransformedMetric','CurrentRule','PublishedScore','Effects']].to_string(index=False))
recover=pd.read_csv('E:/ER_boundary_audit_20260913/candidate_metadata_recovery_exact_match.csv')
print(recover.loc[recover.LatestModelRowID.isin([81,82,362]),['LatestModelRowID','SourceLocators','MatchedRows']].to_string(index=False))
