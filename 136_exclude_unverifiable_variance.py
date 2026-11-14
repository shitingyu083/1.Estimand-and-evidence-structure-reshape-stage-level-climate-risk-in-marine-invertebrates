"""Move records whose sampling variance cannot be verified out of the main analysis.

Garcia et al. (2015; StudyID 23) settlement records: the extracted standard
errors are smaller than the binomial standard error implied by the reported
means and the 135 larvae per treatment (e.g. control 5.74% settlement with
SE 0.20 percentage points, against about 2.0 points under a binomial count),
so the variances entered as (SE/mean)^2 cannot be reconciled with any
replicate unit. Tank-level data are not accessible. The six records are
removed from the analysis-ready datasets and written to a sidecar file that
the sensitivity analysis (R/133) restores. Larval records of the same study
are unaffected. The script is idempotent; R/128_settlement_source_corrections.py
does not modify these rows, so the two steps are order-independent.
"""
import os
from pathlib import Path

import pandas as pd

root = Path(os.environ.get('ER_ARCHIVE_ROOT', Path(__file__).resolve().parents[1]))
DATASETS = [root / 'data/processed/analysis_ready_model_dataset.csv',
            root / 'data/processed/analysis_ready_model_dataset_deduplicated.csv']
SIDECAR = root / 'data/source_data/Settlement_variance_unverifiable_StudyID23.csv'
EXCLUDE = [1077, 1083, 1086, 1095, 1096, 1097]
REASON = ('Sampling variance not verifiable: extracted SEs are smaller than the binomial SE '
          'implied by the means and n = 135 larvae; tank-level data not accessible')

removed = None
for path in DATASETS:
    d = pd.read_csv(path, low_memory=False)
    hit = d['LatestModelRowID'].isin(EXCLUDE)
    if hit.any():
        rows = d.loc[hit].copy()
        bad = rows[(rows['StudyID'] != 23) | (rows['ObservedStage'] != 'SETTLEMENT/METAMORPHOSIS')]
        if len(bad):
            raise SystemExit(f'Unexpected records selected for exclusion in {path.name}: {bad.LatestModelRowID.tolist()}')
        if hit.sum() != len(EXCLUDE):
            raise SystemExit(f'{path.name}: expected {len(EXCLUDE)} records, found {hit.sum()}')
        if removed is None:
            removed = rows
        d.loc[~hit].to_csv(path, index=False)

if removed is not None:
    removed.insert(0, 'ExclusionReason', REASON)
    removed.to_csv(SIDECAR, index=False)
if not SIDECAR.exists():
    raise SystemExit(f'Sidecar {SIDECAR.name} missing and records not found in the datasets')
print(f'{len(EXCLUDE)} StudyID 23 settlement records held out; sidecar {SIDECAR.relative_to(root)}')
