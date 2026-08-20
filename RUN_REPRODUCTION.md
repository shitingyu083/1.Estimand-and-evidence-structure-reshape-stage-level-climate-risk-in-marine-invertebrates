# Reproduction guide

This archive starts from the disclosed analysis-ready effect-size dataset and contains the scripts, result tables and figure source data used for the submitted manuscript.

## Requirements

- R >= 4.3 with: readr, dplyr, tidyr, stringr, forcats, ggplot2, patchwork, metafor, robumeta, scales, ragg and svglite.
- Python >= 3.10 with: numpy, pandas, scipy, scikit-learn and matplotlib.

1. From the archive root, reproduce the REML models, endpoint-direction coding, RVE analyses, estimand sensitivities and Figs. 2-4:
   `Rscript run_all.R`

2. Reproduce the Fig. 1 schematic and evidence-structure panels:
   `python R/05_fig1_schematic.py`

3. Reproduce the reference-deviation calibration and study-held-out machine-learning outputs underlying Fig. 5:
   `python R/09_reference_deviation_ml.py`

The R workflow starts from data/processed/analysis_ready_model_dataset.csv. The Fig. 1 script uses the archived figure source tables and processed evidence-balance table. The evidence-base search and extraction trail is retained in the processed screening and update files but is not reconstructed by run_all.R.

The Python workflow starts from the same analysis-ready dataset and reproduces the null-calibrated reference-deviation tables, grouped cross-validation, Fig. 5 and its reproducibility log. Supplementary Fig. S5 is a direct visualization of the resulting tables.