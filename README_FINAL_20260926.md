# Final submission package — boxplot revision, 26 September 2026

This package was rebuilt from a successful local R run on Windows. No remote-server run is claimed.
Use the files in this folder; older packages and intermediate exports are not included.

## Upload inventory
- Six documents, each in DOCX and PDF: manuscript, supplementary material, title page, cover letter, highlights, declarations.
- main_figures: exactly six figure families, each in PDF, SVG, PNG and 600-dpi TIFF.
- supplementary_figures: 13 standalone PDFs (S1–S13). S5 reproduces final Figure 6.
- All_meta_estimates_and_tests.xlsx: refreshed numerical source tables and display audit.
- Data/code ZIP: the historical filename is retained to match document citations, but its contents and manifest were rebuilt on 26 September 2026.
- validation: actual R logs and the numerical/file consistency report.
- SHA256SUMS.txt: checksums of every payload file in this package.

## Reproduction
Extract the data/code ZIP and work from its root. The final figure entry point is R/97_annotated_evidence_forests.R.
It calls R/93_submission_visual_final.R for baseline panels; R/83_distribution_composites.R supplies the final descriptive Figure 5;
R/100_three_question_synthesis.R extends Figure 6; R/98_annotated_supplementary_forests.R refreshes supplementary forests.
Script 93 alone is not the complete six-figure final entry point.

On this Windows installation, invalid inherited C.UTF-8 locale settings prevented existing R packages from loading.
Set LC_ALL and LANG to English_United States.utf8 before launching R, and set R_LIBS_USER to the installed package library.
Set ER_ARCHIVE_ROOT to the extracted archive directory, then run:

    Rscript --vanilla R/97_annotated_evidence_forests.R
    Rscript --vanilla R/94_verify_final_estimates.R
    python R/78_make_all_estimates_workbook.py

Actual R version, package versions and library paths are recorded in validation/runtime_session_20260926.log.
No model estimates or BH correction rules were changed. The signed paired families remain 0/9 and 0/69 significant.
The 207 subgroup REML estimates and confidence intervals were independently refitted from the archived study cells.
Small forest points are the corresponding observed study cells; black numeric labels occupy a separate area.
Figure 5 uses original-unit quartiles and 1.5-IQR boxplots. The nine signed paired tests are labelled ns; all nine signed IQRs are zero. Folded contrasts are descriptive; model variance is a diagnostic and is not assigned a significance test. Figure 6e uses a compact point-and-interval display with symbols for inference status. No new tests were introduced.

## Production notes
These are full-width vector masters. Nominal dimensions are 190 × 389 mm for Figure 1;
190 × 299 mm for Figures 2 and 3; 190 × 305 mm for Figure 5; 190 × 253 mm for Figure 4; 190 × 267 mm for Figure 6.
Figure 1 contains all 138 labelled species. Preserve legibility when the journal lays out the tall master.
This technical rebuild does not establish new literature-search completeness or independent human coding provenance;
the scientific limitations and author declarations already present in the manuscript remain applicable.
No files were uploaded to a journal or public repository during this rebuild.

This package supersedes the earlier 2026-09-26 visual package. R/106_refresh_boxplots_and_synthesis.R incrementally refreshes Figures 5 and 6; script 97 remains the full entry point. The Excel file contains dedicated boxplot statistics, paired-test and graphical-summary audit sheets.
