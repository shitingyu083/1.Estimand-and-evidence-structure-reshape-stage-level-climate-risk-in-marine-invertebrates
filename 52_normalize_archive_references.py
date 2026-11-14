"""Mechanical archive naming synchronization; preserve raw document templates."""
from pathlib import Path
root=Path(__file__).resolve().parents[1]
for p in [root/'build_revision_documents.ps1',root/'README_REPRODUCTION.md']:
    t=p.read_text(encoding='utf-8-sig')
    t=t.replace('ER_data_and_code_archive_taxonomy_meta_review_revision_20260913.zip','ER_data_and_code_archive_taxonomy_meta_boundary_revision_20260913.zip')
    p.write_text(t,encoding='utf-8')
