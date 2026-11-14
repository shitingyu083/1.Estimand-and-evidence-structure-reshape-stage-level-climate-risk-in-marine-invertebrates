"""Assemble a clean, checksummed review revision without stale submission exports."""
from pathlib import Path
import shutil,zipfile,hashlib,json,sys
ROOT=Path(__file__).resolve().parents[1]
pkg=Path(sys.argv[1]).resolve()
visual_revision=pkg.name in ['ER_visual_unified_revision_20260914','ER_pairwise_visual_revision_20260914']
archive=pkg/'data_and_code_archive_taxonomy_meta'
if archive.exists():
    assert (archive/'README_REPRODUCTION.md').exists(), 'Move prior assembly to a backup before packaging'
archive.mkdir(exist_ok=True)
def eligible(p):
    rel=p.relative_to(ROOT)
    if (pkg.name=='ER_source_verified_revision_20260914' or visual_revision) and p.name=='Human_adjudication_required_36_entries.csv':return False
    if any(x=='__pycache__' or x.startswith('document_build_') or x.startswith('legacy_exports') for x in rel.parts):return False
    if p.name in ['Rplots.pdf','16_rebuild_fig4.py','逐项修正报告_20260913.md','ER_manuscript_taxonomy_synced.txt','ER_manuscript.docx.txt','ER_supplementary_material_taxonomy_synced.txt','ER_supplementary_material.docx.txt']:return False
    if rel.parts[0]=='analysis_logs' and (p.suffix.lower() in ['.png','.rdata'] or p.name=='current_supplement.pdf'):return False
    return p.is_file()
for p in sorted(ROOT.rglob('*')):
    if eligible(p):
        dest=archive/p.relative_to(ROOT);dest.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(p,dest)
for folder in ['main','graphical_abstract']:
    dest=pkg/('main_figures' if folder=='main' else folder)
    dest.mkdir(exist_ok=True)
    for p in (ROOT/'figures'/folder).iterdir():
        if p.is_file():shutil.copy2(p,dest/p.name)
shutil.copy2(ROOT/'figures/extended/SupplementaryFigS2_complete_species_topology.pdf',pkg/'SupplementaryFigS2_complete_species_topology.pdf')
shutil.copy2(ROOT/'figures/extended/SupplementaryFigS6_exposure_adjusted_meta_regression.pdf',pkg/'SupplementaryFigS6_exposure_adjusted_meta_regression.pdf')
for name in ['study_scope_corrections_and_open_items.md','Boundary_issue_status.csv']:
    if (ROOT/name).exists():shutil.copy2(ROOT/name,pkg/name)
for name in ['README_REPRODUCTION.md']:shutil.copy2(ROOT/name,pkg/name)
if pkg.name=='ER_visual_unified_revision_20260914':
    shutil.copy2(ROOT/'可视化统一修订说明_20260914.md',pkg/'可视化统一修订说明_20260914.md')
if pkg.name=='ER_pairwise_visual_revision_20260914':
    shutil.copy2(ROOT/'配对森林图修订说明_20260914.md',pkg/'配对森林图修订说明_20260914.md')
if visual_revision and (ROOT/'data/source_data/All_meta_estimates_and_tests.xlsx').exists():
    shutil.copy2(ROOT/'data/source_data/All_meta_estimates_and_tests.xlsx',pkg/'All_meta_estimates_and_tests.xlsx')
for name in ['Review_independent_coding_record.csv','Review_independent_coding_summary.json','Review_independent_coding_effect_crosswalk.csv']:
    shutil.copy2(ROOT/'data/source_data'/name,pkg/name)
shutil.copy2(ROOT/'final_review_and_independent_check_notes.md',pkg/'final_review_and_independent_check_notes.md')
if pkg.name=='ER_human_endorsed_coding_review_20260914':
    shutil.copy2(ROOT/'data/source_data/Human_adjudication_required_36_entries.csv',pkg/'Human_adjudication_required_36_entries.csv')
    shutil.copy2(ROOT/'coding_review/dual_audit_itemwise_report.md',pkg/'dual_audit_itemwise_report.md')
if pkg.name=='ER_source_verified_revision_20260914' or visual_revision:
    shutil.copy2(ROOT/'data/source_data/Source_verification_resolutions_36_entries.csv',pkg/'Source_verification_resolutions_36_entries.csv')
def digest(p):
    h=hashlib.sha256()
    with p.open('rb') as f:
        for b in iter(lambda:f.read(1024*1024),b''):h.update(b)
    return h.hexdigest()
def manifest(folder):
    items=[p for p in sorted(folder.rglob('*')) if p.is_file() and p.name!='SHA256SUMS.txt' and not (folder==pkg and archive in p.parents)]
    (folder/'SHA256SUMS.txt').write_text('\n'.join(digest(p)+'  '+p.relative_to(folder).as_posix() for p in items)+'\n',encoding='utf-8')
manifest(archive)
datazip=pkg/'ER_data_and_code_archive_taxonomy_meta_final_review_20260914.zip'
with zipfile.ZipFile(datazip,'w',zipfile.ZIP_DEFLATED,compresslevel=6) as z:
    for p in sorted(archive.rglob('*')):
        if p.is_file():z.write(p,p.relative_to(archive).as_posix())
with zipfile.ZipFile(datazip) as z:
    assert z.testzip() is None
    assert not any(i.file_size==0 for i in z.infolist() if not i.is_dir())
    assert len([n for n in z.namelist() if n.startswith('figures/main/') and n.endswith('.pdf')])==6
manifest(pkg)
outer=pkg.parent/(pkg.name+'.zip' if pkg.name in ['ER_human_endorsed_coding_review_20260914','ER_source_verified_revision_20260914'] or visual_revision else 'ER_submission_final_review_20260914.zip')
with zipfile.ZipFile(outer,'w',zipfile.ZIP_DEFLATED,compresslevel=6) as z:
    for p in sorted(pkg.rglob('*')):
        if not p.is_file() or archive in p.parents:continue
        z.write(p,p.relative_to(pkg).as_posix())
with zipfile.ZipFile(outer) as z:assert z.testzip() is None
outer.with_suffix('.sha256').write_text(digest(outer)+'  '+outer.name+'\n',encoding='utf-8')
print(json.dumps({'submission_zip':str(outer),'bytes':outer.stat().st_size,'data_zip':str(datazip),'data_bytes':datazip.stat().st_size,'sha256':digest(outer)},indent=2))
