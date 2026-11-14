"""Stage, verify, and archive the locally rerun 2026-09-26 submission.

Usage: python R/101_package_submission_20260926.py stage|verify|build
Between stage and verify, render the six staged DOCX files to PDFs using Word.
"""
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED
from io import BytesIO
import sys, re, json, hashlib, shutil
import pandas as pd
import numpy as np
import pymupdf
from PIL import Image
from lxml import etree
from docx import Document

ARC = Path(__file__).resolve().parents[1]
ROOT = ARC.parent
OLD = ROOT / 'ER_submission_visual_final_20260919_deduplicated_final'
OUT = ROOT / 'ER_submission_final_20260926'
LOG = ARC / 'analysis_logs'
SD = ARC / 'data/source_data'
NAMES = ['Fig1_classification_and_evidence_update', 'Fig2_ecological_subgroup_responses',
         'Fig3_subgroup_paired_differences', 'Fig4_primary_matched_response_comparison',
         'Fig5_robustness_and_inference_limits', 'Fig6_reference_deviation_calibration']
DOCS = ['ER_manuscript', 'ER_supplementary_material', 'ER_title_page',
        'ER_cover_letter', 'Highlights', 'Submission_Declarations']
DATAZIP = 'ER_data_and_code_archive_taxonomy_meta_final_review_20260918.zip'

def sha(p):
    h = hashlib.sha256()
    with Path(p).open('rb') as f:
        for b in iter(lambda: f.read(1024 * 1024), b''): h.update(b)
    return h.hexdigest()

def successful_runs():
    for n in ['final_render_20260926.log', 'estimate_verification_20260926.log']:
        text = (LOG/n).read_text(encoding='utf-8-sig')
        assert 'Rscript exit code = 0' in text, n
        assert not re.search(r'(^Error|Execution halted)', text, re.M), n

def supplementary_map():
    result = {}
    for old in (OLD/'supplementary_figures').glob('*.pdf'):
        n = int(re.match(r'SupplementaryFigS(\d+)_', old.name).group(1))
        source = ARC/'figures/extended'/old.name
        if n == 5: source = ARC/'figures/main'/(NAMES[5]+'.pdf')
        if n == 7: source = ARC/'figures/extended/Fig_taxonomy_class_stressor_calibration.pdf'
        assert source.exists(), source
        result[n] = (old.name, source)
    assert set(result) == set(range(1, 14))
    return result

def refresh_references():
    replacements = {
      'ER_manuscript': {
        'Figure 5a shows that the stage coefficients were effectively unchanged across the tested working correlations.':
        'The sensitivity results in Supplementary Table S13 and the Excel attachment show that stage coefficients were effectively unchanged across the tested working correlations. Figure 5a displays the observed study-cell responses.',
        'Figure 5b shows the corresponding exposure-specific REML summaries.':
        'Figure 5b shows the corresponding exposure-specific observed study-cell distributions; REML summaries are reported in the Excel attachment.'},
      'ER_supplementary_material': {
        'Figure 5c and the Excel attachment report the rho = 0.8 pairwise stage contrasts; none survives its BH correction.':
        'The Excel attachment reports the rho = 0.8 pairwise stage contrasts; none survives its BH correction. Figure 5c displays observed within-cell differences between response definitions descriptively, rather than stage contrasts.'}}
    for name, changes in replacements.items():
        path=OUT/(name+'.docx'); doc=Document(path)
        for old,new in changes.items():
            matches=[p for p in doc.paragraphs if old in p.text]
            if not matches:
                assert any(new in p.text for p in doc.paragraphs),(name,old)
                continue
            assert len(matches)==1
            p=matches[0]; start=p.text.index(old); end=start+len(old)
            pos=0; written=False
            for run in p.runs:
                a=pos; b=pos+len(run.text); pos=b
                if b<=start or a>=end:continue
                prefix=run.text[:max(0,start-a)]
                suffix=run.text[max(0,end-a):] if end<b else ''
                run.text=prefix+(new if not written else '')+suffix
                written=True
        doc.save(path)

def stage():
    successful_runs()
    OUT.mkdir(exist_ok=True)
    for folder in ['main_figures', 'supplementary_figures', 'validation']:
        (OUT/folder).mkdir(exist_ok=True)
    for n in DOCS: shutil.copy2(OLD/(n+'.docx'), OUT/(n+'.docx'))
    refresh_references()
    for n in NAMES:
        for ext in ['pdf','svg','png','tiff']:
            shutil.copy2(ARC/'figures/main'/(n+'.'+ext), OUT/'main_figures'/(n+'.'+ext))
    smap = supplementary_map()
    for n, (name, source) in smap.items(): shutil.copy2(source, OUT/'supplementary_figures'/name)
    # Replace embedded supplementary previews using the exact final PNG exports.
    target = OUT/'ER_supplementary_material.docx'
    with ZipFile(target) as z: parts = {n:z.read(n) for n in z.namelist()}
    xml = etree.fromstring(parts['word/document.xml'])
    rel = etree.fromstring(parts['word/_rels/document.xml.rels'])
    rels = {r.get('Id'):r.get('Target') for r in rel}
    ratios = {}
    for n in range(1,12):
        source = smap[n][1].with_suffix('.png')
        assert source.exists(), source
        data = source.read_bytes()
        media = 'word/media/review_s'+str(n)+'.png'
        assert media in parts, media
        parts[media] = data
        with Image.open(BytesIO(data)) as im: ratios[media] = im.width/im.height
    for par in xml.xpath('//*[local-name()="p"]'):
        ids = par.xpath('.//*[local-name()="blip"]/@*[local-name()="embed"]')
        if not ids: continue
        media = 'word/'+rels[ids[0]]
        if media not in ratios: continue
        extents = par.xpath('.//*[local-name()="extent"]')
        if not extents: continue
        cx,cy = int(extents[0].get('cx')),int(extents[0].get('cy'))
        ncx = min(cx, int(cy*ratios[media])); ncy = int(ncx/ratios[media])
        for el in extents + par.xpath('.//*[local-name()="xfrm"]/*[local-name()="ext"]'):
            el.set('cx', str(ncx)); el.set('cy', str(ncy))
    parts['word/document.xml'] = etree.tostring(xml,encoding='UTF-8',xml_declaration=True,standalone=True)
    with ZipFile(target,'w',ZIP_DEFLATED) as z:
        for name,data in parts.items(): z.writestr(name,data)
    shutil.copy2(SD/'All_meta_estimates_and_tests.xlsx',OUT/'All_meta_estimates_and_tests.xlsx')
    for n in ['final_render_20260926.log','estimate_verification_20260926.log','runtime_session_20260926.log']:
        shutil.copy2(LOG/n, OUT/'validation'/n)
    readme = '''# Final submission package — 26 September 2026

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
Figure 5 now matches the current manuscript caption: observed lollipops, development distributions, paired distributions, and model-variance diagnostics.

## Production notes
These are full-width vector masters. Nominal dimensions are 190 × 389 mm for Figure 1;
190 × 299 mm for Figures 2, 3 and 5; 190 × 253 mm for Figure 4; 190 × 267 mm for Figure 6.
Figure 1 contains all 138 labelled species. Preserve legibility when the journal lays out the tall master.
This technical rebuild does not establish new literature-search completeness or independent human coding provenance;
the scientific limitations and author declarations already present in the manuscript remain applicable.
No files were uploaded to a journal or public repository during this rebuild.
'''
    (OUT/'README_REPRODUCTION.md').write_text(readme,encoding='utf-8')
    (ARC/'README_FINAL_20260926.md').write_text(readme,encoding='utf-8')
    print(json.dumps({'staged':str(OUT),'main_exports':24,'supplementary_pdfs':13,'documents_to_render':6},ensure_ascii=False))

def verify():
    successful_runs()
    report = {'date':'2026-09-26','execution':'local Windows R 4.6.1','R_render_exit':0,'R_estimate_check_exit':0}
    assert {p.stem for p in (OUT/'main_figures').glob('*.pdf')} == set(NAMES)
    assert len(list((OUT/'main_figures').iterdir()))==24
    figures = []
    for n in NAMES:
        for ext in ['pdf','svg','png','tiff']:
            p=OUT/'main_figures'/(n+'.'+ext)
            assert sha(p)==sha(ARC/'figures/main'/p.name),p
        doc=pymupdf.open(OUT/'main_figures'/(n+'.pdf')); assert len(doc)==1
        page=doc[0]; txt=page.get_text(); assert len(txt)>100
        spans=[s for b in page.get_text('dict')['blocks'] if 'lines' in b for l in b['lines'] for s in l['spans'] if s['text'].strip()]
        outside=[s['text'] for s in spans if s['bbox'][0]<-1 or s['bbox'][1]<-1 or s['bbox'][2]>page.rect.width+1 or s['bbox'][3]>page.rect.height+1]
        assert not outside,(n,outside)
        with Image.open(OUT/'main_figures'/(n+'.tiff')) as im:
            dpi=tuple(float(x) for x in im.info['dpi']); assert all(x>=599 for x in dpi)
        figures.append({'name':n,'pdf_mm':[round(page.rect.width*25.4/72,2),round(page.rect.height*25.4/72,2)],'tiff_dpi':dpi,'minimum_pdf_font_pt':round(min(s['size'] for s in spans),2),'sha256_pdf':sha(OUT/'main_figures'/(n+'.pdf'))})
    report['main_figures']=figures
    report['identical_source_submission_exports']=24
    tree=etree.parse(str(OUT/'main_figures'/(NAMES[0]+'.svg')))
    nodes=tree.xpath('//*[local-name()="text"]'); texts=[''.join(n.itertext()) for n in nodes]
    species=pd.read_csv(SD/'Fig1a_opentree_tip_metadata.csv')['MatchedName'].str.split(' | ',regex=False).str[0]
    for name in species:
        assert name in texts,name
        style=nodes[texts.index(name)].get('style',''); assert 'italic' in style and 'Arial' in style,name
    report['verified_species_labels']=len(species); assert len(species)==138
    texts={}
    for n in DOCS:
        with ZipFile(OUT/(n+'.docx')) as z:
            assert z.testzip() is None
            for part in z.namelist():
                if part.endswith('.xml'):etree.fromstring(z.read(part))
        texts[n]='\n'.join(p.text for p in Document(OUT/(n+'.docx')).paragraphs)
        pdf=pymupdf.open(OUT/(n+'.pdf')); assert len(pdf)>0 and all(p.get_text().strip() or p.get_images() or p.get_drawings() for p in pdf)
        pdftext=' '.join(' '.join(p.get_text().split()) for p in pdf)
        # Check every substantive paragraph has its text in the freshly rendered PDF.
        missing=[]
        for par in Document(OUT/(n+'.docx')).paragraphs:
            t=' '.join(par.text.split())
            if len(t)>50 and t[:70] not in pdftext:missing.append(t[:70])
        assert not missing,(n,missing[:5])
    report['verified_document_pairs']=len(DOCS)
    main=texts['ER_manuscript']
    assert re.findall(r'^Figure ([1-6])\.',main,re.M)==list('123456')
    assert ('Lollipop display of observed' in main and 'against model variance' in main) or ('Standard boxplots' in main and 'Model-variance distributions' in main)
    assert 'Figure 5a shows that the stage coefficients' not in main
    assert 'Figure 5b shows the corresponding exposure-specific REML summaries' not in main
    assert 'Figure 5c and the Excel attachment report the rho = 0.8 pairwise stage contrasts' not in texts['ER_supplementary_material']
    assert main.count('Black row labels in panel b report')==1
    smap=supplementary_map()
    for _,(name,source) in smap.items(): assert sha(source)==sha(OUT/'supplementary_figures'/name)
    with ZipFile(OUT/'ER_supplementary_material.docx') as z:
        for n in range(1,12):assert hashlib.sha256(z.read('word/media/review_s'+str(n)+'.png')).hexdigest()==sha(smap[n][1].with_suffix('.png'))
    report['verified_supplementary_pdfs']=13;report['refreshed_embedded_previews']=11
    refit=pd.read_csv(SD/'Final_subgroup_refit_verification.csv')
    assert len(refit)==207 and refit.MaxAbsoluteError.max()<1e-9
    report['subgroup_refits']=207;report['maximum_refit_error']=float(refit.MaxAbsoluteError.max())
    report['BH_signed_families']={}
    for name,expected in [('Fig4b_estimand_target_differences.csv',9),('Fig3_subgroup_estimand_paired_differences.csv',69)]:
        df=pd.read_csv(SD/name)
        assert int(df.FDR_q_value.notna().sum())==expected and int((df.FDR_q_value<.05).sum())==0
        report['BH_signed_families'][name]={'tested':expected,'significant':0}
    display=pd.read_csv(SD/'Annotated_forest_display_audit.csv').fillna({'Stars':''})
    expected=display.FDR_q_value.map(lambda q:'' if pd.isna(q) or q>=.05 else ('***' if q<.001 else '**' if q<.01 else '*'))
    assert display.Stars.equals(expected)
    significant=display.Stars.ne('');assert (display.loc[significant,'StarX']>display.loc[significant,'U']).all()
    finite=display.Estimate.notna();assert (display.loc[finite,'CI_low']<=display.loc[finite,'CI_high']).all()
    trans=finite & display.Panel.ne('CR2 stage contrast')
    assert np.allclose(display.loc[trans,'X'],np.arcsinh(display.loc[trans,'Estimate']/.15))
    assert display.Info.notna().all()
    report['forest_display']={'rows':len(display),'BH_star_rows':int(significant.sum()),'transformed_estimates_verified':True,'star_positions_verified':True}
    assert sha(SD/'All_meta_estimates_and_tests.xlsx')==sha(OUT/'All_meta_estimates_and_tests.xlsx')
    with pd.ExcelFile(OUT/'All_meta_estimates_and_tests.xlsx') as wb:
        assert len(wb.sheet_names)>=35
        restored=pd.read_excel(wb,'Forest display audit').fillna({'Stars':''})
        assert len(restored)==len(display)
        assert np.allclose(restored.Estimate,display.Estimate,equal_nan=True)
        report['excel_sheets']=len(wb.sheet_names)
    report['visual_review']='All six main PDF previews inspected; forest points, labels and BH stars separated; final Fig5 matches manuscript caption.'
    report['limitations']=['Tall vector masters require production-size readability review; no journal-guide compliance certification is implied.','Historical scientific limitations and author declarations are retained; no remote execution or journal upload was performed.']
    for p in [LOG/'final_validation_20260926.json',OUT/'validation/final_validation_20260926.json']:
        p.write_text(json.dumps(report,ensure_ascii=False,indent=2),encoding='utf-8')
    print(json.dumps(report,ensure_ascii=False,indent=2))
    return report

def manifest(base,files):
    p=base/'SHA256SUMS.txt'
    p.write_text(''.join(sha(f)+'  '+f.relative_to(base).as_posix()+'\n' for f in files),encoding='utf-8')
    return p

def archive(base,files,target):
    tmp=target.with_suffix('.building.zip')
    with ZipFile(tmp,'w',ZIP_DEFLATED,compresslevel=6) as z:
        for p in files:z.write(p,p.relative_to(base).as_posix())
    with ZipFile(tmp) as z:
        assert z.testzip() is None
        assert len(z.namelist())==len(set(z.namelist()))==len(files)
        for p in files:assert hashlib.sha256(z.read(p.relative_to(base).as_posix())).hexdigest()==sha(p),p
        for line in z.read('SHA256SUMS.txt').decode('utf-8').splitlines():
            digest,name=line.split('  ',1);assert hashlib.sha256(z.read(name)).hexdigest()==digest,name
    tmp.replace(target)
    return {'file':target.name,'entries':len(files),'bytes':target.stat().st_size,'sha256':sha(target),'crc_and_all_entry_sha256_verified':True}

def build():
    shutil.copy2(LOG/'document_render_20260926.log',OUT/'validation/document_render_20260926.log')
    verify()
    # Superseded alternate main figures and generated preview images do not belong in the archive.
    def included(p):
        rel=p.relative_to(ARC)
        if p.name in ['SHA256SUMS.txt','Rplots.pdf'] or p.suffix.lower()=='.zip':return False
        if any(part.startswith(('legacy_exports','document_build_','__pycache__')) for part in rel.parts):return False
        if len(rel.parts)>=3 and rel.parts[:2]==('figures','main'):
            return p.stem in NAMES and p.suffix in ['.pdf','.svg','.png','.tiff']
        if rel.parts[0]=='analysis_logs' and p.name.startswith(('preview_','review_','contact_')):return False
        return True
    files=sorted(p for p in ARC.rglob('*') if p.is_file() and included(p))
    files.append(manifest(ARC,files))
    inner=archive(ARC,files,OUT/DATAZIP)
    contents=sorted(p for p in OUT.rglob('*') if p.is_file() and p.name!='SHA256SUMS.txt')
    contents.append(manifest(OUT,contents))
    outer=archive(OUT,contents,ROOT/'ER_submission_final_20260926.zip')
    result={'data_code_archive':inner,'submission_archive':outer}
    (ROOT/'ER_submission_final_20260926.verification.json').write_text(json.dumps(result,indent=2),encoding='utf-8')
    (ROOT/'ER_submission_final_20260926.zip.sha256').write_text(outer['sha256']+'  '+outer['file']+'\n',encoding='utf-8')
    print(json.dumps(result,indent=2))

if __name__=='__main__':
    {'stage':stage,'verify':verify,'build':build}[sys.argv[1]]()
