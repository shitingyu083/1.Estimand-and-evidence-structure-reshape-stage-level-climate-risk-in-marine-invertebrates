from pathlib import Path
from zipfile import ZipFile
import sys,re,json
import pymupdf as fitz
from docx import Document

pkg=Path(sys.argv[1]); root=Path(__file__).resolve().parents[1]
out=root/'analysis_logs/document_previews';out.mkdir(parents=True,exist_ok=True)
rows=[]
for f in sorted(pkg.glob('*.docx')):
    with ZipFile(f) as z: assert z.testzip() is None
    d=Document(f); text='\n'.join(p.text for p in d.paragraphs)
    assert '13 of 36' not in text and '36 estimable contrasts' not in text
    if f.name in ['ER_manuscript.docx','ER_title_page.docx','ER_cover_letter.docx']:
        assert 'Stage-level responses to ocean warming and acidification depend on effect definitions and evidence structure' in text
    pdf=f.with_suffix('.pdf'); doc=fitz.open(pdf)
    for i in [0,len(doc)-1]:doc[i].get_pixmap(dpi=100).save(out/f'{f.stem}_page{i+1}.png')
    rows.append(dict(Document=f.name,Pages=len(doc),Paragraphs=len(d.paragraphs),Tables=len(d.tables),Images=len(d.inline_shapes)))
    if f.name=='ER_manuscript.docx':
        assert 'across 27 estimable contrasts' in text
        assert 'StudyID 244' in text
        assert 'ER_data_and_code_archive_taxonomy_meta_final_review_20260918.zip' in text
        for n in range(1,7):assert f'Figure {n}.' in text
        assert '2,391' in text and '2,214' in text
        for obsolete in ['2,380 effects','2,204 effects','13 of 27','216 estimable contrasts','80 survive correction','five effects met']:
            assert obsolete not in text, obsolete
        latest=json.loads((root/'data/source_data/Boundary_manuscript_summary.json').read_text())
        assert f"{latest['paired_significant']} of {latest['paired_tests']}" in text
        assert f"{latest['subgroup_significant']} survive correction" in text
        assert 'none of the effects met the source-based alignment rules' in text
        paragraphs=[p.text for p in d.paragraphs]
        a=paragraphs[paragraphs.index('Abstract')+1]
        rows[-1]['AbstractWords']=len(a.split())
        summary=json.loads((root/'data/source_data/Boundary_manuscript_summary.json').read_text())
        assert rows[-1]['AbstractWords']==summary['abstract_words']
    if f.name=='ER_supplementary_material.docx':
        assert len(d.inline_shapes)==11
        for n in range(1,20):assert f'Supplementary Table S{n}.' in text
        assert 'Endpoint-coding verification and provenance' in text
(root/'analysis_logs/document_quality_checks.json').write_text(json.dumps(rows,indent=2),encoding='utf-8')
print(json.dumps(rows,indent=2))
