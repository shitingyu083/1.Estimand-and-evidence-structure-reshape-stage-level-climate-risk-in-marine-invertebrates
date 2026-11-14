from pathlib import Path
import fitz
import pandas as pd
ROOT=Path(__file__).resolve().parents[1]
out=ROOT/'analysis_logs/figure_previews'
out.mkdir(parents=True,exist_ok=True)
rows=[]
for f in sorted((ROOT/'figures/main').glob('*.pdf')):
    doc=fitz.open(f)
    page=doc[0]
    page.get_pixmap(dpi=150).save(out/(f.stem+'.png'))
    spans=[s for b in page.get_text('dict')['blocks'] if 'lines' in b for l in b['lines'] for s in l['spans'] if s['text'].strip()]
    for s in spans:
        rows.append(dict(Figure=f.name,Text=s['text'],FontSize=s['size'],Font=s['font'],Small=s['size']<6))
pd.DataFrame(rows).to_csv(out/'font_audit.csv',index=False)
print(pd.DataFrame(rows).groupby('Figure').FontSize.min())
