from pathlib import Path
import sys
sys.stdout.reconfigure(encoding='utf-8')
from bs4 import BeautifulSoup
import re,json
root=Path(__file__).resolve().parents[1]
t=(root/'data/raw/boundary_sources/alter_paper_text.txt').read_text(encoding='utf-8')
quotes=[]
for term in ['mortality','transformed','independently','Google Scholar','100 results','January 2022','acclimation']:
    for m in list(re.finditer(re.escape(term),t,re.I))[:5]:
        quotes.append({'term':term,'context':t[max(0,m.start()-240):m.end()+600],'source':'https://doi.org/10.1038/s41467-024-47064-3'})
(root/'data/source_data/Boundary_published_methods_extracts.json').write_text(json.dumps(quotes,ensure_ascii=False,indent=2),encoding='utf-8')
for q in quotes:
    if q['term'] in ['mortality','transformed']: print(q)
soup=BeautifulSoup(Path('E:/ER_boundary_audit_20260913/sources/williams_repository.html').read_text(encoding='utf-8'),'html.parser')
print('BAS FILE LINKS')
for a in soup.find_all('a',href=True):
    if any(x in (a['href']+' '+a.get_text()).lower() for x in ['csv','zip','download','chemistry','readme','folder']): print(a.get_text(' ',strip=True),a['href'])
