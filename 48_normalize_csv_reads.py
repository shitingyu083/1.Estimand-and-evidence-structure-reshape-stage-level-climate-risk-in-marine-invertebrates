"""Mechanical readr guard: inspect all rows when inferring sparse metadata types."""
from pathlib import Path
import re
root=Path(__file__).resolve().parents[1]
changed=[]
for p in (root/'R').glob('*.R'):
    t=p.read_text(encoding='utf-8-sig')
    new=re.sub(r'\bread_csv\((?!guess_max=100000, )','read_csv(guess_max=100000, ',t)
    if new!=t: p.write_text(new,encoding='utf-8');changed.append(p.name)
print('Normalized readr inference:',changed)
