from pathlib import Path
from urllib.request import Request,urlopen
from urllib.parse import urlencode
import base64,json,hashlib,re
root=Path(__file__).resolve().parents[1]; dest=root/'data/raw/boundary_sources'
entry='7adc7b14-abae-4ab9-b60b-b9b6e0e9f320'
paths=['/Carbonate_chemistry/CO2calc_chemistry.csv','/Experimental_Measurements/Behavioural_activity.csv','/Experimental_Measurements/Ecosystem_process_&_functioning.csv','/Macrofauna_morphology/Macrofaunal_final_morphology.csv']
manifest=[]
for p in paths:
    ident='synth:'+entry+':'+base64.b64encode(p.encode()).decode()
    url='https://ramadda.data.bas.ac.uk/repository/entry/get'+p.rsplit('/',1)[-1]+'?'+urlencode({'entryid':ident})
    # The conventional RAMADDA /entry/get?entryid= route needs no filename.
    url='https://ramadda.data.bas.ac.uk/repository/entry/get?'+urlencode({'entryid':ident})
    try:
        with urlopen(Request(url,headers={'User-Agent':'Mozilla/5.0'}),timeout=35) as f:b=f.read()
        assert b'<html' not in b[:1000].lower(), 'Repository returned HTML instead of data'
        name='Williams2024_'+p.rsplit('/',1)[-1].replace('&','and')
        (dest/name).write_bytes(b)
        manifest.append({'url':url,'file':name,'sha256':hashlib.sha256(b).hexdigest(),'bytes':len(b)})
    except Exception as e: manifest.append({'url':url,'error':str(e)})
(root/'data/source_data/Boundary_Williams_download_manifest.json').write_text(json.dumps(manifest,indent=2))
print(json.dumps(manifest,indent=2))
