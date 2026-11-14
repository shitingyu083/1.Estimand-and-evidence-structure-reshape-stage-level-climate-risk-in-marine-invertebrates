"""Current visual checks; supersedes the removed Figure 3c matrix checks."""
from pathlib import Path
import runpy,sys
if len(sys.argv)!=2:
    raise SystemExit('Usage: python R/39_visual_request_checks.py PACKAGE_DIRECTORY')
runpy.run_path(str(Path(__file__).with_name('71_visual_revision_checks.py')),run_name='__main__')
