"""Rule coder A: non-blind semantic audit, not independent human coding."""
import csv
import re
from pathlib import Path

ROOT = Path('E:/ER_final_review_20260914')
def clean(x):
    return re.sub(r'\s+', ' ', x.strip().lower())

with (ROOT/'data/raw/boundary_sources/published_five_expert_scores.csv').open(encoding='utf-8-sig', newline='') as f:
    expert = list(csv.DictReader(f))
evidence = {}
for row in expert:
    # The source explicitly lists equivalent names separated by commas.
    for term in row['metric'].split(','):
        evidence.setdefault(clean(term), []).append((row['Final score'], row['metric']))

# Explicit biological success states, independent of rate/size proxy endpoints.
success = {'normal', 'normal embryos', 'survival', 'developmental success', 'hatching success',
           'hatch success', 'viability', 'egg production', 'fecundity', 'fertilization',
           'fertilization success', 'egg viability', 'viable egg rate', 'ovigerous female'}
direct = success | {'settlement', 'metamorphosis'}
synonyms = {'force of total byssal threads': 'force of byssal threads'}

# These mappings cannot be assumed equivalent from the metric labels alone.
questionable = {
 ('Biomechanics', 'Skeletal microdensity'),
 ('Development', 'calcified larvae'),
 ('Growth', 'Egg volume'),
 ('Growth', 'Arm length'),
 ('Growth', 'Larval size'),
 ('Growth', 'colony condition'),
 ('Growth', 'grow efficiency'),
 ('Growth', 'Organ:Flesh ratio'),
 ('Survival', 'Density'),
 ('Survival', 'Health score'),
}
with (ROOT/'data/source_data/Review_endpoint_dictionary.csv').open(encoding='utf-8-sig', newline='') as f:
    rows = list(csv.DictReader(f))
out = []
for i, row in enumerate(rows, 1):
    metric, target = clean(row['Metric']), clean(row['TransformedMetric'] or row['Metric'])
    lookup = synonyms.get(target, target)
    matches = evidence.get(lookup, [])
    votes = {s for s, _ in matches}
    rule = 'context_dependent'
    reason = 'No unambiguous published performance direction for the represented endpoint.'
    source = 'Endpoint labels; published_five_expert_scores.csv (no eligible consensus match)'
    flag = ''
    if votes == {'positive'}:
        rule = 'beneficial_when_increases'
        reason = 'Published unanimous positive endpoint classification; named aliases resolved.'
    elif votes == {'negative'}:
        rule = 'adverse_when_increases'
        reason = 'Published unanimous negative endpoint classification.'
    elif matches:
        reason = 'Published rating is ambiguous; no universal performance sign inferred.'
    elif target in success:
        rule = 'beneficial_when_increases'
        reason = 'Explicit successful development, reproduction or survival outcome.'
        source = 'Represented biological success endpoint; semantic rule, not expert consensus'
    if matches:
        source = 'published_five_expert_scores.csv: ' + '; '.join(sorted({m for _, m in matches}))
    direct_outcome = target in direct
    if (row['Category'], row['Metric']) in questionable and (target in success or rule != 'context_dependent'):
        rule = 'context_dependent'
        direct_outcome = False
        reason = 'Original endpoint and represented label are not demonstrably equivalent; source-level transformation evidence required before performance alignment.'
        source = 'Original/represented metric semantic mismatch; published consensus cannot validate transformation'
        flag = 'SOURCE_TRANSFORMATION_REQUIRED'
    if target == 'normal' and any(x in metric for x in ('abnormal', 'asym', 'assym', 'malformation')):
        flag = 'VERIFY_COMPLEMENT_OR_SIGN_TRANSFORMATION'
        reason += ' Decision is conditional on correct prior conversion of the adverse original outcome.'
    if target == 'survival' and metric == 'mortality':
        flag = 'VERIFY_COMPLEMENT_OR_SIGN_TRANSFORMATION'
        reason += ' Mortality must already have been converted; do not flip twice.'
    if target in {'settlement', 'metamorphosis'}:
        # A direct ecological event does not override ambiguous published direction.
        reason += ' Direct recruitment event, but timing/measurement context prevents universal alignment.'
    out.append(dict(ReviewItemID=f'END-{i:03d}', DirectionRule=rule,
                    ExcludedFromAligned=rule == 'context_dependent', DirectOutcome=direct_outcome,
                    Reason=reason, Evidence=source, Flag=flag))
with (ROOT/'coding_review/coder_a.csv').open('w', encoding='utf-8-sig', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=list(out[0]))
    writer.writeheader()
    writer.writerows(out)
print('Rule coder A records:', len(out))
for r in out:
    if r['Flag']:
        print(r['ReviewItemID'], r['Flag'])
