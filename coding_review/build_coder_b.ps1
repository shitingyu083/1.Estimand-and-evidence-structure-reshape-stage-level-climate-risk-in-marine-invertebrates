$root = 'E:/ER_final_review_20260914'
$items = Import-Csv -LiteralPath "$root/data/source_data/Review_independent_coding_record.csv" | Select-Object ReviewItemID,Category,Metric,TransformedMetric
$expert = Import-Csv -LiteralPath "$root/data/raw/boundary_sources/published_five_expert_scores.csv"
# Agent B decisions from endpoint semantics and supplied published consensus.
# This is a non-blind rule audit, not a historical independent human coding record.
$positive = @(36,37,38,39,40,41,42,44,46,47,48,72,82,86,87,88,89,90,98,105,113,118,119,120,121,123,128,138,152,156,157,158,163,166,169,229,230,231,232,233,236,254,256,257,259,260,262,263,266)
$negative = @(60,70)
$direct = @(72,82,86,87,88,89,90,98,105,229,230,231,232,233,236,254,256,257,259,260,262,263,266)
$suspect = @(43,45,73,74,75,99,103,108,116,126,127,129,136,137,150,151,155,172,186,193,243,255,258,265)
$complement = @(72,82,98,259,260)
$result = foreach ($row in $items) {
  $n = [int]($row.ReviewItemID -replace 'END-','')
  $direction = 'context_dependent'
  $exclude = $true
  $isDirect = $false
  $reason = 'Neither the represented endpoint nor its unambiguous synonym has a supported common performance direction; exclude rather than infer harm from endpoint class.'
  $evidence = 'published_five_expert_scores.csv; endpoint labels in Review_endpoint_dictionary.csv'
  $flag = 'No full source-paper itemwise verification; rule audit only'
  $matches = @($expert | Where-Object { $_.metric.Trim().ToLowerInvariant() -eq $row.Metric.Trim().ToLowerInvariant() -or $_.metric.Trim().ToLowerInvariant() -eq $row.TransformedMetric.Trim().ToLowerInvariant() })
  if ($matches.Count -gt 0) {
    $evidence += '; ' + (($matches | ForEach-Object { $_.metric + '=' + $_.'Final score' }) -join '; ')
    $reason = 'Supplied published ratings are ambiguous or absent for a performance direction; biochemical activity, state, timing or size alone is not a demonstrated fitness change.'
  }
  if ($positive -contains $n) {
    $direction = 'beneficial_when_increases'; $exclude = $false
    $reason = 'Explicit successful biological outcome or defensible metric-level equivalent of a published positive-consensus trait; greater represented value indicates better performance under this coding rule.'
    if ($n -in @(37,38,39,40,41,42,44,46,47,48)) { $evidence += '; positive consensus: crushing force / force at fracture / hardness / force of byssal threads / maximum load / mechanical resilience' }
    if ($n -in @(113,152)) { $evidence += '; condition factor, fulton index, condition index=positive' }
    if ($n -in @(118,119,120,121,123,128,138,156,157,158,163,166,169)) { $evidence += '; growth rate=positive'; $flag += '; confirm actual change/rate measurement rather than size proxy' }
    if ($n -in @(72,82,86,87,88,89,90,98,105,256,257)) { $evidence += '; developmental success / hatching success=positive; abnormalities=negative' }
  }
  if ($negative -contains $n) { $direction = 'adverse_when_increases'; $exclude = $false; $reason = 'Shell damage retains an adverse direction: greater damage is worse performance.'; $evidence += '; shell damage=negative' }
  if ($direct -contains $n) { $isDirect = $true }
  if ($complement -contains $n) { $reason = 'Represented survival/normality is the documented complement of mortality/abnormality; do not reverse a second time.'; $flag += '; verify numerical complement provenance in source data' }
  if ($suspect -contains $n) { $flag += '; mapping requires source verification'; $reason = 'Original and represented metrics are not demonstrably equivalent from the supplied labels, or the original published rating is ambiguous; cannot import a favourable direction from the replacement label.' }
  if ($n -in @(73,74,75)) { $reason = 'Asymmetry may be continuous and is not automatically the complement of normal development; transformation needs original measurement and formula.' }
  if ($n -eq 45) { $reason = 'Skeletal microdensity is not crushing force; mechanical-strength consensus cannot be transferred without calibration or source verification.' }
  if ($n -in @(255,258)) { $reason = 'Density or health score is not itself a survival proportion; label conversion alone does not establish survival or a direct outcome.' }
  if ($n -in @(84,91,92,106)) { $reason = 'Settlement/metamorphosis is biologically a transition outcome but supplied five-expert consensus is ambiguous; excluded under the common consensus threshold.' }
  [pscustomobject]@{ ReviewItemID=$row.ReviewItemID; DirectionRule=$direction; ExcludedFromAligned=$exclude; DirectOutcome=$isDirect; Reason=$reason; Evidence=$evidence; Flag=$flag }
}
if ($result.Count -ne 266) { throw "Expected 266 items, obtained $($result.Count)" }
$result | Export-Csv -LiteralPath "$root/coding_review/coder_b.csv" -NoTypeInformation -Encoding utf8
$result | Group-Object DirectionRule | Select-Object Name,Count
