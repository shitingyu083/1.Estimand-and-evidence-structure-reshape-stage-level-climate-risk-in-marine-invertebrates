param([string]$ArchivePath=$PSScriptRoot,[string]$RenderPath='D:/tmp/ER_visual_final_20260919')
$ErrorActionPreference='Stop'
$pkg=Split-Path $ArchivePath
$clean=Join-Path $pkg 'ER_submission_ready_clean_20260918'
$rebuilt=Join-Path $pkg '_rebuilt_documents_20260919'
$backup=Join-Path $pkg ('_legacy_do_not_submit/visual_package_before_20260919_'+(Get-Date -Format HHmmss))
New-Item -ItemType Directory -Path $backup -Force | Out-Null
Copy-Item -LiteralPath $clean -Destination $backup -Recurse
$bases=@('Fig1_classification_and_evidence_update','Fig2_ecological_subgroup_responses','Fig3_subgroup_paired_differences','Fig4_primary_matched_response_comparison','Fig5_robustness_and_inference_limits','Fig6_reference_deviation_calibration')
$targets=@((Join-Path $ArchivePath 'figures/main'),(Join-Path $pkg 'main_figures'),(Join-Path $clean 'main_figures'))
$ti=0
foreach($target in $targets){
  $old=Join-Path $backup ('previous_main_figures_'+$ti);New-Item -ItemType Directory -Path $old -Force | Out-Null
  foreach($f in Get-ChildItem -LiteralPath $target -File){Move-Item -LiteralPath $f.FullName -Destination $old}
  foreach($base in $bases){foreach($ext in @('pdf','svg','png','tiff')){
    Copy-Item -LiteralPath (Join-Path $RenderPath ('figures/main/'+$base+'.'+$ext)) -Destination $target
  }}
  $ti++
}
foreach($f in Get-ChildItem -LiteralPath $rebuilt -File){
  Copy-Item -LiteralPath $f.FullName -Destination $pkg -Force
  if($f.Extension -in @('.docx','.pdf')){Copy-Item -LiteralPath $f.FullName -Destination $clean -Force}
}
foreach($name in @('README_REPRODUCTION.md','presubmission_review_20260919.md')){
  Copy-Item -LiteralPath (Join-Path $ArchivePath $name) -Destination $pkg -Force
  Copy-Item -LiteralPath (Join-Path $ArchivePath $name) -Destination $clean -Force
}
foreach($target in @($pkg,$clean)){
  Copy-Item -LiteralPath (Join-Path $ArchivePath 'data/source_data/All_meta_estimates_and_tests.xlsx') -Destination $target -Force
}
Write-Output ('Previous submission files preserved in: '+$backup)
