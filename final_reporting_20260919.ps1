param([string]$ArchivePath=$PSScriptRoot)
$ErrorActionPreference='Stop'
$bodyPath=Join-Path $ArchivePath 'manuscript_revision_body.txt'
$text=[IO.File]::ReadAllText($bodyPath)
$text=$text.Replace('cell-weighted mean availability','mean availability across scored cells (each cell weighted equally)')
$text=$text.Replace('Figure 2. Full-database matched response definitions within ecological subgroups.','Figure 2. Response estimates within ecological subgroups in the full database.')
$text=$text.Replace('Figure 4. Full-database paired target differences within ecological subgroups.','Figure 4. Differences between response definitions within ecological subgroups in the full database.')
$text=$text.Replace('Points show REML means and 95% Hartung-Knapp confidence intervals.','Points show REML means and coloured bars show complete 95% Hartung-Knapp confidence intervals. Prediction intervals are not drawn, so no truncation arrows are required.')
$detail='Black row labels in panel b report BH q and matched StudyID-cell count (s) for signed contrasts, and the mean paired difference and s for descriptive contrasts. '
$subdetail='The 69 signed contrasts (including phylum in source data) yielded no BH-significant comparisons. This figure complements the within-subgroup estimates in Figure 2; it does not test differences between ecological groups. '
if(!$text.Contains($detail)){$text=$text.Replace('Full estimates, intervals, P and q values are in Supplementary Table S18 and the accompanying Excel file.',$detail+'Full estimates, intervals, P and q values are in Supplementary Table S18 and the accompanying Excel file.')}
if(!$text.Contains($subdetail)){$text=$text.Replace('A numerical target difference is not a test of biological harm.',$subdetail+'A numerical target difference is not a test of biological harm.')}
$text=$text.Replace($detail+$detail,$detail).Replace($subdetail+$subdetail,$subdetail)
[IO.File]::WriteAllText($bodyPath,$text,[Text.UTF8Encoding]::new($false))
# Number figures by first mention: complete ecological subgroups before primary stages.
if($text.Contains('Figure 4. Differences between response definitions within ecological subgroups')) {
  $text=[regex]::Replace($text,'\b(Figure |Fig\. )([34])(?=[ab\s\).;,:])',{
    param($m) $m.Groups[1].Value + $(if($m.Groups[2].Value -eq '3'){'4'}else{'3'})
  })
  $start=$text.IndexOf('3.3 Directional and endpoint-aligned responses in the primary subset')
  $middle=$text.IndexOf('3.4 Matched evidence and numerical target differences')
  $end=$text.IndexOf('3.5 Sensitivity analyses')
  if($start -ge 0 -and $middle -gt $start -and $end -gt $middle) {
    $rve=$text.Substring($start,$middle-$start).Replace('3.3 Directional','3.4 Directional')
    $matched=$text.Substring($middle,$end-$middle).Replace('3.4 Matched','3.3 Matched')
    $text=$text.Substring(0,$start)+$matched+$rve+$text.Substring($end)
  }
  $lines=$text -split '\r?\n'
  $captions=@($lines | Where-Object {$_ -match '^Figure [1-6]\.'} | Sort-Object {[int]([regex]::Match($_,'^Figure (\d)').Groups[1].Value)})
  $ci=0
  for($i=0;$i -lt $lines.Count;$i++){if($lines[$i] -match '^Figure [1-6]\.'){$lines[$i]=$captions[$ci];$ci++}}
  [IO.File]::WriteAllText($bodyPath,($lines -join "`n"),[Text.UTF8Encoding]::new($false))
}
# Descriptive estimates are not missing or non-estimable. Keep numeric columns intact.
foreach($name in @('Fig3b_estimand_target_differences.csv','TableS18_pairwise_target_differences.csv')) {
  $path=Join-Path $ArchivePath ('data/source_data/'+$name)
  $rows=Import-Csv -LiteralPath $path
  foreach($row in $rows) {
    if($row.Estimate -ne '' -and $row.TargetDifference -ne 'Aligned - directional') {$row.Significance='Descriptive'}
    elseif($row.Estimate -ne '' -and $row.FDR_q_value -ne '' -and [double]$row.FDR_q_value -ge .05) {$row.Significance='ns'}
  }
  $rows | Export-Csv -LiteralPath $path -NoTypeInformation -Encoding utf8
}
