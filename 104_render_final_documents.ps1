param([Parameter(Mandatory=$true)][string]$PackagePath)
$ErrorActionPreference='Stop'
$word=New-Object -ComObject Word.Application
$word.Visible=$false
$word.DisplayAlerts=0
try {
  $main=$word.Documents.Open((Join-Path $PackagePath 'ER_manuscript.docx'),$false,$true)
  try {
    $start=$null; $end=$null; $abstractStart=$null; $abstractEnd=$null
    foreach($para in $main.Paragraphs) {
      $text=$para.Range.Text.Trim()
      if($text -eq '1. Introduction') {$start=$para.Range.Start}
      if($text -eq 'Figure legends') {$end=$para.Range.Start}
      if($text -eq 'Abstract') {$abstractStart=$para.Range.End}
      if($text -eq 'Keywords') {$abstractEnd=$para.Range.Start}
    }
    if($null -eq $start -or $null -eq $end -or $null -eq $abstractStart -or $null -eq $abstractEnd) {throw 'Word-count section boundaries not found'}
    $mainWords=$main.Range($start,$end).ComputeStatistics(0)
    $abstractWords=$main.Range($abstractStart,$abstractEnd).ComputeStatistics(0)
  } finally {$main.Close(0)}
  foreach($file in Get-ChildItem -LiteralPath $PackagePath -Filter '*.docx' -File) {
    $doc=$word.Documents.Open($file.FullName,$false,$false)
    try {
      if($file.Name -eq 'ER_title_page.docx') {
        for($i=$doc.Paragraphs.Count; $i -ge 1; $i--) {
          $para=$doc.Paragraphs.Item($i)
          if($para.Range.Text -match '^Abstract word count:') {$para.Range.Text='Abstract word count: '+$abstractWords+[char]13}
          if($para.Range.Text -match '^Main text word count') {$para.Range.Text='Main text word count (Introduction through Conclusions): '+$mainWords+[char]13}
        }
        $doc.Save()
      }
      $doc.Repaginate()
      $doc.ExportAsFixedFormat([IO.Path]::ChangeExtension($file.FullName,'.pdf'),17)
      Write-Output ($file.Name+': '+$doc.ComputeStatistics(2)+' pages')
    } finally {$doc.Close(0)}
  }
  Write-Output ('WORD_COUNTS: abstract='+$abstractWords+'; main='+$mainWords)
} finally {$word.Quit()}
