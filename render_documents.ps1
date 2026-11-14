param([Parameter(Mandatory=$true)][string]$PackagePath)
$ErrorActionPreference='Stop'
$word=New-Object -ComObject Word.Application
$word.Visible=$false
$word.DisplayAlerts=0
try {
  foreach($file in Get-ChildItem -LiteralPath $PackagePath -Filter '*.docx' -File) {
    $doc=$word.Documents.Open($file.FullName,$false,$true)
    try {
      $doc.Repaginate()
      $out=[IO.Path]::ChangeExtension($file.FullName,'.pdf')
      $doc.ExportAsFixedFormat($out,17)
      Write-Output ($file.Name+': '+$doc.ComputeStatistics(2)+' pages')
    } finally {$doc.Close(0)}
  }
} finally {$word.Quit()}
