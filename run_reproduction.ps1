param([string]$RscriptPath = $env:ER_RSCRIPT)

$ErrorActionPreference = 'Stop'

if (-not $RscriptPath) {
  $rCommand = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($rCommand) {
    $RscriptPath = $rCommand.Source
  } else {
    $rCandidate = Get-ChildItem 'C:\Program Files\R' -Filter Rscript.exe -Recurse -ErrorAction SilentlyContinue |
      Sort-Object FullName -Descending | Select-Object -First 1
    if ($rCandidate) { $RscriptPath = $rCandidate.FullName }
  }
}
if (-not $RscriptPath -or -not (Test-Path -LiteralPath $RscriptPath)) {
  throw 'Rscript was not found. Install R >= 4.3 or pass -RscriptPath / set ER_RSCRIPT.'
}

& $RscriptPath run_all.R
if ($LASTEXITCODE -ne 0) { throw "run_all.R failed with exit code $LASTEXITCODE." }
Write-Output 'Complete reproduction workflow finished.'
