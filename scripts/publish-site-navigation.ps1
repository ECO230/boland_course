[CmdletBinding()]
param([Parameter(Mandatory=$true)][string]$RunRoot,[string[]]$Links=@(),[string]$PythonPath,[switch]$Execute,[switch]$Verify)
$ErrorActionPreference='Stop'
if ($Execute -and $Verify) {throw 'Choose Execute or Verify.'}
$navigationCandidates=@()
if ($PythonPath) {$navigationCandidates+=$PythonPath}
$navigationCandidates+=(Join-Path $PSScriptRoot '../../eco230-canvas-ops/.venv/Scripts/python.exe')
$navigationCandidates+=(Join-Path $env:USERPROFILE '.cache/codex-runtimes/codex-primary-runtime/dependencies/python/python.exe')
$navigationPython=$null
foreach ($navigationCandidate in $navigationCandidates) {
 if (Test-Path -LiteralPath $navigationCandidate) {
  & $navigationCandidate -c "import importlib.util; raise SystemExit(0 if importlib.util.find_spec('yaml') else 1)"
  if ($LASTEXITCODE -eq 0) {$navigationPython=$navigationCandidate;break}
 }
}
if (-not $navigationPython) {throw 'Python with PyYAML is required. Use the adjacent Canvas operations environment or pass -PythonPath to an environment with PyYAML.'}
$env:PYTHONUTF8='1'
$navigationArgs=@((Join-Path $PSScriptRoot 'publish_site_navigation.py'),'--run-root',$RunRoot)
foreach ($navigationLink in $Links) {$navigationArgs+=@('--link',$navigationLink)}
if ($Execute) {$navigationArgs+='--execute'}
if ($Verify) {$navigationArgs+='--verify'}
& $navigationPython @navigationArgs
if ($LASTEXITCODE -ne 0) {throw 'Navigation operation failed; inspect the error and saved plan.'}
