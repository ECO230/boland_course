[CmdletBinding()]
param([switch]$Execute, [switch]$Resume)
$ErrorActionPreference = "Stop"
$ops = "C:\Users\bolan\Documents\eco230-canvas-ops"
$python = Join-Path $ops ".venv\Scripts\python.exe"
$env:PYTHONPATH = Join-Path $ops "src"
$token = [Environment]::GetEnvironmentVariable("CANVAS_TOKEN", "Process")
if ([string]::IsNullOrWhiteSpace($token)) {
    $token = [Environment]::GetEnvironmentVariable("CANVAS_TOKEN", "User")
}
if ([string]::IsNullOrWhiteSpace($token)) { throw "CANVAS_TOKEN is not set." }
$env:CANVAS_TOKEN = $token
$arguments = @((Join-Path $PSScriptRoot "sync-lab3-hints.py"))
if ($Execute) { $arguments += "--execute" }
if ($Resume) { $arguments += "--resume" }
& $python @arguments
if ($LASTEXITCODE -ne 0) { throw "Lab 3 hints synchronization failed; inspect the run receipts before retrying." }
