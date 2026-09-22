[CmdletBinding()]
param([switch]$Execute)
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
$arguments = @((Join-Path $PSScriptRoot "refresh-homework3.py"))
if ($Execute) { $arguments += "--execute" }
& $python @arguments
if ($LASTEXITCODE -ne 0) { throw "Homework 3 refresh failed; inspect the run receipts before retrying." }
