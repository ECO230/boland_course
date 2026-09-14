[CmdletBinding()]
param(
    [string]$OutputPath,

    [string]$PreviewDirectory,

    [switch]$RefreshData
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory ".."))
$builder = Join-Path $scriptDirectory "build_lab_02_workbook.mjs"
$profileCandidates = @(
    [Environment]::GetFolderPath("UserProfile"),
    (Split-Path -Parent (Split-Path -Parent $repoRoot))
) | Select-Object -Unique
$dependencyRoot = $null
foreach ($profileDirectory in $profileCandidates) {
    $candidate = Join-Path $profileDirectory ".cache\codex-runtimes\codex-primary-runtime\dependencies\node"
    if (Test-Path -LiteralPath $candidate -PathType Container) {
        $dependencyRoot = $candidate
        break
    }
}
if ([string]::IsNullOrWhiteSpace($dependencyRoot)) {
    throw "The bundled Codex Node.js runtime was not found for this repository."
}
$node = Join-Path $dependencyRoot "bin\node.exe"
$nodeModules = Join-Path $dependencyRoot "node_modules"
$python = Join-Path (Split-Path -Parent $dependencyRoot) "python\python.exe"
$extractBuilder = Join-Path $scriptDirectory "build_lab_02_extract.py"
$runtime = Join-Path $repoRoot ".assignment-build\lab02-workbook-runtime"
$runtimeBuilder = Join-Path $runtime "build_lab_02_workbook.mjs"
$runtimeModules = Join-Path $runtime "node_modules"

if ([string]::IsNullOrWhiteSpace($OutputPath)) {
    $OutputPath = Join-Path $repoRoot "week02\labs\data\Lab_02_Excel_Starter.xlsx"
}
if ([string]::IsNullOrWhiteSpace($PreviewDirectory)) {
    $PreviewDirectory = Join-Path $repoRoot "tmp\lab02-workbook-preview"
}

foreach ($requiredPath in @($builder, $node, $nodeModules)) {
    if (-not (Test-Path -LiteralPath $requiredPath)) {
        throw "Required workbook build dependency was not found: $requiredPath"
    }
}

if ($RefreshData) {
    foreach ($requiredPath in @($extractBuilder, $python)) {
        if (-not (Test-Path -LiteralPath $requiredPath)) {
            throw "Required Lab 2 data-build dependency was not found: $requiredPath"
        }
    }
    & $python $extractBuilder
    if ($LASTEXITCODE -ne 0) {
        throw "Lab 2 CSV extract build failed with exit code $LASTEXITCODE."
    }
}

New-Item -ItemType Directory -Force -Path $runtime | Out-Null
Copy-Item -LiteralPath $builder -Destination $runtimeBuilder -Force
if (-not (Test-Path -LiteralPath $runtimeModules)) {
    New-Item -ItemType Junction -Path $runtimeModules -Target $nodeModules | Out-Null
}

& $node $runtimeBuilder `
    --repo-root $repoRoot `
    --output ([System.IO.Path]::GetFullPath($OutputPath)) `
    --preview-dir ([System.IO.Path]::GetFullPath($PreviewDirectory))
if ($LASTEXITCODE -ne 0) {
    throw "Lab 2 workbook build failed with exit code $LASTEXITCODE."
}
