[CmdletBinding()]
param(
    [int[]]$Weeks = @(1, 2, 3, 4, 5, 6, 7),
    [string]$OutputDirectory,
    [string]$QuartoPath,
    [string]$ChromePath,
    [string]$NodePath,
    [string]$NodeModules,
    [switch]$SkipRender
)

$ErrorActionPreference = "Stop"

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$repository = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory "..\.."))

if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $repository "canvas\work\slide-pdfs"
}
$OutputDirectory = [System.IO.Path]::GetFullPath($OutputDirectory)
New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null

if (-not $QuartoPath) {
    $quartoCandidates = @(
        "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe",
        (Get-Command quarto -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -First 1)
    ) | Where-Object { $_ -and (Test-Path -LiteralPath $_) }
    $QuartoPath = $quartoCandidates | Select-Object -First 1
}
if (-not $QuartoPath -or -not (Test-Path -LiteralPath $QuartoPath)) {
    throw "Quarto was not found. Pass -QuartoPath explicitly."
}

if (-not $ChromePath) {
    $chromeCandidates = @(
        "C:\Program Files\Google\Chrome\Application\chrome.exe",
        "C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe",
        "C:\Program Files\Microsoft\Edge\Application\msedge.exe"
    ) | Where-Object { Test-Path -LiteralPath $_ }
    $ChromePath = $chromeCandidates | Select-Object -First 1
}
if (-not $ChromePath -or -not (Test-Path -LiteralPath $ChromePath)) {
    throw "Chrome or Edge was not found. Pass -ChromePath explicitly."
}

if (-not $NodePath) {
    $bundledNode = Join-Path $env:USERPROFILE ".cache\codex-runtimes\codex-primary-runtime\dependencies\node\bin\node.exe"
    $nodeCandidates = @(
        $bundledNode,
        (Get-Command node -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -First 1)
    ) | Where-Object { $_ -and (Test-Path -LiteralPath $_) }
    $NodePath = $nodeCandidates | Select-Object -First 1
}
if (-not $NodePath -or -not (Test-Path -LiteralPath $NodePath)) {
    throw "Node.js was not found. Pass -NodePath explicitly."
}

if (-not $NodeModules) {
    $bundledModules = Join-Path $env:USERPROFILE ".cache\codex-runtimes\codex-primary-runtime\dependencies\node\node_modules"
    $moduleCandidates = @(
        (Join-Path $scriptDirectory "node_modules"),
        $bundledModules,
        (Join-Path $repository "node_modules")
    ) | Where-Object { Test-Path -LiteralPath (Join-Path $_ "playwright-core") }
    $NodeModules = $moduleCandidates | Select-Object -First 1
}
if (-not $NodeModules -or -not (Test-Path -LiteralPath (Join-Path $NodeModules "playwright-core"))) {
    throw "The playwright-core Node package was not found. Run npm install in canvas/scripts or pass -NodeModules explicitly."
}
$pdfExporter = Join-Path $scriptDirectory "export-reveal-pdf.cjs"

$results = @()
Push-Location $repository
try {
    foreach ($week in $Weeks) {
        if ($week -lt 1 -or $week -gt 99) {
            throw "Invalid week number: $week"
        }
        $weekPadded = "{0:D2}" -f $week
        $sourceRelative = "week$weekPadded\week_$weekPadded.qmd"
        $source = Join-Path $repository $sourceRelative
        if (-not (Test-Path -LiteralPath $source)) {
            throw "Slide source does not exist: $source"
        }

        if (-not $SkipRender) {
            & $QuartoPath render $source
            if ($LASTEXITCODE -ne 0) {
                throw "Quarto render failed for $sourceRelative"
            }
        }

        $html = Join-Path $repository "_site\week$weekPadded\week_$weekPadded.html"
        if (-not (Test-Path -LiteralPath $html)) {
            throw "Rendered Reveal HTML was not found: $html"
        }

        $pdf = Join-Path $OutputDirectory ("Week_{0}_Slides.pdf" -f $weekPadded)
        $priorNodePath = $env:NODE_PATH
        try {
            $env:NODE_PATH = $NodeModules
            & $NodePath $pdfExporter $html $pdf $ChromePath
            if ($LASTEXITCODE -ne 0) {
                throw "Browser PDF export failed for $sourceRelative"
            }
        }
        finally {
            $env:NODE_PATH = $priorNodePath
        }

        if (-not (Test-Path -LiteralPath $pdf) -or (Get-Item -LiteralPath $pdf).Length -lt 10000) {
            throw "PDF output is missing or unexpectedly small: $pdf"
        }
        $results += [pscustomobject]@{
            week = $week
            source = $sourceRelative.Replace("\", "/")
            rendered_html = "_site/week$weekPadded/week_$weekPadded.html"
            artifact = (Split-Path -Leaf $pdf)
            bytes = (Get-Item -LiteralPath $pdf).Length
            sha256 = (Get-FileHash -LiteralPath $pdf -Algorithm SHA256).Hash.ToLowerInvariant()
        }
    }
}
finally {
    Pop-Location
}

$manifest = [ordered]@{
    schema_version = 1
    status = "local-candidate"
    generated_at = (Get-Date).ToUniversalTime().ToString("o")
    quarto = (& $QuartoPath --version).Trim()
    browser = $ChromePath
    artifacts = $results
}
$manifestPath = Join-Path $OutputDirectory "slide-pdfs.json"
$manifest | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $manifestPath -Encoding utf8

Write-Output ("Created {0} slide PDF(s) in {1}" -f $results.Count, $OutputDirectory)
