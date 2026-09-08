[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [int]$Section,

    [Parameter(Mandatory = $true)]
    [long]$CourseId,

    [long]$SourceCourseId = 824086,

    [int]$ExpectedModuleCount = 16,

    [string]$ResumeRunRoot,

    [switch]$Execute
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

if (-not $Execute) {
    throw "Provisioning mutates Canvas. Re-run with -Execute after confirming the section and course ID."
}

if ([string]::IsNullOrWhiteSpace($env:CANVAS_TOKEN)) {
    throw "CANVAS_TOKEN is not set in this PowerShell session."
}

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$courseRepo = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory "..\.."))
$ops = "C:\Users\bolan\Documents\eco230-canvas-ops"
$config = Join-Path $ops "config\fall-2026.local.yml"
$python = Join-Path $ops ".venv\Scripts\python.exe"
$env:PYTHONPATH = Join-Path $ops "src"

foreach ($requiredPath in @($config, $python)) {
    if (-not (Test-Path -LiteralPath $requiredPath -PathType Leaf)) {
        throw "Required file not found: $requiredPath"
    }
}

function Invoke-CanvasCtl {
    param(
        [Parameter(Mandatory = $true)][string[]]$CliArguments,
        [int[]]$AllowedExitCodes = @(0)
    )

    Write-Host ""
    Write-Host ("canvasctl " + ($CliArguments -join " ")) -ForegroundColor Cyan
    & $python @CliArguments
    if ($LASTEXITCODE -notin $AllowedExitCodes) {
        throw "canvasctl exited with code $LASTEXITCODE"
    }
}

function Read-JsonFile {
    param([Parameter(Mandatory = $true)][string]$Path)

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        throw "Expected JSON output was not created: $Path"
    }
    return Get-Content -LiteralPath $Path -Raw | ConvertFrom-Json
}

function Assert-PlanReady {
    param(
        [Parameter(Mandatory = $true)]$Plan,
        [Parameter(Mandatory = $true)][string]$Label,
        [string[]]$AllowedStatuses = @("ready", "ready-with-deferred")
    )

    if ($Plan.status -notin $AllowedStatuses) {
        throw "$Label status is '$($Plan.status)', not ready."
    }
    if (@($Plan.blockers).Count -gt 0) {
        throw "$Label has blockers: $($Plan.blockers -join '; ')"
    }
}

function Get-ActionCount {
    param(
        [Parameter(Mandatory = $true)]$Plan,
        [Parameter(Mandatory = $true)][string[]]$Names
    )

    $total = 0
    $counts = $Plan.summary.planned_action_counts
    if ($null -eq $counts) {
        return 0
    }
    foreach ($name in $Names) {
        $property = $counts.PSObject.Properties[$name]
        if ($null -ne $property) {
            $total += [int]$property.Value
        }
    }
    return $total
}

$provisioningRoot = [System.IO.Path]::GetFullPath((Join-Path $ops "work\section-provisioning"))
if ($ResumeRunRoot) {
    $runRoot = [System.IO.Path]::GetFullPath($ResumeRunRoot)
    if (-not $runRoot.StartsWith($provisioningRoot, [System.StringComparison]::OrdinalIgnoreCase)) {
        throw "ResumeRunRoot must be inside $provisioningRoot"
    }
    if (-not (Test-Path -LiteralPath $runRoot -PathType Container)) {
        throw "ResumeRunRoot does not exist: $runRoot"
    }
}
else {
    $stamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $runRoot = Join-Path $provisioningRoot "fall-2026-$Section-$stamp"
    New-Item -ItemType Directory -Force -Path $runRoot | Out-Null
}

$common = @(
    "--course-repo", $courseRepo,
    "--config", $config,
    "--section", [string]$Section
)

# 1. Copy the Canvas-native quizzes, attachments, and assignments that cannot
# be reconstructed safely from repository sources.
$seedReceiptDirectory = Join-Path $runRoot "02-seed-apply"
$seedReceiptPath = Join-Path $seedReceiptDirectory "apply-receipt.json"
$seedMigrationStarted = Test-Path -LiteralPath $seedReceiptPath -PathType Leaf

if (-not $seedMigrationStarted) {
    $seedPlanDirectory = Join-Path $runRoot "01-seed-plan"
    Invoke-CanvasCtl -CliArguments (@("-m", "eco230_canvas.cli", "seed", "plan") + $common + @("--output", $seedPlanDirectory))
    $seedPlanPath = Join-Path $seedPlanDirectory "seed-plan.json"
    $seedPlan = Read-JsonFile -Path $seedPlanPath
    Assert-PlanReady -Plan $seedPlan -Label "Seed plan" -AllowedStatuses @("ready")

    if ((Get-ActionCount -Plan $seedPlan -Names @("copy_from_source")) -gt 0) {
        Invoke-CanvasCtl -CliArguments (@(
            "-m", "eco230_canvas.cli", "seed", "apply"
        ) + $common + @(
            "--plan", $seedPlanPath,
            "--output", $seedReceiptDirectory,
            "--source-course-id", [string]$SourceCourseId,
            "--confirm-destination-course-id", [string]$CourseId,
            "--execute"
        ))
        $seedMigrationStarted = $true
    }
}

if ($seedMigrationStarted) {
    $seedStatusDirectory = Join-Path $seedReceiptDirectory "verification"
    $seedStatus = $null
    for ($attempt = 1; $attempt -le 48; $attempt++) {
        Start-Sleep -Seconds 10
        Invoke-CanvasCtl -AllowedExitCodes @(0, 2) -CliArguments (@(
            "-m", "eco230_canvas.cli", "seed", "status"
        ) + $common + @(
            "--receipt", $seedReceiptPath,
            "--output", $seedStatusDirectory
        ))
        $seedStatus = Read-JsonFile -Path (Join-Path $seedStatusDirectory "status-check.json")
        if ($seedStatus.status -eq "running") {
            continue
        }
        if ($seedStatus.status -ne "completed-verified") {
            throw "Seed migration ended with status '$($seedStatus.status)'."
        }
        break
    }
    if ($null -eq $seedStatus -or $seedStatus.status -ne "completed-verified") {
        throw "Seed migration did not complete within eight minutes."
    }
}

# 2. Create the complete unpublished module skeleton.
$structurePlanDirectory = Join-Path $runRoot "03-structure-plan"
Invoke-CanvasCtl -CliArguments (@("-m", "eco230_canvas.cli", "structure", "plan") + $common + @("--output", $structurePlanDirectory))
$structurePlanPath = Join-Path $structurePlanDirectory "structure-plan.json"
$structurePlan = Read-JsonFile -Path $structurePlanPath
Assert-PlanReady -Plan $structurePlan -Label "Structure plan" -AllowedStatuses @("ready")

if ((Get-ActionCount -Plan $structurePlan -Names @("create_unpublished")) -gt 0) {
    $structureReceiptDirectory = Join-Path $runRoot "04-structure-apply"
    Invoke-CanvasCtl -CliArguments (@(
        "-m", "eco230_canvas.cli", "structure", "apply"
    ) + $common + @(
        "--plan", $structurePlanPath,
        "--output", $structureReceiptDirectory,
        "--confirm-destination-course-id", [string]$CourseId,
        "--execute"
    ))
}

$structureVerifyDirectory = Join-Path $runRoot "05-structure-verification"
Invoke-CanvasCtl -CliArguments (@("-m", "eco230_canvas.cli", "structure", "plan") + $common + @("--output", $structureVerifyDirectory))
$structureVerify = Read-JsonFile -Path (Join-Path $structureVerifyDirectory "structure-plan.json")
Assert-PlanReady -Plan $structureVerify -Label "Structure verification" -AllowedStatuses @("ready")
if ((Get-ActionCount -Plan $structureVerify -Names @("create_unpublished")) -gt 0) {
    throw "Structure verification still contains module or item creation work."
}

# 3. Upload the repository-owned editable guided notes.
$fileReceiptDirectory = Join-Path $runRoot "07-file-apply"
$fileReceiptPath = Join-Path $fileReceiptDirectory "apply-receipt.json"
$filesApplied = Test-Path -LiteralPath $fileReceiptPath -PathType Leaf

if (-not $filesApplied) {
    $filePlanDirectory = Join-Path $runRoot "06-file-plan"
    Invoke-CanvasCtl -CliArguments (@("-m", "eco230_canvas.cli", "files", "plan") + $common + @("--output", $filePlanDirectory))
    $filePlanPath = Join-Path $filePlanDirectory "file-plan.json"
    $filePlan = Read-JsonFile -Path $filePlanPath
    Assert-PlanReady -Plan $filePlan -Label "File plan" -AllowedStatuses @("ready")

    if ((Get-ActionCount -Plan $filePlan -Names @("upload")) -gt 0) {
        Invoke-CanvasCtl -CliArguments (@(
            "-m", "eco230_canvas.cli", "files", "apply"
        ) + $common + @(
            "--plan", $filePlanPath,
            "--output", $fileReceiptDirectory,
            "--confirm-destination-course-id", [string]$CourseId,
            "--execute"
        ))
    }
}
else {
    Write-Host "Reusing successful guided-notes upload receipt: $fileReceiptPath" -ForegroundColor DarkGreen
}

# 4. Create assignments, assignment groups and weights, rubrics, pages, and
# discussions. Intentionally deferred or omitted legacy items remain excluded.
$contentReceiptDirectory = Join-Path $runRoot "09-content-apply"
$contentReceiptPath = Join-Path $contentReceiptDirectory "apply-receipt.json"
$contentApplied = Test-Path -LiteralPath $contentReceiptPath -PathType Leaf

if (-not $contentApplied) {
    $contentPlanDirectory = Join-Path $runRoot "08-content-plan"
    Invoke-CanvasCtl -CliArguments (@("-m", "eco230_canvas.cli", "content", "plan") + $common + @("--output", $contentPlanDirectory))
    $contentPlanPath = Join-Path $contentPlanDirectory "content-plan.json"
    $contentPlan = Read-JsonFile -Path $contentPlanPath
    Assert-PlanReady -Plan $contentPlan -Label "Content plan"

    $contentMutationCount = Get-ActionCount -Plan $contentPlan -Names @(
        "create",
        "create_unpublished",
        "update",
        "update_unpublished",
        "refresh_unpublished"
    )
    if ($contentMutationCount -gt 0) {
    Invoke-CanvasCtl -CliArguments (@(
        "-m", "eco230_canvas.cli", "content", "apply"
    ) + $common + @(
        "--plan", $contentPlanPath,
        "--output", $contentReceiptDirectory,
        "--confirm-destination-course-id", [string]$CourseId,
        "--execute"
    ))
        $contentApplied = $true
    }
}

if ($contentApplied) {
    $contentVerifyDirectory = Join-Path $contentReceiptDirectory "verification"
    Invoke-CanvasCtl -AllowedExitCodes @(0, 2) -CliArguments (@(
        "-m", "eco230_canvas.cli", "content", "verify"
    ) + $common + @(
        "--receipt", $contentReceiptPath,
        "--output", $contentVerifyDirectory
    ))
    $contentVerify = Read-JsonFile -Path (Join-Path $contentVerifyDirectory "content-verification.json")

    $refreshExistingAssignmentKeys = @(
        "homework-1-choose-your-own-data-adventure",
        "guided-notes-1-data-literacy",
        "homework-2-exploratory-data-analysis",
        "guided-notes-2-descriptive-statistics-data-literacy",
        "homework-3-basic-data-visualization",
        "guided-notes-3-data-visualization",
        "homework-4-project-pre-planning",
        "guided-notes-4-communicating-data",
        "guided-notes-4-hypothesis-testing",
        "guided-notes-5-inferential-statistical-tests",
        "guided-notes-6-survey-research-methods",
        "guided-notes-7-machine-learning-experimental-research"
    )
    $unexpectedContentBlockers = @()
    foreach ($blocker in @($contentVerify.blockers)) {
        if ($blocker -match "^Content object is not converged: assignment '([^']+)' requires 'update_unpublished'$" -and $Matches[1] -in $refreshExistingAssignmentKeys) {
            continue
        }
        $unexpectedContentBlockers += $blocker
    }
    if ($unexpectedContentBlockers.Count -gt 0) {
        throw "Content verification has unexpected blockers: $($unexpectedContentBlockers -join '; ')"
    }
    if (@($contentVerify.blockers).Count -gt 0) {
        Write-Warning "Accepted $(@($contentVerify.blockers).Count) refresh_existing convergence warning(s) after a successful content apply."
    }
}

# 5. Place every ready object in its unpublished module and remove only
# placements explicitly classified by the manifest as obsolete duplicates.
$placementPlanDirectory = Join-Path $runRoot "10-placement-plan"
Invoke-CanvasCtl -CliArguments (@("-m", "eco230_canvas.cli", "placement", "plan") + $common + @("--output", $placementPlanDirectory))
$placementPlanPath = Join-Path $placementPlanDirectory "placement-plan.json"
$placementPlan = Read-JsonFile -Path $placementPlanPath
Assert-PlanReady -Plan $placementPlan -Label "Placement plan"

$placementMutationCount = Get-ActionCount -Plan $placementPlan -Names @(
    "create_unpublished",
    "update_unpublished",
    "remove_omitted"
)
if ($placementMutationCount -gt 0) {
    $placementReceiptDirectory = Join-Path $runRoot "11-placement-apply"
    $placementApplyArguments = @(
        "-m", "eco230_canvas.cli", "placement", "apply"
    ) + $common + @(
        "--plan", $placementPlanPath,
        "--output", $placementReceiptDirectory,
        "--confirm-destination-course-id", [string]$CourseId,
        "--execute"
    )
    if ((Get-ActionCount -Plan $placementPlan -Names @("remove_omitted")) -gt 0) {
        $placementApplyArguments += "--remove-omitted"
    }
    Invoke-CanvasCtl -CliArguments $placementApplyArguments

    $placementVerifyDirectory = Join-Path $placementReceiptDirectory "verification"
    Invoke-CanvasCtl -CliArguments (@(
        "-m", "eco230_canvas.cli", "placement", "verify"
    ) + $common + @(
        "--receipt", (Join-Path $placementReceiptDirectory "apply-receipt.json"),
        "--output", $placementVerifyDirectory
    ))
    $placementVerify = Read-JsonFile -Path (Join-Path $placementVerifyDirectory "placement-verification.json")
    if (@($placementVerify.blockers).Count -gt 0) {
        throw "Placement verification has blockers: $($placementVerify.blockers -join '; ')"
    }
}

# 6. Publish only the first two modules and their current contents. The course
# remains unpublished for the instructor's final visual review.
& (Join-Path $scriptDirectory "set-module-visibility.ps1") `
    -CourseId $CourseId `
    -ExpectedModuleCount $ExpectedModuleCount `
    -PublishedModuleNames @(
        "Course Info and Resources",
        "Week 1: Intro to Data Analysis"
    ) `
    -PublishSelectedContent `
    -Execute

Write-Host ""
Write-Host "Section $Section provisioning completed." -ForegroundColor Green
Write-Host "Canvas course ID: $CourseId"
Write-Host "Course publication: unchanged (must remain unpublished)"
Write-Host "Published modules: Course Info and Resources; Week 1: Intro to Data Analysis"
Write-Host "Run record: $runRoot"
