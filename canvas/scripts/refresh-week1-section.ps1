[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [int]$Section,

    [Parameter(Mandatory = $true)]
    [long]$CourseId,

    [int]$ExpectedModuleCount = 16,

    [switch]$Execute
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

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

$stamp = Get-Date -Format "yyyyMMdd-HHmmss"
$runRoot = Join-Path $ops "work\week1-refresh\fall-2026-$Section-$stamp"
New-Item -ItemType Directory -Force -Path $runRoot | Out-Null

$headers = @{ Authorization = "Bearer $env:CANVAS_TOKEN" }
$canvasBase = "https:" + "//uwlac.instructure.com"
$courseUri = "$canvasBase/api/v1/courses/$CourseId"
$moduleUri = "$canvasBase/api/v1/courses/$CourseId/modules?per_page=100"
$assignmentUri = "$canvasBase/api/v1/courses/$CourseId/assignments?per_page=100"
$pageUri = "$canvasBase/api/v1/courses/$CourseId/pages?per_page=100"

function ConvertTo-FlatArray {
    param([Parameter(Mandatory = $true)]$Response)

    $result = @()
    foreach ($record in $Response) {
        $result += $record
    }
    return $result
}

function Invoke-CanvasCtl {
    param([Parameter(Mandatory = $true)][string[]]$CliArguments)

    Write-Host ""
    Write-Host ("canvasctl " + ($CliArguments -join " ")) -ForegroundColor Cyan
    & $python @CliArguments
    if ($LASTEXITCODE -ne 0) {
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

function Assert-ReadyPlan {
    param(
        [Parameter(Mandatory = $true)]$Plan,
        [Parameter(Mandatory = $true)][string]$Label
    )

    if ($Plan.status -notin @("ready", "ready-with-deferred")) {
        throw "$Label status is '$($Plan.status)', not ready."
    }
    if (@($Plan.blockers).Count -gt 0) {
        throw "$Label has blockers: $($Plan.blockers -join '; ')"
    }
}

$course = Invoke-RestMethod -Method Get -Uri $courseUri -Headers $headers
if ([string]$course.id -ne [string]$CourseId) {
    throw "Canvas returned course $($course.id), not $CourseId."
}
if ($course.workflow_state -ne "unpublished") {
    throw "Course $CourseId must remain unpublished during this refresh."
}

$modules = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $moduleUri -Headers $headers)
if ($modules.Count -ne $ExpectedModuleCount) {
    throw "Expected $ExpectedModuleCount Canvas modules; found $($modules.Count). No changes made."
}

$selectedModuleNames = @(
    "Course Info and Resources",
    "Week 1: Intro to Data Analysis"
)
foreach ($moduleName in $selectedModuleNames) {
    if (@($modules | Where-Object { $_.name -eq $moduleName }).Count -ne 1) {
        throw "Expected exactly one module named '$moduleName'. No changes made."
    }
}

$assignments = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $assignmentUri -Headers $headers)
$oldLab = @($assignments | Where-Object { $_.name -eq "Technology Set-Up" })
$newLab = @($assignments | Where-Object { $_.name -eq "Lab 1: Technology Setup" })
if (($oldLab.Count + $newLab.Count) -ne 1) {
    throw "Expected one Technology Set-Up/Lab 1 assignment; found $($oldLab.Count + $newLab.Count). No changes made."
}
$labAssignment = if ($newLab.Count -eq 1) { $newLab[0] } else { $oldLab[0] }

$homeworkAssignments = @(
    $assignments |
        Where-Object { $_.name -eq "Homework 1: Choose Your Own Data Adventure" }
)
if ($homeworkAssignments.Count -ne 1) {
    throw "Expected exactly one Homework 1 assignment; found $($homeworkAssignments.Count). No changes made."
}
$homeworkAssignment = $homeworkAssignments[0]

$pages = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $pageUri -Headers $headers)
$requiredPageTitles = @(
    "Syllabus",
    "Instructor Contact Info",
    "Student Hours Zoom Link"
)
$selectedPages = @()
foreach ($pageTitle in $requiredPageTitles) {
    $matches = @($pages | Where-Object { $_.title -eq $pageTitle })
    if ($matches.Count -ne 1) {
        throw "Expected exactly one Canvas page titled '$pageTitle'; found $($matches.Count). No changes made."
    }
    $selectedPages += $matches[0]
}

$weekOneModule = @($modules | Where-Object { $_.name -eq "Week 1: Intro to Data Analysis" })[0]
$weekOneItemsUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($weekOneModule.id)/items?per_page=100"
$weekOneItems = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $weekOneItemsUri -Headers $headers)
$obsoleteLabLinks = @(
    $weekOneItems |
        Where-Object {
            $_.type -eq "ExternalUrl" -and
            $_.title -eq "Lab 1: Posit Cloud Certificate"
        }
)
if ($obsoleteLabLinks.Count -gt 1) {
    throw "Found more than one obsolete Lab 1 Posit link. No changes made."
}

$planSummary = [pscustomobject]@{
    section = $Section
    course_id = $CourseId
    course_state = $course.workflow_state
    modules_found = $modules.Count
    modules_prepared_for_manual_publication = $selectedModuleNames
    assignments_to_refresh = @($labAssignment.name, $homeworkAssignment.name)
    pages_to_refresh = $requiredPageTitles
    obsolete_lab_links_to_remove = $obsoleteLabLinks.Count
    course_will_be_published = $false
}
$planSummary | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $runRoot "preflight.json")
$planSummary | Format-List

if (-not $Execute) {
    Write-Output "Plan only. Re-run with -Execute after reviewing this summary."
    Write-Output "Audit directory: $runRoot"
    return
}

# Keep every module unpublished while its contents are prepared for manual release.
foreach ($canvasModule in $modules) {
    if ($canvasModule.published -eq $true) {
        $updateModuleUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($canvasModule.id)"
        Invoke-RestMethod -Method Put -Uri $updateModuleUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "module[published]" = "false" } | Out-Null
    }
}

# Canvas requires repository-managed objects to be drafts before refreshing them.
foreach ($assignment in @($labAssignment, $homeworkAssignment)) {
    $updateAssignmentUri = "$canvasBase/api/v1/courses/$CourseId/assignments/$($assignment.id)"
    $body = @{ "assignment[published]" = "false" }
    if ($assignment.id -eq $labAssignment.id) {
        $body["assignment[name]"] = "Lab 1: Technology Setup"
    }
    Invoke-RestMethod -Method Put -Uri $updateAssignmentUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body $body | Out-Null
}

foreach ($page in $selectedPages) {
    $pageSlug = [Uri]::EscapeDataString([string]$page.url)
    $updatePageUri = "$canvasBase/api/v1/courses/$CourseId/pages/$pageSlug"
    Invoke-RestMethod -Method Put -Uri $updatePageUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "wiki_page[published]" = "false" } | Out-Null
}

$common = @(
    "--course-repo", $courseRepo,
    "--config", $config,
    "--section", [string]$Section
)

$assignmentPlanDirectory = Join-Path $runRoot "01-assignment-plan"
Invoke-CanvasCtl -CliArguments (@(
    "-m", "eco230_canvas.cli", "content", "plan"
) + $common + @(
    "--output", $assignmentPlanDirectory,
    "--only-assignment", "technology-set-up",
    "--only-assignment", "homework-1-choose-your-own-data-adventure"
))
$assignmentPlanPath = Join-Path $assignmentPlanDirectory "content-plan.json"
$assignmentPlan = Read-JsonFile -Path $assignmentPlanPath
Assert-ReadyPlan -Plan $assignmentPlan -Label "Assignment refresh plan"

$assignmentReceiptDirectory = Join-Path $runRoot "02-assignment-apply"
Invoke-CanvasCtl -CliArguments (@(
    "-m", "eco230_canvas.cli", "content", "apply"
) + $common + @(
    "--plan", $assignmentPlanPath,
    "--output", $assignmentReceiptDirectory,
    "--confirm-destination-course-id", [string]$CourseId,
    "--execute"
))

$pagePlanDirectory = Join-Path $runRoot "03-page-plan"
Invoke-CanvasCtl -CliArguments (@(
    "-m", "eco230_canvas.cli", "content", "plan"
) + $common + @(
    "--output", $pagePlanDirectory,
    "--only-page", "syllabus",
    "--only-page", "instructor-contact-info",
    "--only-page", "student-hours-zoom-link"
))
$pagePlanPath = Join-Path $pagePlanDirectory "content-plan.json"
$pagePlan = Read-JsonFile -Path $pagePlanPath
Assert-ReadyPlan -Plan $pagePlan -Label "Page refresh plan"

$pageReceiptDirectory = Join-Path $runRoot "04-page-apply"
Invoke-CanvasCtl -CliArguments (@(
    "-m", "eco230_canvas.cli", "content", "apply"
) + $common + @(
    "--plan", $pagePlanPath,
    "--output", $pageReceiptDirectory,
    "--confirm-destination-course-id", [string]$CourseId,
    "--execute"
))

# Remove only the redundant module placement. The underlying external link is
# not a Canvas content object, and no assignment, page, file, or submission is deleted.
foreach ($obsoleteLink in $obsoleteLabLinks) {
    $obsoleteItemUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($weekOneModule.id)/items/$($obsoleteLink.id)"
    Invoke-RestMethod -Method Delete -Uri $obsoleteItemUri -Headers $headers | Out-Null
}

& (Join-Path $scriptDirectory "set-module-visibility.ps1") `
    -CourseId $CourseId `
    -ExpectedModuleCount $ExpectedModuleCount `
    -PublishSelectedContent `
    -KeepModulesUnpublished `
    -AuditDirectory (Join-Path $runRoot "05-module-visibility") `
    -Execute

$verifiedCourse = Invoke-RestMethod -Method Get -Uri $courseUri -Headers $headers
$verifiedModules = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $moduleUri -Headers $headers)
$verifiedAssignments = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $assignmentUri -Headers $headers)
$verifiedPages = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $pageUri -Headers $headers)
$verifiedWeekOneItems = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri $weekOneItemsUri -Headers $headers)

$remainingOldLabAssignments = @($verifiedAssignments | Where-Object { $_.name -eq "Technology Set-Up" })
$verifiedLabAssignments = @($verifiedAssignments | Where-Object { $_.name -eq "Lab 1: Technology Setup" })
$verifiedHomeworkAssignments = @($verifiedAssignments | Where-Object { $_.name -eq "Homework 1: Choose Your Own Data Adventure" })
$remainingObsoleteLinks = @($verifiedWeekOneItems | Where-Object { $_.type -eq "ExternalUrl" -and $_.title -eq "Lab 1: Posit Cloud Certificate" })
$publishedModules = @($verifiedModules | Where-Object { $_.published -eq $true })
$verifiedSelectedPages = @($verifiedPages | Where-Object { $_.title -in $requiredPageTitles })

if ($verifiedCourse.workflow_state -ne "unpublished") {
    throw "Course $CourseId was unexpectedly published."
}
if ($remainingOldLabAssignments.Count -ne 0 -or $verifiedLabAssignments.Count -ne 1) {
    throw "The merged Lab 1 assignment title did not converge."
}
if ($verifiedLabAssignments[0].published -ne $true) {
    throw "Lab 1: Technology Setup is not published."
}
if (@($verifiedLabAssignments[0].submission_types | Where-Object { $_ -eq "online_text_entry" }).Count -ne 1 -or
    @($verifiedLabAssignments[0].submission_types | Where-Object { $_ -eq "online_upload" }).Count -ne 1) {
    throw "Lab 1: Technology Setup does not allow both text entry and file upload."
}
if ($verifiedHomeworkAssignments.Count -ne 1 -or $verifiedHomeworkAssignments[0].published -ne $true) {
    throw "Homework 1 is not uniquely present and published."
}
if (@($verifiedHomeworkAssignments[0].submission_types | Where-Object { $_ -eq "online_text_entry" }).Count -ne 1 -or
    @($verifiedHomeworkAssignments[0].submission_types | Where-Object { $_ -eq "online_upload" }).Count -ne 1) {
    throw "Homework 1 does not allow both text entry and file upload."
}
if ($remainingObsoleteLinks.Count -ne 0) {
    throw "The redundant Lab 1 Posit module link still exists."
}
if ($publishedModules.Count -ne 0) {
    throw "Expected every module to remain unpublished for manual release."
}
if ($verifiedSelectedPages.Count -ne 3 -or
    @($verifiedSelectedPages | Where-Object { $_.published -eq $true }).Count -ne 3) {
    throw "One or more refreshed Course Info pages are not published."
}

$verification = [pscustomobject]@{
    status = "verified"
    section = $Section
    course_id = $CourseId
    course_state = $verifiedCourse.workflow_state
    published_modules = @()
    modules_ready_for_manual_publication = $selectedModuleNames
    lab_assignment = [pscustomobject]@{
        id = $verifiedLabAssignments[0].id
        name = $verifiedLabAssignments[0].name
        published = $verifiedLabAssignments[0].published
        submission_types = $verifiedLabAssignments[0].submission_types
    }
    homework_1 = [pscustomobject]@{
        id = $verifiedHomeworkAssignments[0].id
        published = $verifiedHomeworkAssignments[0].published
        submission_types = $verifiedHomeworkAssignments[0].submission_types
    }
    refreshed_pages = @($verifiedSelectedPages | Sort-Object title | ForEach-Object {
        [pscustomobject]@{ title = $_.title; published = $_.published }
    })
    obsolete_lab_links = $remainingObsoleteLinks.Count
}
$verification | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $runRoot "verification.json")
$verification | ConvertTo-Json -Depth 6
Write-Output "Audit directory: $runRoot"
