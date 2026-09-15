[CmdletBinding()]
param(
    [ValidateSet(4, 11, 12)]
    [int[]]$Sections = @(4, 11, 12),

    [int]$ExpectedModuleCount = 16,

    [switch]$Execute
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$canvasToken = [Environment]::GetEnvironmentVariable("CANVAS_TOKEN", "Process")
if ([string]::IsNullOrWhiteSpace($canvasToken)) {
    $canvasToken = [Environment]::GetEnvironmentVariable("CANVAS_TOKEN", "User")
}
if ([string]::IsNullOrWhiteSpace($canvasToken)) {
    throw "CANVAS_TOKEN is not set for this process or Windows user. Run canvas\scripts\set-canvas-token.ps1 first."
}

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$courseRepo = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory "..\.."))
$ops = "C:\Users\bolan\Documents\eco230-canvas-ops"
$python = Join-Path $ops ".venv\Scripts\python.exe"
$config = Join-Path $ops "config\fall-2026.local.yml"
$renderHelper = Join-Path $scriptDirectory "render-canvas-page.py"
$env:PYTHONPATH = Join-Path $ops "src"
$canvasBase = "https:" + "//uwlac.instructure.com"
$headers = @{ Authorization = "Bearer $canvasToken" }

$moduleTitle = "Week 2: Descriptive Statistics"
$pageTitle = "Lab 2 Descriptive Statistics and Cross Tabs"
$pageSlug = "lab-2-descriptive-statistics-and-cross-tabs"
$sourcePath = "canvas/content/pages/week02-lab.md"
$requiredBodyText = @(
    "There is nothing to submit for Lab 2",
    "Homework 2 requires similar analysis",
    "csi()",
    "I created and interpreted a cross-tabulation with percentages"
)

$courseMap = [ordered]@{
    "4" = [pscustomobject]@{ course_id = 870634; course_code = "ECO 230-04" }
    "11" = [pscustomobject]@{ course_id = 869206; course_code = "ECO 230-11" }
    "12" = [pscustomobject]@{ course_id = 870121; course_code = "ECO 230-12" }
}

foreach ($requiredPath in @($python, $config, $renderHelper, (Join-Path $courseRepo $sourcePath))) {
    if (-not (Test-Path -LiteralPath $requiredPath -PathType Leaf)) {
        throw "Required file was not found: $requiredPath"
    }
}

function ConvertTo-FlatArray {
    param([Parameter(Mandatory = $true)]$Response)

    $result = @()
    foreach ($record in $Response) { $result += $record }
    return $result
}

function Get-PageUri {
    param(
        [Parameter(Mandatory = $true)][long]$CourseId,
        [Parameter(Mandatory = $true)][string]$PageUrl
    )

    return "$canvasBase/api/v1/courses/$CourseId/pages/$([Uri]::EscapeDataString($PageUrl))"
}

$duplicates = @($Sections | Group-Object | Where-Object { $_.Count -gt 1 })
if ($duplicates.Count -gt 0) {
    throw "Sections contains duplicates: $(@($duplicates | ForEach-Object { $_.Name }) -join ', ')"
}

$stamp = Get-Date -Format "yyyyMMdd-HHmmss"
$runRoot = Join-Path $courseRepo "canvas\work\lab2-page-refresh\$stamp"
New-Item -ItemType Directory -Force -Path $runRoot | Out-Null
$preflight = @()

foreach ($section in $Sections) {
    $renderOutput = & $python $renderHelper `
        --course-repo $courseRepo `
        --source-path $sourcePath `
        --title $pageTitle `
        --config $config `
        --section ([string]$section)
    if ($LASTEXITCODE -ne 0) {
        throw "Canvas renderer failed for section $section."
    }
    $renderedBody = [string](($renderOutput -join "`n") | ConvertFrom-Json).body
    if ($renderedBody -match '\{\{[^}]+\}\}') {
        throw "Rendered Lab 2 page still contains an unresolved template value for section $section."
    }
    foreach ($expected in $requiredBodyText) {
        if ($renderedBody -notlike "*$expected*") {
            throw "Rendered Lab 2 page is missing expected text '$expected' for section $section."
        }
    }

    $mapping = $courseMap[[string]$section]
    $courseId = [long]$mapping.course_id
    $course = Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId" -Headers $headers
    if ([string]$course.id -ne [string]$courseId -or [string]$course.course_code -ne [string]$mapping.course_code) {
        throw "Canvas course identity mismatch for section $section. No changes made."
    }
    if ([string]$course.workflow_state -notin @("unpublished", "available")) {
        throw "Section $section has unsupported course workflow state '$($course.workflow_state)'."
    }

    $modules = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId/modules?per_page=100" -Headers $headers)
    if ($modules.Count -ne $ExpectedModuleCount) {
        throw "Section $section expected $ExpectedModuleCount modules; found $($modules.Count). No changes made."
    }
    $week2Matches = @($modules | Where-Object { [string]$_.name -eq $moduleTitle })
    if ($week2Matches.Count -ne 1 -or [bool]$week2Matches[0].published -ne $true) {
        throw "Section $section requires exactly one already-published '$moduleTitle' module."
    }
    $week2 = $week2Matches[0]

    $pages = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId/pages?search_term=$([Uri]::EscapeDataString($pageTitle))&per_page=100" -Headers $headers)
    $pageMatches = @($pages | Where-Object { [string]$_.title -eq $pageTitle -and [string]$_.url -eq $pageSlug })
    if ($pageMatches.Count -ne 1 -or [bool]$pageMatches[0].published -ne $true) {
        throw "Section $section requires exactly one already-published Lab 2 page with the tracked slug."
    }

    $items = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId/modules/$($week2.id)/items?per_page=100" -Headers $headers)
    $itemMatches = @($items | Where-Object {
        [string]$_.title -eq $pageTitle -and [string]$_.type -eq "Page" -and [string]$_.page_url -eq $pageSlug
    })
    if ($itemMatches.Count -ne 1 -or [bool]$itemMatches[0].published -ne $true) {
        throw "Section $section requires exactly one already-published Lab 2 module item."
    }

    $preflight += [pscustomobject]@{
        section = [int]$section
        course_id = $courseId
        course_code = [string]$course.course_code
        course_state = [string]$course.workflow_state
        module_id = [long]$week2.id
        module_item_id = [long]$itemMatches[0].id
        page_url = [string]$pageMatches[0].url
        rendered_body = $renderedBody
        module_states = @($modules | Sort-Object position | ForEach-Object {
            [pscustomobject]@{ id = [long]$_.id; name = [string]$_.name; published = [bool]$_.published }
        })
    }
}

$preflight | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath (Join-Path $runRoot "preflight.json") -Encoding UTF8
$preflight | Select-Object section, course_id, course_code, course_state, module_id, module_item_id | Format-Table -AutoSize

if (-not $Execute) {
    Write-Output "Plan only. The guarded refresh would update the body of one already-published Lab 2 page per section."
    Write-Output "Audit directory: $runRoot"
    return
}

$verification = @()
foreach ($snapshot in $preflight) {
    $courseId = [long]$snapshot.course_id
    $pageUri = Get-PageUri -CourseId $courseId -PageUrl $snapshot.page_url
    $response = Invoke-RestMethod `
        -Method Put `
        -Uri $pageUri `
        -Headers $headers `
        -ContentType "application/x-www-form-urlencoded" `
        -Body @{
            "wiki_page[body]" = [string]$snapshot.rendered_body
            "wiki_page[published]" = "true"
            "wiki_page[notify_of_update]" = "false"
        }
    if ([bool]$response.published -ne $true) {
        throw "Section $($snapshot.section) page publication changed unexpectedly."
    }

    $courseAfter = Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId" -Headers $headers
    if ([string]$courseAfter.workflow_state -ne [string]$snapshot.course_state) {
        throw "Section $($snapshot.section) course publication state changed unexpectedly."
    }
    $modulesAfter = ConvertTo-FlatArray (Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId/modules?per_page=100" -Headers $headers)
    foreach ($moduleBefore in $snapshot.module_states) {
        $moduleAfter = @($modulesAfter | Where-Object { [long]$_.id -eq [long]$moduleBefore.id })
        if ($moduleAfter.Count -ne 1 -or [bool]$moduleAfter[0].published -ne [bool]$moduleBefore.published) {
            throw "Section $($snapshot.section) module publication state changed for '$($moduleBefore.name)'."
        }
    }

    $pageAfter = Invoke-RestMethod -Method Get -Uri $pageUri -Headers $headers
    foreach ($expected in $requiredBodyText) {
        if ([string]$pageAfter.body -notlike "*$expected*") {
            throw "Section $($snapshot.section) refreshed page is missing expected text '$expected'."
        }
    }
    $itemAfter = Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId/modules/$($snapshot.module_id)/items/$($snapshot.module_item_id)" -Headers $headers
    if ([bool]$pageAfter.published -ne $true -or [bool]$itemAfter.published -ne $true) {
        throw "Section $($snapshot.section) Lab 2 page or module item is no longer published."
    }

    $verification += [pscustomobject]@{
        section = [int]$snapshot.section
        course_id = $courseId
        course_state = [string]$courseAfter.workflow_state
        page_published = [bool]$pageAfter.published
        module_item_published = [bool]$itemAfter.published
        module_publication_unchanged = $true
        expected_content_verified = $true
        canvas_mutations = 1
    }
}

$result = [pscustomobject]@{
    status = "verified"
    mode = "repository-source-api-refresh"
    course_publication_unchanged = $true
    module_publication_unchanged = $true
    sections = $verification
}
$result | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath (Join-Path $runRoot "verification.json") -Encoding UTF8
$result | ConvertTo-Json -Depth 8
Write-Output "Audit directory: $runRoot"
