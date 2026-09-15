[CmdletBinding()]
param(
    [ValidateSet(4, 11, 12)]
    [int[]]$Sections = @(4, 11, 12),
    [string]$TokenEnvironmentVariable = "CANVAS_TOKEN",
    [string]$AuditRoot,
    [switch]$Execute
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$courseIds = @{ 4 = 870634; 11 = 869206; 12 = 870121 }
$expectedModuleCount = 16
$moduleTitle = "Week 2: Descriptive Statistics"
$pageTitle = "Lab 2 Descriptive Statistics and Cross Tabs"
$pageSlug = "lab-2-descriptive-statistics-and-cross-tabs"
$canvasBase = "https:" + "//uwlac.instructure.com"

$token = [Environment]::GetEnvironmentVariable($TokenEnvironmentVariable, "Process")
if ([string]::IsNullOrWhiteSpace($token)) {
    $token = [Environment]::GetEnvironmentVariable($TokenEnvironmentVariable, "User")
}
if ([string]::IsNullOrWhiteSpace($token)) {
    throw "The $TokenEnvironmentVariable environment variable is not set for this process or Windows user. Run canvas\scripts\set-canvas-token.ps1 first."
}

$duplicates = @($Sections | Group-Object | Where-Object { $_.Count -gt 1 })
if ($duplicates.Count -gt 0) {
    throw "Sections contains duplicates: $(@($duplicates | ForEach-Object { $_.Name }) -join ', ')"
}

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$repository = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory "..\.."))
if (-not $AuditRoot) {
    $AuditRoot = Join-Path $repository ("canvas\work\lab2-page-publication\" + (Get-Date -Format "yyyyMMdd-HHmmss"))
}
$AuditRoot = [System.IO.Path]::GetFullPath($AuditRoot)
New-Item -ItemType Directory -Force -Path $AuditRoot | Out-Null

$headers = @{ Authorization = "Bearer $token" }
$plans = @()

foreach ($section in $Sections) {
    $courseId = [long]$courseIds[$section]
    $courseUri = "$canvasBase/api/v1/courses/$courseId"
    $modulesUri = "$canvasBase/api/v1/courses/$courseId/modules?per_page=100"
    $pageSearchUri = "$canvasBase/api/v1/courses/$courseId/pages?search_term=$([Uri]::EscapeDataString($pageTitle))&per_page=100"

    $course = Invoke-RestMethod -Method Get -Uri $courseUri -Headers $headers
    $courseState = [string]$course.workflow_state
    if ($courseState -notin @("unpublished", "available")) {
        throw "Section $section course $courseId has unsupported workflow state '$courseState'."
    }

    $moduleResponse = Invoke-RestMethod -Method Get -Uri $modulesUri -Headers $headers
    $modules = @()
    foreach ($record in $moduleResponse) { $modules += $record }
    if ($modules.Count -ne $expectedModuleCount) {
        throw "Section $section expected $expectedModuleCount modules; found $($modules.Count). No changes made."
    }

    $week2Matches = @($modules | Where-Object { [string]$_.name -eq $moduleTitle })
    if ($week2Matches.Count -ne 1) {
        throw "Section $section expected exactly one '$moduleTitle' module; found $($week2Matches.Count). No changes made."
    }
    $week2 = $week2Matches[0]
    if ($week2.published -ne $true) {
        throw "Section $section '$moduleTitle' must already be published. This wrapper will not publish a module."
    }

    $pageResponse = Invoke-RestMethod -Method Get -Uri $pageSearchUri -Headers $headers
    $pages = @()
    foreach ($record in $pageResponse) {
        if ([string]$record.title -eq $pageTitle) { $pages += $record }
    }
    if ($pages.Count -ne 1 -or [string]$pages[0].url -ne $pageSlug) {
        throw "Section $section did not have exactly one matching Lab 2 page and slug. No changes made."
    }

    $itemsUri = "$canvasBase/api/v1/courses/$courseId/modules/$($week2.id)/items?per_page=100"
    $itemResponse = Invoke-RestMethod -Method Get -Uri $itemsUri -Headers $headers
    $items = @()
    foreach ($record in $itemResponse) {
        if (
            [string]$record.title -eq $pageTitle -and
            [string]$record.type -eq "Page" -and
            [string]$record.page_url -eq $pageSlug
        ) { $items += $record }
    }
    if ($items.Count -ne 1) {
        throw "Section $section expected exactly one Lab 2 page item in '$moduleTitle'; found $($items.Count). No changes made."
    }

    $moduleStates = @(
        $modules |
            Sort-Object position |
            ForEach-Object {
                [pscustomobject]@{
                    id = [long]$_.id
                    name = [string]$_.name
                    published = [bool]$_.published
                }
            }
    )
    $plans += [pscustomobject]@{
        section = [int]$section
        course_id = $courseId
        course_workflow_state = $courseState
        module_id = [long]$week2.id
        module_published = [bool]$week2.published
        page_published = [bool]$pages[0].published
        module_item_id = [long]$items[0].id
        module_item_published = [bool]$items[0].published
        planned_mutations = @(
            if ($pages[0].published -ne $true) { "publish_page" }
            if ($items[0].published -ne $true) { "publish_module_item" }
        )
        module_states = $moduleStates
    }
}

$plans | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath (Join-Path $AuditRoot "plan.json") -Encoding UTF8
$plans |
    Select-Object section, course_id, course_workflow_state, module_published, page_published, module_item_published, @{ Name = "planned_mutations"; Expression = { @($_.planned_mutations).Count } } |
    Format-Table -AutoSize

if (-not $Execute) {
    Write-Output "Plan only: no Canvas mutations were made."
    Write-Output "Audit directory: $AuditRoot"
    return
}

$receipts = @()
foreach ($plan in $plans) {
    $courseId = [long]$plan.course_id
    $pageUri = "$canvasBase/api/v1/courses/$courseId/pages/$([Uri]::EscapeDataString($pageSlug))"
    $itemUri = "$canvasBase/api/v1/courses/$courseId/modules/$($plan.module_id)/items/$($plan.module_item_id)"
    $mutations = 0

    if ($plan.page_published -ne $true) {
        $result = Invoke-RestMethod -Method Put -Uri $pageUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "wiki_page[published]" = "true" }
        if ($result.published -ne $true) { throw "Section $($plan.section) did not confirm Lab 2 page publication." }
        $mutations += 1
    }
    if ($plan.module_item_published -ne $true) {
        $result = Invoke-RestMethod -Method Put -Uri $itemUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "module_item[published]" = "true" }
        if ($result.published -ne $true) { throw "Section $($plan.section) did not confirm Lab 2 module-item publication." }
        $mutations += 1
    }

    $course = Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId" -Headers $headers
    if ([string]$course.workflow_state -ne [string]$plan.course_workflow_state) {
        throw "Section $($plan.section) course workflow state changed during publication."
    }

    $moduleResponse = Invoke-RestMethod -Method Get -Uri "$canvasBase/api/v1/courses/$courseId/modules?per_page=100" -Headers $headers
    $modules = @()
    foreach ($record in $moduleResponse) { $modules += $record }
    foreach ($before in $plan.module_states) {
        $after = @($modules | Where-Object { [long]$_.id -eq [long]$before.id })
        if ($after.Count -ne 1 -or [bool]$after[0].published -ne [bool]$before.published) {
            throw "Section $($plan.section) module publication state changed for '$($before.name)'."
        }
    }

    $page = Invoke-RestMethod -Method Get -Uri $pageUri -Headers $headers
    $item = Invoke-RestMethod -Method Get -Uri $itemUri -Headers $headers
    if ($page.published -ne $true -or $item.published -ne $true) {
        throw "Section $($plan.section) Lab 2 page or module item is still unpublished."
    }

    $receipts += [pscustomobject]@{
        section = [int]$plan.section
        course_id = $courseId
        course_workflow_state = [string]$course.workflow_state
        page_published = [bool]$page.published
        module_item_published = [bool]$item.published
        module_states_preserved = $true
        canvas_mutations = $mutations
    }
}

$receipts | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath (Join-Path $AuditRoot "receipt.json") -Encoding UTF8
$receipts | Format-Table section, course_id, course_workflow_state, page_published, module_item_published, module_states_preserved, canvas_mutations -AutoSize
Write-Output "Verified Lab 2 page publication for $($receipts.Count) section(s)."
Write-Output "Audit directory: $AuditRoot"
