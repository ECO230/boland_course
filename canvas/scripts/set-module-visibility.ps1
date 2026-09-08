[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [long]$CourseId,

    [string[]]$PublishedModuleNames = @(
        "Course Info and Resources",
        "Week 1: Intro to Data Analysis"
    ),

    [int]$ExpectedModuleCount = 0,

    [string]$TokenEnvironmentVariable = "CANVAS_TOKEN",

    [string]$AuditDirectory,

    [switch]$PublishSelectedContent,

    [switch]$Execute
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$token = [Environment]::GetEnvironmentVariable($TokenEnvironmentVariable, "Process")
if ([string]::IsNullOrWhiteSpace($token)) {
    throw "The $TokenEnvironmentVariable environment variable is not set in this PowerShell session."
}

if ($PublishedModuleNames.Count -eq 0) {
    throw "At least one published module name is required."
}

$duplicateNames = @(
    $PublishedModuleNames |
        Group-Object |
        Where-Object { $_.Count -gt 1 } |
        ForEach-Object { $_.Name }
)
if ($duplicateNames.Count -gt 0) {
    throw "PublishedModuleNames contains duplicates: $($duplicateNames -join ', ')"
}

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$repository = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory "..\.."))

if (-not $AuditDirectory) {
    $stamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $AuditDirectory = Join-Path $repository "canvas\work\module-visibility\course-$CourseId-$stamp"
}
$AuditDirectory = [System.IO.Path]::GetFullPath($AuditDirectory)
New-Item -ItemType Directory -Force -Path $AuditDirectory | Out-Null

$headers = @{ Authorization = "Bearer $token" }
$canvasBase = "https:" + "//uwlac.instructure.com"
$moduleUri = "$canvasBase/api/v1/courses/$CourseId/modules?per_page=100"
$courseUri = "$canvasBase/api/v1/courses/$CourseId"

$course = Invoke-RestMethod -Method Get -Uri $courseUri -Headers $headers
if ($course.workflow_state -ne "unpublished") {
    throw "Course $CourseId must remain unpublished while module visibility is staged."
}

$modules = @(Invoke-RestMethod -Method Get -Uri $moduleUri -Headers $headers)
if ($modules.Count -eq 0) {
    throw "Canvas returned no modules for course $CourseId."
}
if ($ExpectedModuleCount -gt 0 -and $modules.Count -ne $ExpectedModuleCount) {
    throw "Expected $ExpectedModuleCount Canvas modules; found $($modules.Count). No changes made."
}

$moduleNames = @($modules | ForEach-Object { [string]$_.name })
$missingNames = @($PublishedModuleNames | Where-Object { $_ -notin $moduleNames })
if ($missingNames.Count -gt 0) {
    throw "Modules requested for publication were not found: $($missingNames -join ', '). No changes made."
}

$ambiguousNames = @()
foreach ($publishedName in $PublishedModuleNames) {
    if (@($modules | Where-Object { $_.name -eq $publishedName }).Count -gt 1) {
        $ambiguousNames += $publishedName
    }
}
if ($ambiguousNames.Count -gt 0) {
    throw "Module names are ambiguous in Canvas: $($ambiguousNames -join ', '). No changes made."
}

$before = @(
    $modules |
        Sort-Object position |
        ForEach-Object {
            [pscustomobject]@{
                id = $_.id
                position = $_.position
                name = $_.name
                published = [bool]$_.published
                desired_published = $_.name -in $PublishedModuleNames
            }
        }
)
$before | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $AuditDirectory "before.json")

$changes = @($before | Where-Object { $_.published -ne $_.desired_published })
$before | Format-Table position, name, published, desired_published -AutoSize

if (-not $Execute) {
    Write-Output "Plan only: $($changes.Count) module publication state change(s)."
    Write-Output "Run again with -Execute after reviewing the table."
    Write-Output "Audit directory: $AuditDirectory"
    return
}

foreach ($change in @($changes | Where-Object { -not $_.desired_published })) {
    $updateUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($change.id)"
    Invoke-RestMethod `
        -Method Put `
        -Uri $updateUri `
        -Headers $headers `
        -ContentType "application/x-www-form-urlencoded" `
        -Body @{ "module[published]" = "false" } |
        Out-Null
}

$publishedContentCount = 0
if ($PublishSelectedContent) {
    $publishedContentKeys = New-Object 'System.Collections.Generic.HashSet[string]'
    foreach ($selectedModule in @($modules | Where-Object { $_.name -in $PublishedModuleNames })) {
        $itemsUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($selectedModule.id)/items?per_page=100"
        $moduleItems = @(Invoke-RestMethod -Method Get -Uri $itemsUri -Headers $headers)

        foreach ($item in $moduleItems) {
            $contentKey = "$($item.type):$($item.content_id):$($item.page_url)"
            if ($publishedContentKeys.Add($contentKey)) {
                $contentResponse = $null
                if ($item.type -eq "Assignment") {
                    $contentUri = "$canvasBase/api/v1/courses/$CourseId/assignments/$($item.content_id)"
                    $contentResponse = Invoke-RestMethod -Method Put -Uri $contentUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "assignment[published]" = "true" }
                }
                elseif ($item.type -eq "Page") {
                    $pageUrl = [Uri]::EscapeDataString([string]$item.page_url)
                    $contentUri = "$canvasBase/api/v1/courses/$CourseId/pages/$pageUrl"
                    $contentResponse = Invoke-RestMethod -Method Put -Uri $contentUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "wiki_page[published]" = "true" }
                }
                elseif ($item.type -eq "Discussion") {
                    $contentUri = "$canvasBase/api/v1/courses/$CourseId/discussion_topics/$($item.content_id)"
                    $contentResponse = Invoke-RestMethod -Method Put -Uri $contentUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "published" = "true" }
                }
                elseif ($item.type -eq "Quiz") {
                    $contentUri = "$canvasBase/api/v1/courses/$CourseId/quizzes/$($item.content_id)"
                    $contentResponse = Invoke-RestMethod -Method Put -Uri $contentUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "quiz[published]" = "true" }
                }

                if ($null -ne $contentResponse) {
                    if ($contentResponse.published -ne $true) {
                        throw "Canvas did not confirm publication of $($item.type) '$($item.title)'."
                    }
                    $publishedContentCount += 1
                }
            }

            $itemUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($selectedModule.id)/items/$($item.id)"
            $itemResponse = Invoke-RestMethod -Method Put -Uri $itemUri -Headers $headers -ContentType "application/x-www-form-urlencoded" -Body @{ "module_item[published]" = "true" }
            if ($itemResponse.published -ne $true) {
                throw "Canvas did not confirm publication of module item '$($item.title)'."
            }
        }
    }
}

foreach ($change in @($changes | Where-Object { $_.desired_published })) {
    $updateUri = "$canvasBase/api/v1/courses/$CourseId/modules/$($change.id)"
    Invoke-RestMethod `
        -Method Put `
        -Uri $updateUri `
        -Headers $headers `
        -ContentType "application/x-www-form-urlencoded" `
        -Body @{ "module[published]" = "true" } |
        Out-Null
}

$verifiedModules = @(Invoke-RestMethod -Method Get -Uri $moduleUri -Headers $headers)
$after = @(
    $verifiedModules |
        Sort-Object position |
        ForEach-Object {
            [pscustomobject]@{
                id = $_.id
                position = $_.position
                name = $_.name
                published = [bool]$_.published
                desired_published = $_.name -in $PublishedModuleNames
            }
        }
)
$after | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $AuditDirectory "after.json")

$mismatches = @($after | Where-Object { $_.published -ne $_.desired_published })
if ($mismatches.Count -gt 0) {
    throw "Canvas verification found $($mismatches.Count) module publication-state mismatch(es)."
}

$course = Invoke-RestMethod -Method Get -Uri $courseUri -Headers $headers
if ($course.workflow_state -ne "unpublished") {
    throw "Course $CourseId was unexpectedly published."
}
$after | Format-Table position, name, published -AutoSize
Write-Output "Verified published modules: $(@($after | Where-Object { $_.published }).Count)"
Write-Output "Published selected content objects: $publishedContentCount"
Write-Output "Course state: $($course.workflow_state)"
Write-Output "Audit directory: $AuditDirectory"
