[CmdletBinding()]
param(
    [string]$ArchivePath,
    [string]$OutputDirectory,
    [switch]$Force
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$scriptDirectory = Split-Path -Parent $MyInvocation.MyCommand.Path
$repository = [System.IO.Path]::GetFullPath((Join-Path $scriptDirectory "..\.."))

if (-not $ArchivePath) {
    $ArchivePath = Join-Path $repository "canvas\data-analysis-for-business-applications-s26-12-export.imscc"
}
if (-not $OutputDirectory) {
    $OutputDirectory = Join-Path $repository "shared\guided-notes"
}

$ArchivePath = [System.IO.Path]::GetFullPath($ArchivePath)
$OutputDirectory = [System.IO.Path]::GetFullPath($OutputDirectory)

if (-not (Test-Path -LiteralPath $ArchivePath -PathType Leaf)) {
    throw "Canvas export was not found: $ArchivePath"
}

$outputRoot = [System.IO.Path]::GetFullPath((Join-Path $repository "shared\guided-notes"))
if ($OutputDirectory -ne $outputRoot) {
    throw "OutputDirectory must be the repository guided-notes directory: $outputRoot"
}

$files = [ordered]@{
    "web_resources/Guides_Tutorials/DataLiteracyGuide - Student Version.docx" = "Guided_Notes_01_Data_Literacy.docx"
    "web_resources/Quantitative Concepts Study Guide - Part 2.docx" = "Guided_Notes_02_Descriptive_Statistics_and_Data_Literacy.docx"
    "web_resources/DataVisualizationGuide - Student Version.docx" = "Guided_Notes_03_Data_Visualization.docx"
    "web_resources/Guides_Tutorials/week4_study_guide.docx" = "Guided_Notes_04_Communicating_Data.docx"
    "web_resources/Hypothesis Testing - Student Version.docx" = "Guided_Notes_04_Hypothesis_Testing.docx"
    "web_resources/Inferential Statistical Tests - Student Version.docx" = "Guided_Notes_05_Inferential_Statistical_Tests.docx"
    "web_resources/Guides_Tutorials/SurveyMethodsGuide.docx" = "Guided_Notes_06_Survey_Research_Methods.docx"
    "web_resources/Guides_Tutorials/Expermiental Research Machine Learning Guided Notes.docx" = "Guided_Notes_07_Machine_Learning_and_Experimental_Research.docx"
}

Add-Type -AssemblyName System.IO.Compression.FileSystem
New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null

$archive = [System.IO.Compression.ZipFile]::OpenRead($ArchivePath)
try {
    $entries = @{}
    foreach ($entry in $archive.Entries) {
        $entries[$entry.FullName] = $entry
    }

    foreach ($sourcePath in $files.Keys) {
        if (-not $entries.ContainsKey($sourcePath)) {
            throw "Required guided-notes file is missing from the Canvas export: $sourcePath"
        }

        $destination = Join-Path $OutputDirectory $files[$sourcePath]
        if ((Test-Path -LiteralPath $destination) -and -not $Force) {
            throw "Destination already exists; use -Force only to refresh it from the same export: $destination"
        }

        $sourceStream = $entries[$sourcePath].Open()
        try {
            $destinationStream = [System.IO.File]::Open(
                $destination,
                [System.IO.FileMode]::Create,
                [System.IO.FileAccess]::Write,
                [System.IO.FileShare]::None
            )
            try {
                $sourceStream.CopyTo($destinationStream)
            }
            finally {
                $destinationStream.Dispose()
            }
        }
        finally {
            $sourceStream.Dispose()
        }
    }
}
finally {
    $archive.Dispose()
}

$results = foreach ($sourcePath in $files.Keys) {
    $destination = Join-Path $OutputDirectory $files[$sourcePath]
    [pscustomobject]@{
        source = $sourcePath
        output = [System.IO.Path]::GetRelativePath($repository, $destination).Replace("\", "/")
        bytes = (Get-Item -LiteralPath $destination).Length
        sha256 = (Get-FileHash -LiteralPath $destination -Algorithm SHA256).Hash.ToLowerInvariant()
    }
}

$results | Format-Table -AutoSize

