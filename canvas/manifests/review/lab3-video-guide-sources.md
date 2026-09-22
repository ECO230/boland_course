# Lab 3 video guide source review

Prepared September 21, 2026 from the instructor's nine recordings, their
English automatic captions, the current Lab 3 task page, and the supplied
`Lab3_Completed.twbx`. All nine caption tracks were ready when retrieved.

Workbook SHA-256:
`8b3232add0971515af045edfef82acd9088fd0be1a5af92ee96dbabc93cc6ad8`.
The packaged workbook contains seven worksheets and a Hyper extract. It was
read as a ZIP/XML reference; it was not altered or uploaded as a student solution.

| Recording | Entry ID | Caption asset |
| --- | --- | --- |
| Start | `1_m54bimsj` | `1_jbt2l1h3` |
| Ranking | `1_9nbvv17j` | `1_hzf2021c` |
| Nominal | `1_6boale46` | `1_43um1gsn` |
| Time | `1_5rlmqfrg` | `1_2jbb1nrn` |
| Part-to-whole | `1_ktwl7eh0` | `1_mhwcqm20` |
| Relationship | `1_iv34qksm` | `1_gobyod9x` |
| Distribution | `1_inchbtz6` | `1_g7kj2piz` |
| Maps | `1_6n4dxamo` | `1_v6g86h20` |
| Excel | `1_2otopmwa` | `1_h33gxax7` |

Caption SRT files, plain-text extracts, workbook XML, and synchronization
receipts are in ignored `canvas/work/lab3-guide/`. Re-run
`../eco230-canvas-ops/canvas/scripts/pull-lab3-captions.py` to refresh available tracks. Public
playback-session values are held in memory and are not recorded by that helper.

## Differences reconciled in the student guide

The instructor subsequently confirmed that the recorded walkthrough is the
in-class lab. The Lab 3 prompt and guide now both use the demonstrated quarter,
temperature, and count-at-least-30 choices. The original comparison below
records the source audit; its references to time_period, visibility_miles,
and matching the ranking's cause selection are superseded. Lab Prep 3 now
starts in Excel's Instructions worksheet before the Tableau practice.

- Ranking saves a manual selection of ten causes. It is not a dynamic Top N rule.
- Nominal Comparison removes the ranking selection, uses average injuries,
  filters count to at least 30, and excludes NOT APPLICABLE. The lab asks for
  the same selected causes as the ranking, so the broader video version is
  identified as a demonstration variation.
- Time Series uses continuous months and colors by reported weather condition.
  The saved sheet excludes CLEAR; the recording also discusses UNKNOWN.
- Part to Whole uses discrete quarter, excludes null injury classifications,
  and hides NO INDICATION OF INJURY and REPORTED, NOT EVIDENT. The guide explains
  the hidden denominator and separately gives the lab's time_period version.
- Relationship disables aggregation, panels by station, and excludes null
  station names. The guide explains repeated station-hour weather observations.
- Distribution disables aggregation and compares temperature by reported
  weather condition. The guide gives an explicit visibility_miles adaptation
  for the written lab requirement.
- Map retains the latitude/longitude point map with crash_id on Detail and
  trafficway_type on Color. ZIP-code mapping is an optional video extension.
- Excel uses PivotCharts for grouped summaries and a regular XY chart for the
  raw temperature/dew-point pairs.

Automatic-transcription errors and verbal imprecision were not copied into
instructions. The guide distinguishes discrete/continuous from
dimension/measure, packages local data in .twbx, uses percentiles for box
boundaries, and does not equate sampled counts with exposure-adjusted risk.

## Rendering and deployment

The Canvas operations renderer now supports `video_placement: inline` with
exactly one `<!-- video:ENTRY_ID -->` slot for every metadata entry. Missing,
duplicate, and unknown slots fail rendering. Existing pages keep their prior
top-of-page video layout. The implementation is in the companion
`eco230-canvas-ops/src/eco230_canvas/content.py`; regression tests are in
`../eco230-canvas-ops/canvas/scripts/tests/test_inline_canvas_videos.py`.

Use `../eco230-canvas-ops/canvas/scripts/sync-lab3-video-guide.ps1` for preflight and append
`-Execute` to apply. It renders the tracked source, validates nine player/link
pairs and section anchors, creates only the guide and its placement after
Lab 3, and verifies existing publication states and item order. New page/item
publication follows the existing Lab 3 page/item. All three Lab 3 pages were
unpublished during this run, so the guides were created unpublished.
