# Week 3 field goal extract

`nfl_field_goals_2025.csv` contains all 1,140 field goal attempts in the pinned
2026-fall-v1 NFL course release for the 2025 season: 1,088 regular-season and
52 postseason attempts. The postseason includes games played in early 2026.
All teams, overtime, and blocked attempts are retained. There are 976 made,
140 missed, and 24 blocked attempts. Deleted plays and nullified no-plays are
not included.

Source: [NFL course dataset](../../datasets/nfl.qmd), derived from
[nflverse-data](https://github.com/nflverse/nflverse-data), CC BY 4.0.
The release URL and SHA-256 checksums are in
`nfl_field_goals_2025.provenance.json`.

Rebuild from the downloaded course release (DuckDB 1.5.2):

```powershell
python scripts/build_week_03_field_goals.py
```

The default source is
`.data-build/nfl/nfl-complete-2021-2025-v1/release/nfl_plays.parquet`.
Alternatively pass `--source` pointing to that release file and keep its
`.parquet.sha256` sidecar beside it. Slides read the committed CSV and need no
network access or DuckDB installation to render.

## Lecture compatibility

| Existing lecture field | Course source / mapping |
| --- | --- |
| `specialTeamsPlayType` | `Field Goal` for the selected field-goal attempts |
| `specialTeamsResult` | made -> `Kick Attempt Good`; missed -> `Kick Attempt No Good`; blocked -> `Blocked Kick Attempt` |
| `quarter` | `quarter` (5 is overtime) |
| `week` | `week`, including postseason weeks 19-22 |
| `yardlineNumber` | `min(yardline_100, 100-yardline_100)`, the numbered field marker (0-50), not kick distance |
| `gameClock` | `game_clock` with `:00` appended, preserving the original `mm:ss:00` format and countdown meaning |

Other identifying fields use the legacy camel-case names where applicable.
`gameId` and `kickerId` now contain nflverse game and GSIS player identifiers;
they are text identifiers, not the old tracking-data numeric IDs. `seasonType`
is included to distinguish regular-season and postseason records. Unused
tracking-only fields from the old CSV are not fabricated.

The lecture's existing filters remain unchanged: Q1-Q4 comparisons omit
overtime; made-only plots use made attempts; the make-rate example includes
blocked attempts in its denominator; the good/no-good comparison omits blocked
attempts. The nominal comparison and practice examples use all 22 weeks. The
correlation example excludes overtime and converts the countdown clock to
seconds elapsed in a 15-minute regulation quarter (900 minus seconds remaining).
Chart types and the instructional slide prose are preserved. The old Crosby CSV remains available for the
other lessons that still reference it.
