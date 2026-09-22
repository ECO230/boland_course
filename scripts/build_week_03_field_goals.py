"""Build Week 3's legacy-field-name extract from the pinned NFL course release."""
import argparse
import hashlib
import json
from pathlib import Path
import duckdb

ROOT = Path(__file__).resolve().parent.parent
RELEASE_URL = 'https://data.60land.com/project1/2026-fall/v1/nfl/nfl_plays.parquet'

def sha256(path):
    with path.open('rb') as source:
        return hashlib.file_digest(source, 'sha256').hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, default=ROOT/'.data-build/nfl/nfl-complete-2021-2025-v1/release/nfl_plays.parquet')
    args = parser.parse_args()
    source = args.source.resolve()
    expected = source.with_suffix('.parquet.sha256').read_text().split()[0]
    if sha256(source) != expected:
        raise ValueError('Course release checksum mismatch')
    con = duckdb.connect()
    con.read_parquet(str(source)).create_view('source_plays')
    con.execute('CREATE VIEW attempts AS SELECT * FROM source_plays WHERE season=2025 AND season_type IN (\'REG\',\'POST\') AND field_goal_attempt AND play_type=\'field_goal\'')
    assert con.execute('SELECT count(*)=count(DISTINCT (game_id,play_id)) FROM attempts').fetchone()[0]
    assert con.execute("SELECT count(*) FROM attempts WHERE field_goal_result NOT IN ('made','missed','blocked') OR field_goal_result IS NULL OR yardline_100 IS NULL OR game_clock IS NULL OR week IS NULL OR quarter IS NULL").fetchone()[0] == 0
    # Original slide filters, fields, and result labels stay unchanged.
    data = con.execute('''SELECT
        game_id AS gameId, play_id AS playId, play_description AS playDescription,
        quarter, down, yards_to_go AS yardsToGo, possession_team AS possessionTeam,
        'Field Goal' AS specialTeamsPlayType,
        CASE field_goal_result WHEN 'made' THEN 'Kick Attempt Good'
          WHEN 'missed' THEN 'Kick Attempt No Good'
          WHEN 'blocked' THEN 'Blocked Kick Attempt' END AS specialTeamsResult,
        kicker_player_id AS kickerId, side_of_field AS yardlineSide,
        least(yardline_100,100-yardline_100) AS yardlineNumber,
        game_clock || ':00' AS gameClock, kick_distance AS kickLength,
        kicker_player_name AS displayName, season, week, game_date AS gameDate,
        home_team AS homeTeamAbbr, away_team AS visitorTeamAbbr, season_type AS seasonType
        FROM attempts ORDER BY game_id, play_id''').fetchall()
    names = [col[0] for col in con.description]
    import csv
    output = ROOT/'shared/data/nfl_field_goals_2025.csv'
    with output.open('w', newline='', encoding='utf-8') as target:
        writer = csv.writer(target, lineterminator='\n')
        writer.writerow(names)
        writer.writerows(data)
    counts = dict(con.execute('SELECT field_goal_result,count(*) FROM attempts GROUP BY 1 ORDER BY 1').fetchall())
    metadata = {
        'dataset_id': 'nfl_complete_2021_2025', 'release_version': '2026-fall-v1',
        'source_url': RELEASE_URL, 'source_sha256': expected,
        'upstream': 'https://github.com/nflverse/nflverse-data',
        'scope': '2025 season; REG and POST; all teams; includes overtime and blocked attempts',
        'rows': len(data), 'results': counts,
        'season_types': dict(con.execute('SELECT season_type,count(*) FROM attempts GROUP BY 1 ORDER BY 1').fetchall()),
        'output_sha256': sha256(output),
    }
    output.with_suffix('.provenance.json').write_text(json.dumps(metadata, indent=2)+'\n', encoding='utf-8')
    print(json.dumps(metadata, indent=2))

if __name__ == '__main__':
    main()
