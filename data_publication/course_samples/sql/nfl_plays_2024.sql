COPY (
    WITH eligible AS (
        SELECT
            game_id || '-' || CAST(play_id AS VARCHAR) AS play_id,
            game_id,
            game_date,
            week,
            home_team,
            away_team,
            possession_team,
            defensive_team,
            quarter,
            down,
            yards_to_go,
            yardline_100,
            game_seconds_remaining,
            play_type,
            yards_gained,
            shotgun,
            no_huddle,
            qb_dropback,
            qb_scramble,
            pass_length,
            pass_location,
            air_yards,
            yards_after_catch,
            run_location,
            run_gap,
            complete_pass,
            first_down,
            sack,
            qb_hit,
            interception,
            fumble_lost,
            penalty,
            penalty_yards,
            touchdown,
            field_goal_attempt,
            field_goal_result,
            kick_distance,
            special_teams_play,
            possession_team_score,
            defensive_team_score,
            score_differential,
            expected_points_before,
            expected_points_added,
            possession_team_win_probability,
            win_probability_added,
            successful_play,
            passer_player_name,
            receiver_player_name,
            rusher_player_name,
            kicker_player_name
        FROM read_parquet('{{SOURCE}}')
        WHERE season = 2024
          AND season_type = 'REG'
          AND is_play = true
    )
    SELECT *
    FROM eligible
    ORDER BY md5(play_id || '{{SAMPLE_SALT}}')
    LIMIT {{ROW_LIMIT}}
) TO '{{OUTPUT}}' (FORMAT CSV, HEADER true);
