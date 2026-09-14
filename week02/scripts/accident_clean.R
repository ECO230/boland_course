WEEK02_SAMPLE_SIZE <- 7900L
WEEK02_SAMPLE_SEED <- 230L

load_week02_crashes <- function(
    course_root,
    sample_size = WEEK02_SAMPLE_SIZE,
    seed = WEEK02_SAMPLE_SEED) {
  crash_path <- file.path(
    course_root,
    "shared",
    "data",
    "early-homework-v1",
    "traffic_chicago_2024.csv"
  )

  crashes <- readr::read_csv(
    crash_path,
    na = c("", "NA"),
    show_col_types = FALSE
  )

  required <- c(
    "crash_id",
    "crash_datetime",
    "time_period",
    "reported_weather_condition",
    "temperature_f",
    "relative_humidity_percent",
    "wind_speed_mph"
  )
  missing_fields <- setdiff(required, names(crashes))
  if (length(missing_fields) > 0) {
    stop(
      "Chicago crash source is missing required fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  if (nrow(crashes) < sample_size) {
    stop(
      "Chicago crash source contains ", nrow(crashes),
      " rows; Week 2 requires ", sample_size, "."
    )
  }

  set.seed(seed)

  crashes %>%
    dplyr::slice_sample(n = sample_size) %>%
    dplyr::mutate(
      crash_datetime = lubridate::ymd_hms(
        crash_datetime,
        tz = "America/Chicago",
        quiet = TRUE
      ),
      season = dplyr::case_when(
        lubridate::month(crash_datetime) %in% 3:5 ~ "Spring",
        lubridate::month(crash_datetime) %in% 6:8 ~ "Summer",
        lubridate::month(crash_datetime) %in% 9:11 ~ "Fall",
        TRUE ~ "Winter"
      ),
      season = factor(
        season,
        levels = c("Winter", "Spring", "Summer", "Fall"),
        ordered = TRUE
      ),
      time_period = factor(
        time_period,
        levels = c(
          "Night or early morning",
          "Morning commute",
          "Daytime",
          "Evening commute"
        ),
        ordered = TRUE
      ),
      weather_status = factor(
        dplyr::if_else(
          is.na(reported_weather_condition) |
            reported_weather_condition == "UNKNOWN",
          "Unknown",
          "Reported"
        ),
        levels = c("Reported", "Unknown")
      ),
      injury_severity = factor(
        dplyr::case_when(
          is.na(injuries_total) &
            is.na(injuries_fatal) &
            is.na(injuries_incapacitating) &
            is.na(injuries_non_incapacitating) ~ NA_character_,
          injuries_fatal > 0 ~ "Fatal",
          injuries_incapacitating > 0 ~ "Incapacitating",
          injuries_non_incapacitating > 0 ~ "Non-incapacitating",
          injuries_total > 0 ~ "Other reported injury",
          TRUE ~ "No reported injury"
        ),
        levels = c(
          "No reported injury",
          "Other reported injury",
          "Non-incapacitating",
          "Incapacitating",
          "Fatal"
        ),
        ordered = TRUE
      )
    ) %>%
    dplyr::arrange(crash_datetime, crash_id)
}
