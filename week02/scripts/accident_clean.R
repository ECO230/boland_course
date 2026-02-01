library(tidyverse)
library(lubridate)
library(gt)

# Reusable GT slide theme
source(here("shared","scripts","gt_boland.R"), local = TRUE)

# ---- Load data ONCE at startup ----
COURSE_ROOT <- "/data/junior/boland_course"
acc_path <- file.path(COURSE_ROOT, "shared", "data", "accident_wi.csv")

accidents_wi <- read_csv(acc_path, show_col_types = FALSE)  %>%
  mutate(
    # Clean raw time strings (handle blanks + extra whitespace)
    Start_Time_clean = na_if(str_squish(Start_Time), ""),
    End_Time_clean   = na_if(str_squish(End_Time), ""),
    
    # Robust parse (handles "m/d/Y H:M" and "m/d/Y H:M:S")
    start_time = parse_date_time(
      Start_Time_clean,
      orders = c("mdy HM", "mdy HMS"),
      tz = "America/Chicago"
    ),
    end_time = parse_date_time(
      End_Time_clean,
      orders = c("mdy HM", "mdy HMS"),
      tz = "America/Chicago"
    ),
    
    # ---- Season from start_time ----
    season = case_when(
      month(start_time) %in% 3:5  ~ "spring",
      month(start_time) %in% 6:8  ~ "summer",
      month(start_time) %in% 9:11 ~ "fall",
      TRUE                        ~ "winter"
    ),
    
    # ---- Time-of-day bins from start_time ----
    time_of_day = case_when(
      hour(start_time) >= 6  & hour(start_time) < 12 ~ "morning",  # 06:00–11:59
      hour(start_time) >= 12 & hour(start_time) < 18 ~ "daytime",  # 12:00–17:59
      hour(start_time) >= 18 & hour(start_time) < 24 ~ "evening",  # 18:00–23:59
      TRUE                                           ~ "night"     # 00:00–05:59
    ),
    
    # ---- Duration: end_time - start_time ----
    duration = end_time - start_time,                       # difftime
    duration_mins = as.numeric(duration, units = "mins"),    # numeric minutes
    duration_hrs  = as.numeric(duration, units = "hours"),   # numeric hours
    
    # Optional: ordered factors for nicer plots
    season = factor(season, levels = c("spring", "summer", "fall", "winter")),
    time_of_day = factor(time_of_day, levels = c("night", "morning", "daytime", "evening"))
  ) %>%
  select(ID,Severity,`Distance(mi)`,`Temperature(F)`,`Wind_Chill(F)`,`Humidity(%)`,`Pressure(in)`,`Visibility(mi)`,
         Wind_Direction,`Wind_Speed(mph)`,`Precipitation(in)`,Weather_Condition,Sunrise_Sunset,season,time_of_day,duration_mins)


accidents_wi %>%
  head(30) %>%
  gt() %>%
  gt_boland(base_size = 6.25, tight = TRUE)
