
# install.packages(c("dplyr","scales","glue"))  # if needed
library(dplyr)
library(scales)
library(glue)

# If `sleep` is already in memory, keep this. Otherwise read it first:
# sleep <- readr::read_csv("week01/data/sleep_tracking_Jan-Jun_2025_with_zip.csv", show_col_types = FALSE)

june_glamping <- sleep %>%
  filter(Month == "June", Location == "Camping")
# If you want only main sleeps, uncomment:
# june_glamping <- june_glamping %>% filter(SleepEventType == "Main")

mean_minutes   <- mean(june_glamping$TotalMinutes, na.rm = TRUE)
median_minutes <- median(june_glamping$TotalMinutes, na.rm = TRUE)
avg_quality    <- mean(june_glamping$SleepQuality, na.rm = TRUE)
n_records      <- nrow(june_glamping)

summary_vertical <- tibble::tibble(
  Summary = c(
    "June Glamping:",
    glue(""),
    glue("Mean = {sprintf('%.2f', mean_minutes)} Minutes"),
    glue("Median = {round(median_minutes)} Minutes"),
    glue("Quality = {percent(avg_quality, accuracy = 0.01)}"),
    glue("N Records = {comma(n_records)}")
  )
)

summary_vertical

head(sleep)
