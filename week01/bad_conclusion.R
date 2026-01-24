
install.packages(c("readr","dplyr","tidyr","lubridate","stringr","glue","here"))  # if needed
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(glue)
library(here)


# --- Adjust the path to where your file lives ---
data_path <- "../shared/data/airbnb_chicago.csv"  # <-- your uploaded file name  (same columns as your dataset)  # [1](blob:https://m365.cloud.microsoft/64654bcd-b374-425e-846c-62fb20cdff3c)

# 1) Load data --------------------------------------------------------------
abnb <- read_csv(here("shared", "data", "airbnb_chicago.csv"), show_col_types = FALSE)


# 2) Coerce instant_bookable into logical (t/f/TRUE/FALSE variants) --------
abnb <- abnb %>%
  mutate(instant_bookable = tolower(as.character(instant_bookable)) %in% c("t","true"))

# 3) Parse the three date-like columns (WRONG: we will treat these as bookings)
#    These are listing/host timeline fields, NOT actual stay dates. (That’s the point!)
abnb <- abnb %>%
  mutate(
    first_review = suppressWarnings(mdy(first_review)),
    host_since   = suppressWarnings(mdy(host_since)),
    last_review  = suppressWarnings(mdy(last_review))
  )

# 4) Stack the three columns into one fake "booking date" -------------------
#    🚩 WRONG: we’re pretending each row *is a booking*, and each of these dates is a booking timestamp.
long_wrong <- abnb %>%
  select(
    id, listed_price, review_scores_rating, instant_bookable,
    first_review, host_since, last_review
  ) %>%
  pivot_longer(
    cols = c(first_review, host_since, last_review),
    names_to   = "which_date",
    values_to  = "fake_booking_date"
  ) %>%
  filter(!is.na(fake_booking_date))

# 5) Derive a naive season from the calendar month -------------------------
to_season <- function(m) {
  if (m %in% c(12, 1, 2))  return("Winter")
  if (m %in% c(3, 4, 5))   return("Spring")
  if (m %in% c(6, 7, 8))   return("Summer")
  return("Fall")
}


long_wrong <- long_wrong %>%
  mutate(
    Season = case_when(
      month(fake_booking_date) %in% c(12, 1, 2) ~ "Winter",
      month(fake_booking_date) %in% c(3, 4, 5)  ~ "Spring",
      month(fake_booking_date) %in% c(6, 7, 8)  ~ "Summer",
      TRUE                                      ~ "Fall"
    )
  )


# 6) Build the gloriously wrong seasonal summary ---------------------------
#    - "bookings" = count of stacked rows
#    - "rating_dollars" = rating * price  (🚩 unit mashup)
season_summary <- long_wrong %>%
  group_by(Season) %>%
  summarise(
    bookings      = n(),
    avg_price     = mean(listed_price, na.rm = TRUE),
    avg_rating    = mean(review_scores_rating, na.rm = TRUE),
    instant_share = mean(instant_bookable, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    rating_dollars = avg_rating * avg_price  # 🚩 nonsense cross-units
  ) %>%
  arrange(desc(rating_dollars))

# 7) Print the confidently wrong conclusion --------------------------------
best_season <- season_summary %>% slice(1) %>% pull(Season)

cat(glue("
CONFIDENT (but wrong) Conclusion:
Enable Instant Book in {best_season} — it's the unequivocally best season for superior ratings and returns.\n
"))

# 8) Show the bogus “evidence” table ---------------------------------------
season_summary %>%
  mutate(
    avg_price     = round(avg_price, 2),
    avg_rating    = round(avg_rating, 2),
    instant_share = round(100 * instant_share, 2),
    rating_dollars= round(rating_dollars, 2)
  ) %>%
  select(Season, bookings, avg_price, avg_rating, instant_share, rating_dollars) %>%
  print(n = Inf)


# 20 rows that should raise eyebrows: these are properties/listings, not bookings.
head_20_flags <- abnb %>%
  select(
    # IDs & label-like fields that scream "listing"
    id, name, property_type, room_type, neighbourhood, zipcode,
    # Capacity-level fields (not per-stay)
    accommodates, bedrooms, beds,
    # The three date fields we are misusing as bookings (🚩)
    host_since, first_review, last_review,
    # Aggregated stats at the listing level (🚩 not a single-stay field)
    number_of_reviews, review_scores_rating,
    # Price & toggles
    listed_price, instant_bookable,
    
  ) %>%
  slice_head(n = 20)

head_20_flags

