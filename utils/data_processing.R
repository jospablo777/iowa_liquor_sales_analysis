# ============================================================
# Several functions to process our analysis data
# ============================================================

# Extract the name od the week day
name_days <- function(date_vector){
  week_day <- wday(date_vector, week_start = 1)
  case_when(week_day == 1 ~ 'Monday',
            week_day == 2 ~ 'Tuesday',
            week_day == 3 ~ 'Wednesday',
            week_day == 4 ~ 'Thursday',
            week_day == 5 ~ 'Friday',
            week_day == 6 ~ 'Saturday',
            week_day == 7 ~ 'Sunday',
            .default = 'Uknown')
}

# A label for weekends
is_weekend <- function(date_vector) {
  if_else(wday(date_vector, week_start = 1) %in% c(5, 6,7), 1, 0)
}
