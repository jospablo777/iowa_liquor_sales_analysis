# ============================================================
# Several functions to process our analysis data
# ============================================================

#' Name week days
#'
#' Takes a vector of dates and return the vector of names of the week days which corresponds the vector.
#'
#' @param date_vector An atomic vector. It contain dates at a day granularity.
#'
#' @return A string vector. The string name of the week day which corresponds to the date.
#' @export
#'
#' @examples
#' some_dates <- c(as.Date("2020-01-01"), as.Date("2020-01-02"), as.Date("2020-01-03"))
#' name_days(some_dates)
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


#' Is weekend?
#'
#' Returns a 1 if the day of the week is weekend (Friday, Saturday, or Sunday), 0 if it is a week day.
#'
#' @param date_vector An atomic vector. It contain dates at a day granularity.
#'
#' @return An int vector. The value will be 1 for weekend days, 0 for week days.
#' @export
#'
#' @examples
#' some_dates <- c(as.Date("2020-01-01"), as.Date("2020-01-02"), as.Date("2020-01-03"))
#' is_weekend(some_dates)
is_weekend <- function(date_vector) {
  if_else(wday(date_vector, week_start = 1) %in% c(5, 6, 7), 1, 0)
}
