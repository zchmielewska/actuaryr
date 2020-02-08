#' Last day of a month
#'
#' Returns the last day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_ldom("2020-09-21")
dref_ldom <- function(date) {
  if(!inherits(date, "Date")) {
    tryCatch(
      expr = {
        date <- as.Date(date)
      },
      error = function(e) {
        stop(paste0("Can't coerce ", date, " to date."), call. = FALSE)
      }
    )
  }

  month <- lubridate::month(date)
  year  <- lubridate::year(date)

  # nm = next month
  if (month < 12) {
    nm_month <- month + 1
    nm_year <- year
  } else {
    nm_month <- 1
    nm_year <- year + 1
  }
  result <- lubridate::make_date(year = nm_year, month = nm_month, day = 1) + lubridate::days(-1)
  return(result)
}
