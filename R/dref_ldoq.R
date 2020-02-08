#' Last day of a quarter
#'
#' Returns the last day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
dref_ldoq <- function(date) {
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

  # nq = next quarter
  if(month <= 3) {
    nq_month <- 4
    nq_year <- year
  } else if (month <= 6) {
    nq_month <- 7
    nq_year <- year
  } else if (month <= 9) {
    nq_month <- 10
    nq_year <- year
  } else {
    nq_month <- 1
    nq_year <- year + 1
  }

  result <- lubridate::make_date(year = nq_year, month = nq_month, day = 1) + lubridate::days(-1)
  return(result)
}
