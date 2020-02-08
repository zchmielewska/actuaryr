#' First day of a quarter
#'
#' Returns the first day of a quarter in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fdoq("2020-09-21")
dref_fdoq <- function(date) {
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

  if(month <= 3) {
    res_month = 1
  } else if (month <= 6) {
    res_month = 4
  } else if (month <= 9) {
    res_month = 7
  } else {
    res_month = 10
  }

  result <- lubridate::make_date(year = year, month = res_month, day = 1)
  return(result)
}
