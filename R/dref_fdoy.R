#' First day of an year
#'
#' Returns the first day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fdoy("2020-09-21")
dref_fdoy <- function(date) {
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

  year  <- lubridate::year(date)
  result <- lubridate::make_date(year = year, month = 1, day = 1)
  return(result)
}
