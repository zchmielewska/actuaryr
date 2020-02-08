#' First day of a month
#'
#' Returns the first day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fdom("2020-09-21")
dref_fdom <- function(date) {
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

  result <- lubridate::make_date(year = year, month = month, day = 1)
  return(result)
}
