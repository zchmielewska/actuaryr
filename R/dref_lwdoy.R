#' Last working day of an year
#'
#' Returns the last working day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_lwdoy("2022-09-21")
dref_lwdoy <- function(date) {
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

  ldoy <- dref_ldoy(date)
  wday <- as.POSIXlt(ldoy)$wday # 0 = Sunday
  if(wday == 0) {
    result <- ldoy + lubridate::days(-2)
  } else if (wday == 6) {
    result <- ldoy + lubridate::days(-1)
  } else {
    result <- ldoy
  }

  return(result)
}
