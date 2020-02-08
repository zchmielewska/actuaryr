#' First working day of a quarter
#'
#' Returns the first working day of a quarter in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fwdoq("2020-09-21")
dref_fwdoq <- function(date) {
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

  fdoq <- dref_fdoq(date)
  wday <- as.POSIXlt(fdoq)$wday # 0 = Sunday
  if(wday == 6) {
    result <- fdoq + lubridate::days(2)
  } else if (wday == 0) {
    result <- fdoq + lubridate::days(1)
  } else {
    result <- fdoq
  }
  return(result)
}
