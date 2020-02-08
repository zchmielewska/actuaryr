#' First working day of an year
#'
#' Returns the first working day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fwdoy("2022-09-21")
dref_fwdoy <- function(date) {
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

  fdoy <- dref_fdoy(date)
  wday <- as.POSIXlt(fdoy)$wday # 0 = Sunday

  if(wday == 6) {
    result <- fdoy + lubridate::days(2)
  } else if (wday == 0) {
    result <- fdoy + lubridate::days(1)
  } else {
    result <- fdoy
  }

  return(result)

}
