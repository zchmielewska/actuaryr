#' First working day of a month
#'
#' Returns the first working day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fwdom("2020-11-21")
dref_fwdom <- function(date) {
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

  fdom <- dref_fdom(date)
  wday <- as.POSIXlt(fdom)$wday # 0 = Sunday
  if(wday == 6) {
    result <- fdom + lubridate::days(2)
  } else if (wday == 0) {
    result <- fdom + lubridate::days(1)
  } else {
    result <- fdom
  }

  return(result)
}
