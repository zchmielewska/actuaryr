#' Last working day of a month
#'
#' Returns the last working day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_lwdom("2020-10-21")
dref_lwdom <- function(date) {
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

  ldom <- dref_ldom(date)
  wday <- as.POSIXlt(ldom)$wday # 0 = Sunday
  if(wday == 0) {
    result <- ldom + lubridate::days(-2)
  } else if (wday == 6) {
    result <- ldom + lubridate::days(-1)
  } else {
    result <- ldom
  }

  return(result)
}
