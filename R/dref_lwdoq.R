#' Last working day of a quarter
#'
#' Returns the last working day of a quarter in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_lwdoq("2020-09-21")
dref_lwdoq <- function(date) {
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

  ldoq <- dref_ldoq(date)
  wday <- as.POSIXlt(ldoq)$wday # 0 = Sunday
  if(wday == 0) {
    result <- ldoq + lubridate::days(-2)
  } else if (wday == 6) {
    result <- ldoq + lubridate::days(-1)
  } else {
    result <- ldoq
  }

  return(result)
}
