#' Quarter to date
#' 
#' Returns the last day of the previous quarter.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_qtd("2020-09-21")
dref_qtd <- function(date) {
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
  
  result <- dref_fdoq(date) + lubridate::days(-1)
  return(result)
}