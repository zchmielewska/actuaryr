new_ircurve <- function(x = double(), type, period) {
  stopifnot(is.double(x))
  type <- match.arg(type, c("forward", "spot"))
  period <- match.arg(period, c("month", "year"))
  structure(x, 
            class = "ircurve",
            type = type,
            period = period)
}

ircurve <- function(x, type = "forward", period = "month") {
  new_ircurve(x, type, period)
}

ircurve(x = c(1, 2, 3))
