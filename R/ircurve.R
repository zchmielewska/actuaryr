
as_ircurve <- function(x, type = "forward", period = "year") {
  type   <- match.arg(type, c("forward", "spot"))
  period <- match.arg(period, c("month", "year"))
  
  result <- structure(x, 
                      class = "ircurve",
                      type = type,
                      period = period)
  return(result)
}

y <- as_ircurve(1:3, type = "spot")

g <- function(x) {
  x <- 10
  y <- 10
  UseMethod("g")
}

g.default <- function(x) c(x = x, y = y)

