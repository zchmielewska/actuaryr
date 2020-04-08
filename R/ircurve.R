
create_ircurve <- function(x = double(), type = "forward", period = "year") {
  type   <- match.arg(type, c("forward", "spot"))
  period <- match.arg(period, c("month", "year"))
  
  ircurve <- structure(x, 
                      class = "ircurve",
                      type = type,
                      period = period)
  return(ircurve)
}

print.ircurve <- function(ircurve) {
  cat(crayon::silver(paste0("Interest rate curve\nType: ", attributes(ircurve)$type, "\nPeriod: ", attributes(ircurve)$period, "\nRates:\n")))
  as.vector(ircurve)
}

ircurve <- create_ircurve(rep(0.01, 10))

print(ircurve)
