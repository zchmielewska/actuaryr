
as_ircurve <- function(rates = double(), type = "forward", period = "year") {
  type   <- match.arg(type, c("forward", "spot"))
  period <- match.arg(period, c("month", "year"))
  
  ircurve <- structure(rates, 
                      class = "ircurve",
                      type = type,
                      period = period)
  return(ircurve)
}

change_type <- function(ircurve, to_type) {
  if(!inherits(ircurve, "ircurve")) ircurve <- as_ircurve(ircurve)
  to_type   <- match.arg(to_type, c("forward", "spot"))
  from_type <- attributes(ircurve)$type
  
  if(from_type == to_type) return(ircurve)
  
  if(from_type == "forward" & to_type == "spot") {
    forward <- ircurve
    spot    <- as_ircurve(rates = 0)
    attributes(spot)$type <- to_type
    
    for (n in seq_along(forward)) {
      spot[n] <- prod(1+forward[1:n])^(1/n)-1
    }
    return(spot)
  }
  
  if(from_type == "spot" & to_type == "forward") {
    spot    <- ircurve
    forward <- as_ircurve(rates = 0)
    attributes(forward)$type <- to_type
    
    for (n in seq_along(spot)) {
      if(n == 1) {
        forward[n] <- spot[n]
      } else {
        forward[n] <- (1+spot[n])^(n)/prod(1+forward[1:(n-1)]) - 1
      }
    }
    return(forward)
  }
}
