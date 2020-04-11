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

change_period <- function(ircurve, to_period) {
  if(!inherits(ircurve, "ircurve")) ircurve <- as_ircurve(ircurve)
  to_period   <- match.arg(to_period, c("month", "year"))
  from_period <- attributes(ircurve)$period
  type        <- attributes(ircurve)$type
  
  if(from_period == to_period) return(ircurve)
  
  # from = month; to = year | forward
  if(to_period == "year"  & type == "forward") {
    ircurve_month <- ircurve
    
    # curve must include full year data
    if((length(ircurve_month) %% 12) != 0) {
      warning(paste0("The last available rate has been assumed for the months remaining to full year."))
      last_rate <- ircurve_month[length(ircurve_month)]
      remaining_months <- 12 - (length(ircurve_month) %% 12)
      ircurve_month <- append(ircurve_month, rep(last_rate, remaining_months))
    }
    
    no_bins    <- length(ircurve_month)/12
    start_bins <- (0:(no_bins-1))*12 +1
    end_bins   <- (1:no_bins)*12
    
    rates_year <- c()
    for(i in 1:no_bins) {
      rates_year[i] <- prod(1+ircurve_month[start_bins[i]:end_bins[i]])-1
    }
    
    result <- as_ircurve(rates_year, type = "forward", period = "year")
  }
  
  # from = year; to = month | forward
  if(to_period == "month" & type == "forward") { 
    ircurve_year <- ircurve
    
    rates_month <- c()
    for(i in seq_along(ircurve_year)) {
      monthly_rate <- (1+ircurve_year[i])^(1/12)-1
      rates_month[(1:12)+12*(i-1)] <- rep(monthly_rate, 12)
    }
    
    result <- as_ircurve(rates_month, type = "forward", period = "month")
  }
  
  # from = month; to = year | spot
  if(to_period == "year"  & type == "spot") { 
    ircurve_month <- ircurve
    
    # curve must include full year data
    if((length(ircurve_month) %% 12) != 0) {
      warning(paste0("The last available rate has been assumed for the months remaining to full year."))
      last_rate <- ircurve_month[length(ircurve_month)]
      remaining_months <- 12 - (length(ircurve_month) %% 12)
      ircurve_month <- append(ircurve_month, rep(last_rate, remaining_months))
    }
    
    no_bins    <- length(ircurve_month)/12
    end_bins   <- (1:no_bins)*12
    
    rates_year <- c()
    for(i in 1:no_bins) {
      rates_year[i] <- (1+ircurve_month[end_bins[i]])^(12)-1
    }
    
    result <- as_ircurve(rates_year, type = "spot", period = "year")
  }
  
  # from = year; to = month | spot
  if(to_period == "month" & type == "spot") { 
    stop("Not developed yet.")
    ircurve_year <- ircurve
    
    rates_month <- c()
    for(i in seq_along(ircurve_year)) {
      monthly_rate <- (1+ircurve_year[i])^(1/12)-1
      rates_month[(1:12)+12*(i-1)] <- rep(monthly_rate, 12)
    }
    
    result <- as_ircurve(rates_month, type = "spot", period = "month")
  }
  return(result)
}
