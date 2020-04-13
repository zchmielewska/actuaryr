#' Create an interest rates curve
#'
#' Creates an object of class ircurve which represents in interest rates curve.
#' The curve cosists of interest rates and is of type either "forward" or
#' "spot". The interest rates curve can be for monthly or yearly periods.
#'
#' @param rates vector of rates
#' @param type "forward" or "spot"
#' @param period "year" or "month"
#'
#' @return Interest rates curve, an object of class ircurve.
#' @export
#'
#' @examples
#' as_ircurve(c(0.01, 0.015))
#' as_ircurve(c(0.002, 0.001), type = "spot", period = "month")
as_ircurve <- function(rates = double(), type = "forward", period = "year") {
  type   <- match.arg(type, c("forward", "spot"))
  period <- match.arg(period, c("month", "year"))
  
  ircurve <- structure(rates, 
                      class = "ircurve",
                      type = type,
                      period = period)
  return(ircurve)
}

#' Change the type of an interest rates curve
#'
#' Changes the type of an interest rates curve (object of class ircurve) 
#' from forward to spot or vice versa.
#'
#' @param ircurve an object of class ircurve
#' @param to "forward" or "spot"
#'
#' @return An object of class ircurve with changed type.
#' @export
#'
#' @examples
#' my_ircurve <- as_ircurve(c(0.1, 0.2, 0.3), type = "spot")
#' change_type(my_ircurve, to = "forward")
change_type <- function(ircurve, to) {
  if(!inherits(ircurve, "ircurve")) ircurve <- as_ircurve(ircurve)
  to   <- match.arg(to, c("forward", "spot"))
  from <- attributes(ircurve)$type
  
  if(from == to) return(ircurve)
  
  # from = forward; to = spot
  if(from == "forward" & to == "spot") {
    forward <- ircurve
    spot    <- as_ircurve(rates = 0)
    attributes(spot)$type <- to
    
    for (n in seq_along(forward)) {
      spot[n] <- prod(1+forward[1:n])^(1/n)-1
    }
    return(spot)
  }
  
  # from = spot; to = forward
  if(from == "spot" & to == "forward") {
    spot    <- ircurve
    forward <- as_ircurve(rates = 0)
    attributes(forward)$type <- to
    
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

#' Change the period of an interest rates curve
#'
#' Changes the period of an interest rates curve (object of class ircurve)
#' from year to monthl or vice versa.
#'
#' @param ircurve an object of class ircurve
#' @param to "year" or "month" 
#'
#' @return An object of class ircurve with changed type.
#' @export
#'
#' @examples
#' my_ircurve <- as_ircurve(rep(0.01, 12), period = "month")
#' change_period(my_ircurve, to = "year")
change_period <- function(ircurve, to) {
  if(!inherits(ircurve, "ircurve")) ircurve <- as_ircurve(ircurve)
  to   <- match.arg(to, c("month", "year"))
  from <- attributes(ircurve)$period
  type <- attributes(ircurve)$type
  
  if(from == to) return(ircurve)
  
  # from = month; to = year | forward
  if(to == "year"  & type == "forward") {
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
  if(to == "month" & type == "forward") { 
    ircurve_year <- ircurve
    
    rates_month <- c()
    for(i in seq_along(ircurve_year)) {
      monthly_rate <- (1+ircurve_year[i])^(1/12)-1
      rates_month[(1:12)+12*(i-1)] <- rep(monthly_rate, 12)
    }
    
    result <- as_ircurve(rates_month, type = "forward", period = "month")
  }
  
  # from = month; to = year | spot
  if(to == "year"  & type == "spot") { 
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
  if(to == "month" & type == "spot") { 
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

#' Print ircurve
#'
#' @param ircurve an object of class ircurve
#'
#' @return nothing, prints an ircurve
#' @export
#'
#' @examples
#' my_ircurve <- as_ircurve(c(0.018, 0.02, 0.022))
#' print(my_ircurve)
print.ircurve <- function(ircurve) {
  type   <- attributes(ircurve)$type
  period <- attributes(ircurve)$period
  
  cat("Interest rate curve\n")
  cat(paste("Type:  ", type, "\n"))
  cat(paste("Period:", period, "\n"))
  cat(paste("Rates:\n"))
  print(as.numeric(ircurve))
}