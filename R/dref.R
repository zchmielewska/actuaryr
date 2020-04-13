#' Year to date
#' 
#' Returns the last day of the previous year.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_ytd("2020-09-21")
dref_ytd <- function(date) {
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
  
  result <- dref_fdoy(date) + lubridate::days(-1)
  return(result) 
}

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

#' Month to date
#' 
#' Returns the last day of the previous month.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples 
#' dref_mtd("2020-09-21")
dref_mtd <- function(date) {
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
  
  result <- dref_fdom(date) + lubridate::days(-1)
  return(result)
}

#' Last working day of an year
#'
#' Returns the last working day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_lwdoy("2022-09-21")
dref_lwdoy <- function(date) {
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
  
  ldoy <- dref_ldoy(date)
  wday <- as.POSIXlt(ldoy)$wday # 0 = Sunday
  if(wday == 0) {
    result <- ldoy + lubridate::days(-2)
  } else if (wday == 6) {
    result <- ldoy + lubridate::days(-1)
  } else {
    result <- ldoy
  }
  
  return(result)
}

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

#' Last day of an year
#'
#' Returns the last day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_ldoy("2020-09-21")
dref_ldoy <- function(date) {
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
  
  year <- lubridate::year(date)
  result <- lubridate::make_date(year = year, month = 12, day = 31)
  return(result)
}

#' Last day of a quarter
#'
#' Returns the last day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_ldoq("2020-09-21")
dref_ldoq <- function(date) {
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
  
  month <- lubridate::month(date)
  year  <- lubridate::year(date)
  
  # nq = next quarter
  if(month <= 3) {
    nq_month <- 4
    nq_year <- year
  } else if (month <= 6) {
    nq_month <- 7
    nq_year <- year
  } else if (month <= 9) {
    nq_month <- 10
    nq_year <- year
  } else {
    nq_month <- 1
    nq_year <- year + 1
  }
  
  result <- lubridate::make_date(year = nq_year, month = nq_month, day = 1) + lubridate::days(-1)
  return(result)
}

#' Last day of a month
#'
#' Returns the last day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_ldom("2020-09-21")
dref_ldom <- function(date) {
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
  
  month <- lubridate::month(date)
  year  <- lubridate::year(date)
  
  # nm = next month
  if (month < 12) {
    nm_month <- month + 1
    nm_year <- year
  } else {
    nm_month <- 1
    nm_year <- year + 1
  }
  result <- lubridate::make_date(year = nm_year, month = nm_month, day = 1) + lubridate::days(-1)
  return(result)
}

#' First working day of an year
#'
#' Returns the first working day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fwdoy("2022-09-21")
dref_fwdoy <- function(date) {
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
  
  fdoy <- dref_fdoy(date)
  wday <- as.POSIXlt(fdoy)$wday # 0 = Sunday
  
  if(wday == 6) {
    result <- fdoy + lubridate::days(2)
  } else if (wday == 0) {
    result <- fdoy + lubridate::days(1)
  } else {
    result <- fdoy
  }
  
  return(result)
  
}

#' First working day of a quarter
#'
#' Returns the first working day of a quarter in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fwdoq("2020-09-21")
dref_fwdoq <- function(date) {
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
  
  fdoq <- dref_fdoq(date)
  wday <- as.POSIXlt(fdoq)$wday # 0 = Sunday
  if(wday == 6) {
    result <- fdoq + lubridate::days(2)
  } else if (wday == 0) {
    result <- fdoq + lubridate::days(1)
  } else {
    result <- fdoq
  }
  return(result)
}

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

#' First day of an year
#'
#' Returns the first day of a year in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fdoy("2020-09-21")
dref_fdoy <- function(date) {
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
  
  year  <- lubridate::year(date)
  result <- lubridate::make_date(year = year, month = 1, day = 1)
  return(result)
}

#' First day of a quarter
#'
#' Returns the first day of a quarter in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fdoq("2020-09-21")
dref_fdoq <- function(date) {
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
  
  month <- lubridate::month(date)
  year  <- lubridate::year(date)
  
  if(month <= 3) {
    res_month = 1
  } else if (month <= 6) {
    res_month = 4
  } else if (month <= 9) {
    res_month = 7
  } else {
    res_month = 10
  }
  
  result <- lubridate::make_date(year = year, month = res_month, day = 1)
  return(result)
}

#' First day of a month
#'
#' Returns the first day of a month in reference to the base date.
#'
#' @param date base date in format YYYY-MM-DD
#'
#' @return date
#' @export
#'
#' @examples
#' dref_fdom("2020-09-21")
dref_fdom <- function(date) {
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
  
  month <- lubridate::month(date)
  year  <- lubridate::year(date)
  
  result <- lubridate::make_date(year = year, month = month, day = 1)
  return(result)
}
