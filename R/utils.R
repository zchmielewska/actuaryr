.as <- function(x, type) {
  if (type == "integer") {
    result <- as.integer(x)
  } else if (type == "double") {
    result <- as.double(x)
  } else if (type == "character") {
    result <- as.character(x)
  } else {
    stop(paste("Unknown type:", type))
  }
  return(result)
}

.coerceToCharacter <- function(tables) {
  x <- tables$x
  y <- tables$y
  
  # factors, POSIXct and Dates are converted to strings
  for(j in 1:ncol(x)) {
    class <- class(x[, j])[1] # POSIXct inherits from two classes
    if(class == "factor" | class == "Date" | class == "POSIXct") {
      cat(yellow(colnames(x)[j], "from", bold("x"), "has been coerced from", class, "to character."))
      x[, j] <- as.character(x[, j])
    } 
  }
  
  for(j in 1:ncol(y)) {
    class <- class(y[, j])[1]
    if(class == "factor" | class == "Date" | class == "POSIXct") {
      cat(yellow(colnames(y)[j], "from", bold("y"), "has been coerced from", class, "to character."))
      y[, j] <- as.character(y[, j])
    } 
  }
  
  tables <- list(x = x, y = y)
  return(tables)
}

.getCommonColumns <- function(tables) {
  x <- tables$x
  y <- tables$y
  
  not.in.x <- colnames(y)[!c(colnames(y) %in% colnames(x))]
  not.in.y <- colnames(x)[!c(colnames(x) %in% colnames(y))]
  
  if(length(not.in.x) > 0) cat(yellow("Column(s)", not.in.x, "are not in", bold("y"), 
                                      "so they have been removed from comparison."))
  if(length(not.in.y) > 0) cat(yellow("Column(s)", not.in.y, "are not in", bold("x"), 
                                      "so they have been removed from comparison."))
  
  common.columns <- colnames(x)[colnames(x) %in% colnames(y)]
  
  x <- x[, common.columns]
  y <- y[, common.columns]
  
  tables <- list(x = x, y = y)
  return(tables)
}

.getCommonNrRows <- function(tables) {
  x <- tables$x
  y <- tables$y

  n1 <- nrow(x)
  n2 <- nrow(y)
  if(n1 > n2) {
    warning(paste0("x has more rows than y. The last ", n1-n2, " row(s) of x have been removed."))
    x <- x[1:n2, ]
  }
  
  if(n2 > n1) {
    warning(paste0("y has more rows than x. The last ", n2-n1, " row(s) of y have been removed."))
    y <- y[1:n1, ]
  }
  
  tables <- list(x = x, y = y)
  return(tables)
}
