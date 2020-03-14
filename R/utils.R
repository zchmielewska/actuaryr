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
      cat(crayon::yellow(paste0("Column '", colnames(x)[j], "' in ", crayon::bold("LHS"), 
                                " has been coerced from ", class, " to character.\n")))
      x[, j] <- as.character(x[, j])
    } 
  }
  
  for(j in 1:ncol(y)) {
    class <- class(y[, j])[1]
    if(class == "factor" | class == "Date" | class == "POSIXct") {
      cat(crayon::yellow(paste0("Column '", colnames(y)[j], "' in ", crayon::bold("RHS"), 
                                " has been coerced from ", class, " to character.\n")))
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
  
  if(length(not.in.x) > 0) {
    cat(crayon::yellow(paste0("Column(s) '", not.in.x, "' are not in ", 
                              crayon::bold("LHS"), " so they have been removed from the comparison.\n")))
  }
  if(length(not.in.y) > 0) {
    cat(crayon::yellow(paste0("Column(s) '", not.in.y, "' are not in ", 
                              crayon::bold("RHS"), " so they have been removed from the comparison.\n")))
  }
  
  common.columns <- colnames(x)[colnames(x) %in% colnames(y)]
  
  x <- x[common.columns]
  y <- y[common.columns]
  
  tables <- list(x = x, y = y)
  return(tables)
}

.getCommonNrRows <- function(tables) {
  x <- tables$x
  y <- tables$y

  n1 <- nrow(x)
  n2 <- nrow(y)
  if(n1 > n2) {
    cat(crayon::yellow(paste0(crayon::bold("LHS"), " has more rows than ", 
                              crayon::bold("RHS"), ". The last ", n1-n2, 
                              " row(s) of ", crayon::bold("LHS"), 
                              " have been removed.\n")))
    x <- x[1:n2, ]
  }
  
  if(n2 > n1) {
    cat(crayon::yellow(paste0(crayon::bold("RHS"), " has more rows than ", 
                              crayon::bold("LHS"), ". The last ", n2-n1, 
                              " row(s) of ", crayon::bold("RHS"), 
                              " have been removed.\n")))
    y <- y[1:n1, ]
  }
  
  tables <- list(x = x, y = y)
  return(tables)
}

.getCommonTypes <- function(tables) {
  x <- tables$x
  y <- tables$y
  
  util.types <- data.frame(
    ColType = c("logical", "integer", "double", "character"),
    Power = 1:4,
    stringsAsFactors = FALSE
  )
  
  col.types.x <- c()
  col.types.y <- c()
  for(i in 1:ncol(x)) col.types.x[i] <- typeof(x[, i])
  for(i in 1:ncol(y)) col.types.y[i] <- typeof(y[, i])
  
  col.types <- data.frame(
    Column = colnames(x),
    ColTypeX = col.types.x,
    ColTypeY = col.types.y,
    stringsAsFactors = FALSE
  )
  
  types.tally <- col.types %>% 
    dplyr::left_join(util.types, by = c("ColTypeX" = "ColType")) %>%
    dplyr::rename(PowerX = Power) %>%
    dplyr::left_join(util.types, by = c("ColTypeY" = "ColType")) %>% 
    dplyr::rename(PowerY = Power) %>% 
    dplyr::mutate(Same = (ColTypeX == ColTypeY)) %>% 
    dplyr::mutate(StrongerType = 
                    purrr::pmap_chr(list(PowerX, PowerY, ColTypeX, ColTypeY), 
                                    function(p1, p2, ct1, ct2) if(p1 > p2) ct1 else ct2))
  
  for(r in 1:nrow(types.tally)) {
    if(types.tally[r, "Same"] == FALSE) {
      row <- types.tally[r, ]
      if(row$PowerY > row$PowerX) {
        cat(crayon::yellow(paste0("Column '", row$Column, "' in ", crayon::bold("LHS"), 
                                  " has been coerced from ", row$ColTypeX, 
                                  " to ", row$StrongerType, ".\n")))
        x[, row$Column] <- .as(x[, row$Column], type = row$StrongerType)
      } else {
        cat(crayon::yellow(paste0("Column '", row$Column, "' in ", crayon::bold("RHS"), 
                                  " has been coerced from ", row$ColTypeY, 
                                  " to ", row$StrongerType, ".\n")))
        y[, row$column] <- .as(y[, row$Column], type = row$StrongerType)
      }
    }
  }
  
  tables <- list(x = x, y = y)
  return(tables)
}
