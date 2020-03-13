#' Title
#'
#' @param x 
#' @param y 
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
compare <- function(x, y) { 

  util.types <- data.frame(
    col.type = c("logical", "integer", "double", "character"),
    power = 1:4,
    stringsAsFactors = FALSE
  )
  
  tables <- list(x = x, y = y)
  tables <- .coerceToCharacter(tables) # factors, POSIXct and Dates are not handled
  tables <- .getCommonColumns(tables)
  tables <- .getCommonNrRows(tables)
  
  # types
  
  col.types1 <- c()
  for(i in 1:ncol(x.a)) {
    col.types1[i] <- typeof(x.a[, i])
  }  
  
  col.types2 <- c()
  for(i in 1:ncol(y.a)) {
    col.types2[i] <- typeof(y.a[, i])
  }  
  
  col.types <- data.frame(
    column = common.cols,
    col.type1 = col.types1,
    col.type2 = col.types2,
    stringsAsFactors = FALSE
  )
  
  types.tally <- col.types %>% 
    dplyr::left_join(util.types, by = c("col.type1" = "col.type")) %>%
    dplyr::rename(power1 = power) %>%
    dplyr::left_join(util.types, by = c("col.type2" = "col.type")) %>% 
    dplyr::rename(power2 = power) %>% 
    dplyr::mutate(same = (col.type1 == col.type2)) %>% 
    dplyr::mutate(stronger.type = purrr::pmap_chr(list(power1, power2, col.type1, col.type2), 
                                             function(p1, p2, ct1, ct2) if(p1 > p2) ct1 else ct2))
  
  # b = common columns and types
  x.b <- x.a
  y.b <- y.a
  for(r in 1:nrow(types.tally)) {
    if(types.tally[r, "same"] == FALSE) {
      row <- types.tally[r, ]
      
      if(row$power2 > row$power1) {
        warning(paste0(row$column," from x has been coerced from ", row$col.type1, " to ", row$stronger.type,"."))
        x.b[, row$column] <- .as(x.b[, row$column], type = row$stronger.type)
      } else {
        warning(paste0(row$column," from y has been coerced from ", row$col.type2, " to ", row$stronger.type,"."))
        y.b[, row$column] <- .as(y.b[, row$column], type = row$stronger.type)
      }
    }
  }
  
  
  
  # compare tables
  result <- x.c
  for(j in 1:ncol(x.c)) {
    if(typeof(x.c[, j]) == "character") {
      result[, j] <- purrr::map2_chr(x.c[, j], y.c[, j], function(x, y) if(x == y) "the_same" else "different")  
    } else {
      result[, j] <- x.c[, j] - y.c[, j]
    }
  }
  
  return(result)
}

# logicals?
# one column?
# not data frames

# ex. the same - numericals
x <- data.frame(
  a = rep(1, 3),
  b = rep(2, 3),
  c = rep(3, 3)
)
y <- data.frame(
  a = rep(1, 3),
  b = rep(2, 3),
  c = rep(3, 3)
)
compare(x, y)

# ex. the same - strings
x <- data.frame(a = letters[1:3],
                b = letters[4:6],
                stringsAsFactors = FALSE)
y <- data.frame(a = letters[1:3],
                b = letters[4:6],
                stringsAsFactors = FALSE)
compare(x, y)

# ex. the same - factors
x <- data.frame(a = letters[1:3],
                b = letters[4:6])
y <- data.frame(a = letters[1:3],
                b = letters[4:6])
compare(x, y)

# ex. different amount of columns
x <- data.frame(
  a = rep(1, 3),
  b = rep(2, 3),
  c = rep(3, 3)
)
y <- data.frame(
  a = rep(1, 3),
  b = rep(2, 3)
)
compare(x, y)

# ex. factors / Dates / POSIXct
x <- data.frame(a = 1:3,
                b = 4:6)
y <- data.frame(a = letters[1:3],
                b = letters[4:6])
compare(x, y)

# ex. different number of rows
x <- data.frame(
  a = rep(1, 2),
  b = rep(2, 2),
  c = rep(3, 2)
)
y <- data.frame(
  a = rep(1, 3),
  b = rep(2, 3),
  c = rep(3, 3)
)
compare(x, y)

# ex. logicals
x <- data.frame(
  a = c(TRUE, FALSE)
)

y <- data.frame(
  a = c(FALSE, FALSE)
)
compare(x, y)
