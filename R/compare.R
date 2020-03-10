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

  # find common and uncommon columns
  # change type of different columns
  # cut uneven number of rows
  # produce comparison result
 
  x <- dplyr::as_tibble(x)
  y <- dplyr::as_tibble(y)
  
  not.x <- colnames(y)[!c(colnames(y) %in% colnames(x))]
  not.y <- colnames(x)[!c(colnames(x) %in% colnames(y))]
  
  if(length(not.x) > 0) warning(paste("Columns in y but not in x:", not.x))
  if(length(not.y) > 0) warning(paste("Columns in x but not in y:", not.y))

  common.cols <- colnames(x)[colnames(x) %in% colnames(y)]
  
  # a = common columns
  x.a <- x[, common.cols]
  y.a <- y[, common.cols]
  
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
  
  util.types <- data.frame(
    col.type = c("logical", "integer", "double", "character"),
    power = 1:4,
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
      column <- types.tally[r, "column"]
      col.type <- types.tally[r, "stronger.type"]
      if(col.type == "integer")   x.b[, column] <- as.integer(x.b[, column])
      if(col.type == "double")    x.b[, column] <- as.double(x.b[, column])
      if(col.type == "character") x.b[, column] <- as.character(x.b[, column])
      if(col.type == "integer")   y.b[, column] <- as.integer(y.b[, column])
      if(col.type == "double")    y.b[, column] <- as.double(y.b[, column])
      if(col.type == "character") y.b[, column] <- as.character(y.b[, column])
    }
  }
  
  # c = common columns, types and number of rows
  n1 <- nrow(x)
  n2 <- nrow(y)
  x.c <- x.b
  y.c <- y.b
  if(n1 > n2) x.c <- x.b[1:n2, ]
  if(n2 > n1) y.c <- y.b[1:n1, ]
  
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



# same - numericals

x <- data.frame(
  x = rep(1, 3),
  y = rep(2, 3),
  z = rep(3, 3)
)

y <- data.frame(
  x = rep(1, 3),
  y = rep(2, 3),
  z = rep(3, 3)
)

compare(x, y)


# co zrobić z factorami?

# same - characters

x2 <- data.frame(v = letters[1:3])
y2 <- data.frame(v = letters[1:3])
compare(x2, y2)




tib <- dplyr::tibble(
  x = 1:3,
  y = rep("a", 3)
)

