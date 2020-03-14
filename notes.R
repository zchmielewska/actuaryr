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

compare(1:3, 1:2)
