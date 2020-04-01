context("Test compare()")

test_that("compare() returns absolute difference for numericals", {
  x <- data.frame(
    v1 = 1:3,
    v2 = 4:6
  )
  y <- data.frame(
    v1 = 1:3,
    v2 = 7:9
  )
  res <- data.frame(
    v1 = rep(0, 3),
    v2 = rep(-3, 3)
  )
  expect_equal(compare(x, y), res)
})

test_that("compare() returns the_same/different for characters", {
  x <- data.frame(
    v1 = letters[1:3],
    v2 = letters[4:6]
  )
  y <- data.frame(
    v1 = letters[1:3],
    v2 = letters[7:9]
  )
  res <- data.frame(
    v1 = rep("the_same", 3),
    v2 = rep("different", 3),
    stringsAsFactors = FALSE
  )
  expect_equal(compare(x, y), res)
})


test_that("compare() compares only common columns", {
  x <- data.frame(
    v1 = 1:3,
    v2 = 4:6
  )
  y <- data.frame(
    v1 = 1:3,
    v3 = 7:9
  )
  res <- data.frame(v1 = rep(0, 3))
  expect_equal(compare(x, y), res)
})

test_that("compare() slices to the same number of rows", {
  x <- data.frame(v1 = 1:3)
  y <- data.frame(v1 = 1:4)
  res <- data.frame(v1 = rep(0, 3))
  expect_equal(compare(x, y), res)
})

test_that("compare() coerces to stronger type in common columns", {
  x <- data.frame(v1 = c(1, 2, 3))
  y <- data.frame(v1 = c("1", "2", "3"))
  res <- data.frame(
    v1 = rep("the_same", 3),
    stringsAsFactors = FALSE
  )
  expect_equal(compare(x, y), res)
})

test_that("compare() combines all methods", {
  x <- data.frame(
    v1 = c(1, 2, 3),
    v2 = c(1, 2, 3),
    v3 = c(1, 2, 3)
    )
  y <- data.frame(
    v1 = c("1", "2", "3", "4"),
    v3 = rep(4, 4),
    stringsAsFactors = FALSE
    )
  res <- data.frame(
    v1 = rep("the_same", 3),
    v3 = c(-3, -2, -1),
    stringsAsFactors = FALSE
  )
  expect_equal(compare(x, y), res)
})

test_that("compare() works for tibbles", {
  x <- tibble::tibble(v1 = 1:3)
  y <- tibble::tibble(v1 = 1:3)
  res <- data.frame(
    v1 = rep(0, 3),
    stringsAsFactors = FALSE
  )
  expect_equal(compare(x, y), res)
  
})
