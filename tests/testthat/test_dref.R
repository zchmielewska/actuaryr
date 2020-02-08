context("Test date reference functions")

test_that("dref___m() returns date reference within a month", {
  expect_equal(dref_fdom("2020-02-14"),  as.Date("2020-02-01"))
  expect_equal(dref_fwdom("2020-02-14"), as.Date("2020-02-03"))
  expect_equal(dref_ldom("2020-02-14"),  as.Date("2020-02-29"))
  expect_equal(dref_lwdom("2020-02-14"), as.Date("2020-02-28"))
})

test_that("dref___q() returns date reference within a quarter", {
  expect_equal(dref_fdoq("2022-10-14"),  as.Date("2022-10-01"))
  expect_equal(dref_fwdoq("2022-10-14"), as.Date("2022-10-03"))
  expect_equal(dref_ldoq("2022-10-14"),  as.Date("2022-12-31"))
  expect_equal(dref_lwdoq("2022-10-14"), as.Date("2022-12-30"))
})


test_that("dref___y() returns date reference within a year", {
  expect_equal(dref_fdoy("2022-02-14"),  as.Date("2022-01-01"))
  expect_equal(dref_fwdoy("2022-02-14"), as.Date("2022-01-03"))
  expect_equal(dref_ldoy("2022-02-14"),  as.Date("2022-12-31"))
  expect_equal(dref_lwdoy("2022-02-14"), as.Date("2022-12-30"))
})
