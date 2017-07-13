context("Test a time series for intermittency")

test_that("is.intermittent() works on Ampney Brook", {
  x <- ampneyBrook
  expect_silent(is.intermittent(x))
  expect_equal(is.intermittent(x, threshold = 0.001), T)

  # there are 386 no-flow days in total
  expect_equal(is.intermittent(x, ndays = 386, consecutive = F), T)
  expect_equal(is.intermittent(x, ndays = 387, consecutive = F), F)

  # longest no-flow spell lasts 143 daxs
  expect_equal(is.intermittent(x, ndays = 386, consecutive = T), F)
  expect_equal(is.intermittent(x, ndays = 144, consecutive = T), F)
  expect_equal(is.intermittent(x, ndays = 143, consecutive = T), T)
})
