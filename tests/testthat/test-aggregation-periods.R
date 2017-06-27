context("Aggregation of periods")


test_that("agg_period() returns NULL for period == 'none'", {
  expect_null(agg_period())

  # period == "none" is default
  expect_null(agg_period(period = "none"))
})


test_that("weekly and monthly aggregates return same vector as in list 'interval'", {
  choices <- c("week", "month")
  for(i in choices) expect_equal(agg_period(period = i), interval[[i]])
})



test_that("default seasons are correct", {
  expected <- structure(c(16495, 16679), .Names = c("summer", "winter"),
                        class = "Date")

  expect_equal(twoSeasons, expected = expected)
})


test_that("seasonal aggregates", {
  expected <- c(summer = 60, winter = 244)

  # start is of class Date
  expect_equal(agg_period(period = "season", start = twoSeasons),
               expected = expected)

  # start is of class numeric
  expect_equal(agg_period(period = "season", start = expected),
               expected = expected)
})


test_that("yearly aggregates", {
  # default is calendar year
  expect_equal(agg_period(period = "year"), expected = 1)

  # hydrological year
  starts <- c(1, sample(365, size = 10), 365)
  for(i in starts) expect_equal(agg_period(period = "year", start = i),
                                expected = i)
})



