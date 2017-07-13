context("All exported functions run without an error")
require(bindrcpp)


test_that("quick & dirty check if functions run", {
  expect_silent(null <- is.intermittent(balder))
  expect_silent(suppressMessages(validate(balder)))


  expect_equal(object = names(intervals),
               expected = c("week", "month", "twoSeasons", "fourSeasons"))

  expect_silent(start_season(x = intervals$twoSeasons))
  expect_silent(start_season(x = c("summer" = 60, "winter" = 244)))

  expect_silent(p <- group_by_interval(balder, minor = intervals$fourSeasons))
  expect_silent(p <- group_by_interval(balder, minor = "month"))
  expect_silent(p <- group_by_interval(balder, minor = "week"))


  expect_silent(e <- find_spells(balder))
  expect_silent(e <- hydrological_year(balder$time))


  expect_silent(suppressMessages(s <- drop_na_periods(x = p, period = "major")))


  expect_silent(binary <- smires(balder))
  expect_silent(metric(balder))

  expect_silent(plot_groups(binary))
  expect_silent(plot_intermittency(balder))
})



#multiperiod
#print.multiperiod
