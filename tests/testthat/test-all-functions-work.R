context("All exported functions run without an error")
require(bindrcpp)

test_that("quick & dirty check if functions run", {
  expect_silent(null <- is.intermittent(balder))
  expect_silent(suppressMessages(check_ts(balder)))
  expect_silent(e <- find_spells(balder))

  expect_silent(p <- assign_period(e))
  expect_silent(p <- assign_period(e, interval = "month"))
  expect_silent(p <- assign_period(e, interval = "week"))
  #expect_silent(p <- assign_period(e, interval = "quarter"))
  expect_silent(p <- assign_period(e))
  expect_silent(s <- split_spells(p))
  expect_silent(suppressMessages(s <- drop_na_periods(s, year)))

  expect_silent(start_season(x = twoSeasons))
  expect_silent(start_season(x = c("summer" = 60, "winter" = 244)))

  expect_silent(plot_spells(e))

  a <- s %>% group_by(year, state) %>% summarise_at(vars(duration), funs(max))
  expect_silent(plot_period(a, type = "ts"))
  expect_silent(plot_period(a, type = "dist"))
})



#multiperiod
#print.multiperiod
