context("All exported functions run without an error")

test_that("quick & dirty check if functions run", {
  expect_silent(is.intermittent(balder))
  expect_silent(suppressMessages(check_ts(balder)))
  expect_silent(e <- find_events(balder))

  expect_silent(p <- assign_period(e))
  expect_silent(s <- split_events(p))
  expect_silent(s <- drop_na_periods(s, year))

  season <- c("summer" = as.Date("2015-03-01"), "winter" =  as.Date("2015-09-01"))
  expect_silent(start_season(x = season))

  expect_silent(plot_events(e))

  a <- s %>% group_by(year, state) %>% summarise_at(vars(duration), funs(max))
  expect_silent(plot_period(a, type = "ts"))
  expect_silent(plot_period(a, type = "dist"))
})



#multiperiod
#print.multiperiod
