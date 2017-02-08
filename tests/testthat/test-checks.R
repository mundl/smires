context("Automated checks before calculating metrics")

# just keep file formats with header
test_that("dt of time series can be guessed", {
  dict <- c(monthly = "months", weekly = "weeks", daily = "days")
  for(i in names(dict)) {
    index <- seq(Sys.Date(), by = dict[i], length.out = 1000)
    expect_equal(object = .guess_deltat(index),
                 expected = i)
  }
})
