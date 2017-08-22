context("SMIRES metrics")
require(bindrcpp)


test_that("quick & dirty check if metrics run", {
  expect_equal(no_flow_years(balder), c(f0 = 1))
  expect_equal(round(no_flow_years(ampneyBrook), 4), c(f0 = 0.1818))

  expect_equal(round(MAN(balder), 4), c(MAN = 80.8571))
  expect_equal(round(CVAN(balder), 4), c(CVAN = 1.018))
  expect_equal(FAN(balder), c(54, 181, 207, 0, 0, 72, 52))

  expect_equal(round(MAMD(balder), 4), c(MAMD = 27.5714))
  expect_equal(round(CVAMD(balder), 4), c(CVAMD = 0.9669))
  expect_equal(FAMD(balder), c(46, 45, 70, 0, 0, 20, 12))
})


test_that("all registered metrics exist", {
  for(i in metrics(markup = FALSE)$Function) {
    expect_true(exists(x = i, mode = "function", where = "package:smires"))
  }
})
