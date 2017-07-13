context("Internal Presentation CEH")

test_that("Mean annual maximum Duration for Balder", {
  current <- smires(balder, fun_major = max, fun_total = mean,
                    drop_na = "major", rule = "start",
                    state = "no-flow", drop = T)

  expected <- c("variable" = 39.8)
  expect_equal(current, expected)

  # not yet implemented
  # current <- smires(balder, fun_major = max, fun_total = mean,
  #                   drop_na = "major", rule = "cut_major",
  #                   state = "no-flow", drop = T)
  #
  # expected <- c("variable" = 38.6)
  # expect_equal(current, expected)

})

