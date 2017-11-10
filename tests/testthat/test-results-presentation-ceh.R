context("Internal Presentation CEH")

test_that("Mean annual maximum Duration for Balder", {

  # This was computed for complete = FALSE, which is incorrect.
  current <- char_binary(balder, fun_major = max, fun_total = mean,
                         drop_na = "major", rule = "onset",
                         state = "no-flow", simplify = TRUE,
                         complete = FALSE)

  expected <- 39.8
  expect_equal(current, expected)

  # not yet implemented
  # current <- char_binary(balder, fun_major = max, fun_total = mean,
  #                        drop_na = "major", rule = "cut_major",
  #                        state = "no-flow", simplify = T)
  #
  # expected <- c("variable" = 38.6)
  # expect_equal(current, expected)

})

