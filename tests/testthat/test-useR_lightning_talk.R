context("Results from useR!2017 lightning talk")

test_that("Results are identical to the ones in rnw/pdf", {
  current <- metric(balder, drop_na = "major",
                    fun_major = max, fun_total = mean,
                    simplify = TRUE, varname = "mean.annual.max")

  expected <- c(mean.annual.max = 5.222)
  expect_equal(round(current, 3), expected)


  current <- metric(balder, major = 244, drop_na = "major",
                    fun_major = max, fun_total = mean,
                    simplify = TRUE, varname = "mean.annual.max")

  expected <- c(mean.annual.max = 5.814)
  expect_equal(round(current, 6), expected)


  u <- metric(balder, major = 244,
              fun_major = function(x) max(x, na.rm = TRUE))
  current <- as_tibble(u[,seq_len(ncol(u))])

  expected <- structure(list(
    major = structure(1:7,
                      .Label = c("1974", "1975", "1976", "1977", "1978", "1979", "1980"),
                      class = c("ordered", "factor")),
    variable = c(4.72, 3.955, 5.471, 5.334, 7.757, 6.753, 5.885)),
    .Names = c("major", "variable"),
    row.names = c(NA, -7L),
    class = c("tbl_df", "tbl", "data.frame"))

  expect_equal(current, expected)


  # This was computed for complete = FALSE, which is incorrect.
  u <- smires(balder, fun_major = max,
              fun_total = function(x) round(mean(x, na.rm = TRUE), 4),
              complete = FALSE)
  current <- as_tibble(u[,seq_len(ncol(u))])

  expected <- structure(list(
    state = structure(c(1L, 2L, NA), .Label = c("no-flow", "flow"), class = "factor"),
    variable = structure(c(37, 84.4286, 122.5), units = "days", class = "difftime")),
    .Names = c("state", "variable"),
    row.names = c("1", "2", "3"),
    class = c("tbl_df", "tbl", "data.frame"))

  expect_equal(current, expected)



  seasons <- c(spring = 60, summer = 152,
               autumn = 244, winter = 335)

  u <- smires(balder, minor = seasons, fun_minor = max, drop_na = "group")
  current <- as_tibble(u[,seq_len(ncol(u))])

  expected <- structure(list(
    minor = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
                      .Label = c("spring", "summer", "autumn", "winter"),
                      class = c("ordered", "factor")),
    state = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
                      .Label = c("no-flow", "flow"),
                      class = "factor"),
    variable = structure(c(70, 92, 45, 47, 46, 44, 56, 90),
                         units = "days",
                         class = "difftime")),
    .Names = c("minor",  "state", "variable"),
    row.names = c(NA, -8L),
    class = c("tbl_df", "tbl", "data.frame"))

  expect_equal(current, expected)
})





