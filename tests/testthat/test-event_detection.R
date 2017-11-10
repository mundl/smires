context("Spell Detection")

e <- find_spells(balder)

test_that("structure of tibble is correct", {

  # colnames
  expect_equal(colnames(e),
               c("spell", "state", "onset", "termination", "duration"))

  # classes
  expect_equal(object = unname(sapply(as.list(e), class)),
               expected = list(c("ordered", "factor"),
                               "factor",
                               "Date",
                               "Date",
                               "difftime"))
})


test_that("state is either 'no-flow', 'flow' or NA", {

  # order is of importance, colors in ggplot palette
  # 'no-flow' should be first
  expect_equal(levels(e$state), c("no-flow", "flow"))

  # NAs in discharge will result in NA state
  expect_true(any(is.na(e$state)))
})

test_that("levels of factor state are in correct order", {
  x <- data_frame(time = Sys.Date() + 0:10,
                  discharge = c(NA, rep(0.23, 5), NA, rep(0.8, 4))) %>%
    validate(approx.missing = 0)

  flow <- char_binary(x, threshold = 0.001, drop_na = "none", complete = FALSE)
  expect_equal(object = levels(flow$state), expected = c("no-flow", "flow"))
  expect_equal(object = as.character(flow$state),
               expected = c(NA, "flow", NA, "flow"))

  noflow <- char_binary(x, threshold = 1, complete = FALSE)
  expect_equal(object = levels(noflow$state), expected = c("no-flow", "flow"))
  expect_equal(object = as.character(noflow$state),
               expected = c(NA, "no-flow", NA, "no-flow"))
})




