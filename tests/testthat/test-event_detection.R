context("Event Detection")

e <- find_events(balder)

test_that("structure of tibble is correct", {

  # colnames
  expect_equal(colnames(e),
               c("event", "state", "start", "end", "duration"))

  # classes
  expect_equal(unname(sapply(as.list(e), class)),
               c("factor", "factor", "Date", "Date", "difftime"))
})


test_that("state is either dry, wet or NA", {

  # order is of importance, colors in ggplot palette
  # dry should be first
  expect_equal(levels(e$state), c("dry", "wet"))

  # NAs in discharge will result in NA state
  expect_true(any(is.na(e$state)))
})


