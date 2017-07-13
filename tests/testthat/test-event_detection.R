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


