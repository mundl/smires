context("Default arguments as proposed by workgroup")

# just keep file formats with header
test_that("find_event() and is.inermittent() have identical default args", {
  int <- formals(is.intermittent)
  event <- formals(find_events)

  # period should not be an argument to find_event()
  # todo: remove, once solved
  arg <- setdiff(intersect(names(int), names(event)), c("x", "period"))
  expect_equal(int[arg], event[arg])
  }
)
