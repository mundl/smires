context("Default arguments as proposed by workgroup")

# just keep file formats with header
test_that("find_spell() and is.inermittent() have identical default args", {
  int <- formals(is.intermittent)
  spell <- formals(find_spells)

  arg <- setdiff(intersect(names(int), names(spell)), c("x"))
  expect_equal(int[arg], spell[arg])
  }
)
