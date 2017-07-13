context("Internal Presentation CEH")

test_that("Mean annual maximum Duration for Balder", {
  meanMaxDur <- balder %>% find_spells(threshold = 0.001) %>%
    assign_period() %>%
    split_spells(at = "year") %>%
    drop_na_periods(year) %>%
    group_by(year, state) %>% summarise_at(vars(duration), funs(max)) %>%
    group_by(state) %>% summarise_at(vars(duration), funs(mean))

  current <- as.numeric(meanMaxDur$duration[meanMaxDur$state == "no-flow"])
  expected <- 38.6

  expect_equal(current, expected)

  })

