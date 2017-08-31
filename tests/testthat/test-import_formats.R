context("read.smires imports various formats")

fileYmd <- tempfile()

test_that("pasting time columns work", {
  writeLines(c("year,month,day,discharge",
               "2001,12,20,3.45",
               "2001,12,21,1.2"),
             con = fileYmd)

  current <- read.smires(file = fileYmd, timecols = c("year", "month", "day"))

  expect_equal(colnames(current), c("time", "discharge"))
  expect_equal(current$time, structure(c(11676, 11677), class = "Date"))
  expect_equal(current$discharge, c(3.45, 1.2))
})

test_that("unnamed 2 column file", {
  writeLines(c("2001-12-20,3.45",
               "2001-12-21,1.2"),
             con = fileYmd)

  current <- read.smires(file = fileYmd, header = FALSE)

  expect_equal(colnames(current), c("time", "discharge"))
  expect_equal(current$time, structure(c(11676, 11677), class = "Date"))
  expect_equal(current$discharge, c(3.45, 1.2))
})

test_that("named 2 column file", {
  writeLines(c("time-date,discharge",
               "2001-12-20,3.45",
               "2001-12-21,1.2"),
             con = fileYmd)

  current <- read.smires(file = fileYmd, timecol = "time-date")

  expect_equal(colnames(current), c("time", "discharge"))
  expect_equal(current$time, structure(c(11676, 11677), class = "Date"))
  expect_equal(current$discharge, c(3.45, 1.2))
})

test_that("non-standard format specifier", {
  writeLines(c("time,discharge",
               "2001/12/20,3.45",
               "2001/12/21,1.2"),
             con = fileYmd)

  current <- read.smires(file = fileYmd, format = "%Y/%m/%d")

  expect_equal(colnames(current), c("time", "discharge"))
  expect_equal(current$time, structure(c(11676, 11677), class = "Date"))
  expect_equal(current$discharge, c(3.45, 1.2))
})
