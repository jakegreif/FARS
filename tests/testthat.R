library(testthat)
library(mapdata)
library(FARS)
test_that("Make file name", {
  library(mapdata)
  expect_match(make_filename(2019), "accident_2019.csv.bz2")
})
