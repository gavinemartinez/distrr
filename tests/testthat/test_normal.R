# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(distrr)

test_check("distrr")

source("R/plot_normal.R")

test_that("distrr returns expected output for normal inputs", {
  mu <- 0
  sigma <- 1
  level <- 0.95
  crit <- FALSE

  result <- plot_binom(mu, sigma, level, crit)

  expect_is(result, "numeric")

  expect_equal(length(result), 100)

  expect_equal(mean(result), mu, tolerance = 0.01)

  expect_equal(sd(result), sigma, tolerance = 0.01)
})
