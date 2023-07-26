library(testthat)

test_that("Win probability modified by recent wins", {
  base <- c(0.8, 0.1)
  delta <- c(0.1, 0.1)
  expect_gte(mod_recent_1(base, delta), base) 
  expect_lte(mod_recent_1(base, delta), 1) 
})
mod_recent_1(base, delta )
