library(testthat)
library(dplyr)

project_root <- ""
source ("./RScripts/main.R")

test_that("Read results and forecast file",{
  data <- read_results_and_predictions("Forecast Weighted 10 Evaluation 2.xlsx")
  expect_gte(length(data$Result),800)
})

test_that("Calculate Brier Score",{
  expect_equal(
    brier_score( c(.5, 1), c(1,1)),
    0.125
  )
    expect_equal(
    brier_score( c(.9, .3), c(1,1)),
    (.01 + 0.49)/2
  )
    expect_equal(
    brier_score( c(.9, .9, .1), c(1,1,0)),
    .01
  )
    expect_equal(
    brier_score( c(.9, .9, .9), c(0,1,0)),
    (.81 + .01 + .81) / 3
  )
})

test_that("Brier scores of odds",{
  bs_h_odds <- brier_score(data$Home.Odds.P, as.numeric(data$Result=="H"))
  bs_d_odds <- brier_score(data$Draw.Odds.P, as.numeric(data$Result=="D"))
  bs_a_odds <- brier_score(data$Away.Odds.P, as.numeric(data$Result=="A"))
  bs_h_fcst <- brier_score(data$Fcast.Home.P, as.numeric(data$Result=="H"))
  bs_d_fcst <- brier_score(data$Fcast.Draw.P, as.numeric(data$Result=="D"))
  bs_a_fcst <- brier_score(data$Fcast.Away.P, as.numeric(data$Result=="A"))
})


test_that("")  
