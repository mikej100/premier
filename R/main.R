library(openxlsx)

read_results_and_predictions <- function (filename) {
  wb_path <- file.path("../data", filename)
  sec_base <-  read.xlsx(
    wb_path,
    sheet = "Data",
  )
}

brier_score <- function(fcst, actual) {
  (sum( (fcst - actual)^2) ) /length(fcst)
}

