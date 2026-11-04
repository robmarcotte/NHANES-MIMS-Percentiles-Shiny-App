library(testthat)
library(dplyr)
library(wwadapercentiles)

test_that("Batch CSV processing handles metrics and outputs correctly", {
  # 1. Create a dummy test dataset matching your UI specifications
  test_df <- data.frame(
    id = c(1, 2, 3),
    age = c(12, 45, 60),
    sex = c("M", "F", "M"),
    metric = c("Counts", "MIMS", "invalid_metric"),
    value = c(500000, 15000, 100)
  )
  
  # 2. Replicate the processing logic
  percentiles <- numeric(nrow(test_df))
  for(i in 1:nrow(test_df)) {
    row_sex <- toupper(trimws(test_df$sex[i]))
    row_metric <- tolower(trimws(test_df$metric[i]))
    
    if (row_metric %in% c("counts", "tac", "activity counts")) {
      percentiles[i] <- wwadapercentiles::tac(y = test_df$value[i], age = test_df$age[i], sex = row_sex)
    } else if (row_metric %in% c("mims", "mims units")) {
      percentiles[i] <- wwadapercentiles::mims(y = test_df$value[i], age = test_df$age[i], sex = row_sex)
    } else {
      percentiles[i] <- NA
    }
  }
  test_df$calculated_percentile <- round(percentiles * 100, 0)
  
  # 3. Assertions
  expect_equal(nrow(test_df), 3)
  expect_true(is.na(test_df$calculated_percentile[3])) # Invalid metric handles gracefully
  expect_true(test_df$calculated_percentile[1] >= 0 && test_df$calculated_percentile[1] <= 100)
})
