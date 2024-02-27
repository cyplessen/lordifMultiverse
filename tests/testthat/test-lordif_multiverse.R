library(testthat)
library(lordifmultiverse)  # Your package name here

test_check("lordifMultiverse")  # Your package name here

test_that("lordif_multiverse runs and outputs expected structure", {
  
  # Load or define your Anxiety dataset
  data(Anxiety, package = "lordif")
  
  # Define the items and grouping variable
  items_anxiety <- Anxiety[paste("R", 1:29, sep = "")]
  group_age <- Anxiety$age
  
  # Run lordif_multiverse and capture the output
  output <- lordif_multiverse(items_anxiety, group_age)
  
  # Check that output is not NULL
  expect_false(is.null(output))
  
  # Check that output is a data frame
  expect_true(is.data.frame(output))
  
})
