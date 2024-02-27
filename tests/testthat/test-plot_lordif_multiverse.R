library(testthat)
library(lordifmultiverse)  # Your package name here

test_check("lordifMultiverse")  # Your package name here

# Test that plot_lordif_multiverse runs without error
test_that("plot_lordif_multiverse runs without error", {
  
  data(Anxiety, package = "lordif")
  
  items_anxiety <- Anxiety[paste("R", 1:29, sep = "")]
  
  specification_results <- structure(
    list(
      specification = c("Chisqr", "Chisqr", "Chisqr",
                        "Beta", "Beta", "CoxSnell", "CoxSnell", "CoxSnell", "CoxSnell",
                        "CoxSnell", "Nagelkerke", "Nagelkerke", "Nagelkerke", "Nagelkerke",
                        "Nagelkerke", "McFadden", "McFadden", "McFadden", "McFadden",
                        "McFadden"), 
      threshold = c(0.0001, 0.001, 0.01, 0.05, 0.1, 0.02,
                    0.03, 0.04, 0.05, 0.06, 0.02, 0.03, 0.04, 0.05, 0.06, 0.02, 0.03,
                    0.04, 0.05, 0.06), 
      identified_items = c(NA, "R9-R11-R24", "R9-R11-R18-R24",
                           "R9-R11", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA, NA, NA)), 
    row.names = c(NA, -20L), class = "data.frame")
  
  expect_silent(plot_lordif_multiverse(specification_results, items_anxiety))
})

# Additional tests could include checking for specific attributes of the plot,
# but since plotting functions primarily generate output for visual inspection,
# these tests might be limited in scope.
