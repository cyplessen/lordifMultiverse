#' Plot Results of Multiverse Logistic Regression DIF Analysis
#'
#' This function generates visualizations for the results of a multiverse logistic
#' regression differential item functioning (DIF) analysis. It produces a histogram
#' showing the distribution of the number of items flagged for DIF across different
#' specifications and a boxplot summarizing the number of flagged items per
#' specification criterion and threshold.
#'
#' @param specification_results A data frame containing the results of the DIF
#' analysis, including the specifications (criteria and thresholds) and the items
#' identified as exhibiting DIF.
#' @param items A data frame or matrix of item responses used in the DIF analysis.
#' This is used to set scale limits in the plots.
#'
#' @return The function invisibly returns a grid plot (cowplot object) combining a
#' histogram and a boxplot of the DIF analysis results. The plot is displayed but not
#' explicitly returned; use `return()` to capture the plot if needed.
#'
#' @details The function first checks if all identified_items are missing. If so, it
#' assumes no items were flagged for DIF and adjusts the data accordingly. Otherwise,
#' it processes the `specification_results` to calculate the sum of flagged items for
#' each specification. It uses `ggplot2` for plotting and `cowplot` for arranging the
#' plots. Ensure `ggplot2`, `dplyr`, `tidyr`, and `cowplot` are installed and
#' available in your R environment.
#'
#' @examples
#' # Assuming 'specification_results' is the output from `lordif_multiverse` and
#' # 'items' is the original item response data:
#' data(Anxiety, package = "lordif")
#' items_anxiety <- Anxiety[paste("R", 1:29, sep = "")]
#'
#' specification_results <- structure(list(
#' specification = c("Chisqr", "Chisqr", "Chisqr",
#' "Beta", "Beta", "CoxSnell", "CoxSnell", "CoxSnell", "CoxSnell",
#' "CoxSnell", "Nagelkerke", "Nagelkerke", "Nagelkerke", "Nagelkerke",
#' "Nagelkerke", "McFadden", "McFadden", "McFadden", "McFadden",
#' "McFadden"),
#' threshold = c(0.0001, 0.001, 0.01, 0.05, 0.1, 0.02,
#'               0.03, 0.04, 0.05, 0.06, 0.02, 0.03, 0.04, 0.05, 0.06, 0.02, 0.03,
#'               0.04, 0.05, 0.06),
#' identified_items = c(NA, "R9-R11-R24", "R9-R11-R18-R24",
#'               "R9-R11", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'               NA, NA, NA)),
#' row.names = c(NA, -20L),
#' class = "data.frame")
#' \donttest{
#' plot_lordif_multiverse(specification_results, items = items_anxiety)
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_histogram theme_minimal labs geom_boxplot scale_fill_viridis_d scale_x_continuous scale_y_continuous coord_flip theme
#' @importFrom dplyr mutate row_number rename_with all_of c_across everything rowwise ungroup
#' @importFrom tidyr pivot_wider separate_rows
#' @importFrom cowplot plot_grid
#' @export
plot_lordif_multiverse <- function(specification_results, items) {

  # Check if all identified_items are missing
  if(all(is.na(specification_results$identified_items))) {
    data_flagged_items <- specification_results %>%
      dplyr::mutate(sum = 0, criterion = paste(specification, ": ", threshold))
  } else {
    # Proceed with the original data processing if not all identified_items are missing

    data_flagged_items <- specification_results %>%
      mutate(row = row_number()) %>%
      separate_rows(identified_items, sep = "-") %>%
      mutate(value = 1) %>%
      pivot_wider(names_from = identified_items, values_from = value, values_fill = list(value = 0)) %>%
      rename_with(~ifelse(.x == "<NA>", "none", .x), everything()) %>%
      rowwise() %>%
      mutate(sum = sum(c_across(all_of(intersect(colnames(.), colnames(items)))))) %>%
      ungroup() %>%
      mutate(criterion = paste0(specification, ": ", threshold))
  }

  hist_flagged_items <- data_flagged_items %>%
    ggplot(aes(x = sum)) +
    geom_histogram(binwidth = 1, color = "black", fill = "#21918c") +
    theme_minimal() +
    labs(title = "Histogram: Amount of Flagged Items From Each DIF Analysis",
         x = "Number of Flagged Items",
         y = "Frequency") +
    scale_x_continuous(limits = c(min(data_flagged_items$sum, na.rm = TRUE) - 0.5, max(data_flagged_items$sum, na.rm = TRUE) + 0.5))

  box_flagged_items <- data_flagged_items %>%
    ggplot(aes(x = criterion, y = sum, fill = criterion)) +
    geom_boxplot(alpha = 0.5) +
    scale_fill_viridis_d() +
    labs(title = "Specification: DIF Flagging Criterion and Threshold",
         x = "Method",
         y = "Number of Flagged Items") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(min(data_flagged_items$sum, na.rm = TRUE) - 0.5, max(data_flagged_items$sum, na.rm = TRUE) + 0.5)) +
    coord_flip()

  cowplot::plot_grid(hist_flagged_items, box_flagged_items, ncol = 1, align = 'v')
}
