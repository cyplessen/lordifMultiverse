#' Perform Multiverse Logistic Regression DIF Analysis
#'
#' This function conducts a multiverse analysis for differential item functioning (DIF)
#' using logistic regression. It automatically generates and tests multiple
#' specifications based on a range of criteria and thresholds to robustly assess DIF
#' across items.
#'
#' @param items A data frame or matrix of item responses, where rows represent
#' individuals and columns represent items.
#' @param group A vector indicating the group membership for each individual in
#' 'items'. The length of 'group' must match the number of rows in 'items'.
#' @param custom_criteria_thresholds Optional. A list where each element's name is a criterion (e.g., "Chisqr", "Beta") and each element is a vector of thresholds for that criterion. If NULL, default criteria and thresholds are used.
#' @param verbose A logical value indicating whether to print detailed processing
#' messages. Defaults to FALSE.
#'
#' @return A data frame summarizing the DIF analysis results for each specification,
#' including the specification used (criterion and threshold) and the items identified
#' as exhibiting DIF.
#'
#' @details The function tests for DIF using various criteria including Chi-squared,
#' Beta, Cox-Snell, Nagelkerke, and McFadden's R-squared, across a predefined range
#' of thresholds. It leverages the 'lordif' package for the underlying DIF analysis.
#' The 'dplyr', 'tidyr', and 'lapply' functions are used for data manipulation, and
#' 'do.call' is used for dynamic function calls. Ensure that these packages are
#' installed and available in your R environment: 'lordif' for logistic regression DIF
#' analysis, 'dplyr' and 'tidyr' for data manipulation.
#'
#' @examples
#' # Use data from the lordif package
#' data(Anxiety, package = "lordif")
#' items_anxiety <- Anxiety[paste("R", 1:29, sep = "")]
#' group_age <- Anxiety$age
#'
#' # Run multiverse analysis for different grouping variables
#' # The examples below are wrapped in \donttest{} to avoid long execution times
#' # during package checking. Remove \donttest{} if you want them to be executed
#' # during R CMD check.
#' \donttest{
#' multiverse_age <- lordif_multiverse(items_anxiety, group_age)
#' }
#'
#' @export
#' @importFrom lordif lordif
#' @importFrom dplyr %>% mutate filter
#' @importFrom tidyr separate_rows pivot_wider

lordif_multiverse <- function(items, group, custom_criteria_thresholds = NULL, verbose = FALSE) {
  
  # Use default criteria and thresholds if custom ones are not provided
  if (is.null(custom_criteria_thresholds)) {
    criteria_thresholds <- list(
      Chisqr = c(0.0001, 0.001, 0.01),
      Beta = c(0.05, 0.1),
      CoxSnell = c(0.02, 0.03, 0.04, 0.05, 0.06),
      Nagelkerke = c(0.02, 0.03, 0.04, 0.05, 0.06),
      McFadden = c(0.02, 0.03, 0.04, 0.05, 0.06)
    )
  } else {
    criteria_thresholds <- custom_criteria_thresholds
  }

  # Function to generate specifications based on criteria and thresholds
  generate_specifications <- function(criteria_thresholds) {
    specs <- do.call(rbind, lapply(names(criteria_thresholds), function(criterion) {
      data.frame(criterion = criterion,
                 threshold = criteria_thresholds[[criterion]])
    }))
    return(specs)
  }

  # Generate specifications
  specifications <- generate_specifications(criteria_thresholds)

  prepare_lordif_call <- function(spec, items, group) {
    args <- list(resp.data = items, group = group, minCell = 5)

    if (spec$criterion == "Chisqr" || spec$criterion == "Beta") {
      args$criterion <- spec$criterion
      specific_arg <- ifelse(spec$criterion == "Chisqr", "alpha", "beta.change")
      args[[specific_arg]] <- spec$threshold
    } else if (spec$criterion %in% c("CoxSnell", "Nagelkerke", "McFadden")) {
      args$criterion <- "R2"
      args$pseudo.R2 <- spec$criterion
      args$R2.change <- spec$threshold
    }

    return(do.call(lordif, args))
  }

  # Initialize the results list
  results <- list()

  # Loop through the specifications and process each one
  for (i in 1:nrow(specifications)) {
    spec <- specifications[i, ]
    if (verbose) {
      message("Processing specification: ", i, "/", nrow(specifications),
              " Criterion: ", spec$criterion, " Threshold: ", spec$threshold)
    }

    # Call prepare_lordif_call within a warning handler
    dif_result <- withCallingHandlers({
      prepare_lordif_call(spec, items, group)
    }, warning = function(w) {
      # Check if the warning is one of the specific warnings we want to ignore
      if (grepl("all items got flagged for DIF - stopping", w$message) ||
          grepl("no items got flagged for DIF - stopping", w$message)) {
        # Ignore the specific warning by invoking a restart without signaling
        invokeRestart("muffleWarning")
      } else {
        # Otherwise, allow the default warning handler to signal the warning
        invokeRestart("muffleMessage")
      }
    })

    identified_items <- if (!is.null(dif_result)) {
      items_flagged <- colnames(items)[dif_result$flag]
      if (length(items_flagged) == 0) NA else paste(items_flagged, collapse = "-")
    } else {
      NA
    }

    # Append the results for this specification to the results list
    results[[i]] <- data.frame(
      specification = spec$criterion,
      threshold = spec$threshold,
      identified_items = identified_items,
      stringsAsFactors = FALSE
    )
  }

  # Combine all results into a single data frame
  results_df <- do.call(rbind, results)

  return(results_df)
}