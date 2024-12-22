
#' plspm_permutation_test_multi: Perform pairwise permutation tests for multiple groups
#'
#' @param Data Data frame containing the data.
#' @param path_matrix Path matrix for the PLS-PM model.
#' @param blocks List of blocks for the PLS-PM model.
#' @param grp Grouping variable name (as a string).
#' @param n_perm Number of permutations to perform (default: 100).
#'
#' @return A list of data frames, where each data frame contains results for a pairwise group comparison.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' data <- iris  # Example dataset
#' path_matrix <- matrix(c(0, 1, 1, 0), 2, 2)  # Example path matrix
#' blocks <- list(1:2, 3:4)  # Example blocks
#' result <- plspm_permutation_test_multi(data, path_matrix, blocks, "Species", n_perm = 100)
#' print(result)
#' }
plspm_permutation_test_multi <- function(Data, path_matrix,
                                         blocks, grp, n_perm = 100) {
  library(progress)

  # Extract unique group levels
  unique_groups <- unique(Data[[grp]])

  if (length(unique_groups) < 2) {
    stop("Group variable must have at least two unique levels.")
  }

  # Prepare pairwise group comparisons
  group_pairs <- combn(unique_groups, 2, simplify = FALSE)

  # Initialize results list
  results_list <- list()

  # Iterate over each pair of groups
  for (pair in group_pairs) {
    # Extract data for the current pair of groups
    grp1 <- Data[Data[[grp]] == pair[1], ]
    grp2 <- Data[Data[[grp]] == pair[2], ]

    # Combine data for the two groups
    pair_data <- rbind(grp1, grp2)

    # Perform permutation test using the existing function
    test_result <- plspm_permutation_test(pair_data, path_matrix, blocks, grp, n_perm)

    # Add comparison information
    test_result$group1 <- pair[1]
    test_result$group2 <- pair[2]

    # Store result in the list
    results_list[[paste(pair[1], pair[2], sep = "_vs_")]] <- test_result
  }

  return(results_list)
}
