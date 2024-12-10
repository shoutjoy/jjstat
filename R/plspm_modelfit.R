#' Calculate Model Fit Indices for PLS Models
#'
#' This function computes various model fit indices for a Partial Least Squares (PLS) model, including SRMR, NFI, d_ULS, and d_G.
#' Bootstrap confidence intervals for d_ULS and d_G are also provided.
#'
#' @param pls_model A PLS model object containing model results.
#' @param blocks A list defining the measurement model blocks. Each block should contain variable names.
#' @param bootstrap The number of bootstrap samples to use. Default is 1000.
#' @param ci The confidence level for confidence intervals. Default is 0.95.
#' @param digits The number of decimal places to round results. Default is 5.
#' @param trans If TRUE, transforms the result into a transposed data frame. Default is TRUE.
#'
#' @return A data frame containing the computed fit indices and their evaluations.
#' @export
plspm_modelfit <- function(pls_model, blocks,
                           bootstrap = 1000, ci = 0.95, digits = 5,
                           trans = TRUE) {
  # Helper function to perform item parceling for PLS blocks
  plspm_ip <- function(data, blocks) {
    # Create an empty data frame to store parcelled data
    parcelled_data <- data.frame(matrix(nrow = nrow(data), ncol = 0))

    # Iterate through each block in the measurement model
    for (block_name in names(blocks)) {
      block_vars <- blocks[[block_name]]

      # Check if all variables in the block exist in the dataset
      if (!all(block_vars %in% colnames(data))) {
        stop(paste("Some variables in block", block_name, "are not in the data"))
      }

      # Compute the mean of the block variables (item parceling)
      parcelled_data[[block_name]] <- rowMeans(data[, block_vars], na.rm = TRUE)
    }
    # Return the parcelled data as a data frame
    return(parcelled_data)
  }

  # Step 1: Generate parcelled data
  parcelled_data <- plspm_ip(pls_model$data, blocks)

  # Step 2: Compute correlation matrices
  # Observed correlation matrix based on parcelled data
  obs_cor <- cor(parcelled_data, use = "pairwise.complete.obs")
  # Implied correlation matrix based on PLS scores
  implied_cor <- cor(pls_model$scores, use = "pairwise.complete.obs")

  # Step 3: Calculate the null model's diagonal correlation matrix
  null_cor <- diag(diag(obs_cor))

  # Step 4: Compute Chi-square values
  # Chi-square for the proposed model
  proposed_chi_square <- sum(diag(solve(implied_cor) %*% obs_cor)) - ncol(obs_cor) +
    log(det(implied_cor)) - log(det(obs_cor))
  # Chi-square for the null model
  null_chi_square <- sum(diag(solve(null_cor) %*% obs_cor)) - ncol(obs_cor) +
    log(det(null_cor)) - log(det(obs_cor))

  # Step 5: Calculate degrees of freedom
  df <- (ncol(obs_cor) * (ncol(obs_cor) - 1)) / 2

  # Step 6: Compute p-value from the Chi-square distribution
  p_value <- 1 - pchisq(proposed_chi_square, df)

  # Step 7: Compute SRMR (Standardized Root Mean Square Residual)
  residuals <- obs_cor - implied_cor
  srmr <- sqrt(mean(residuals[lower.tri(residuals)]^2))

  # Step 8: Compute NFI (Normed Fit Index)
  nfi <- (null_chi_square - proposed_chi_square) / null_chi_square

  # Step 9: Calculate d_ULS and d_G distances
  # d_ULS: Squared Euclidean distance between observed and implied matrices
  d_ULS <- sum(residuals^2)
  # d_G: Geodesic distance considering structural characteristics
  d_G <- sqrt(sum(log(diag(solve(implied_cor) %*% obs_cor))^2))

  # Step 10: Perform bootstrap for confidence intervals
  d_ULS_samples <- numeric(bootstrap)
  d_G_samples <- numeric(bootstrap)

  # Initialize progress bar for bootstrap
  pb <- txtProgressBar(min = 0, max = bootstrap, style = 3)

  for (i in 1:bootstrap) {
    # Resample data with replacement
    resample_idx <- sample(1:nrow(pls_model$data), replace = TRUE)
    resampled_data <- pls_model$data[resample_idx, ]

    # Generate parcelled data for the resampled dataset
    resampled_parcelled_data <- plspm_ip(resampled_data, blocks)

    # Compute correlation matrix for the resampled data
    resampled_obs_cor <- cor(resampled_parcelled_data, use = "pairwise.complete.obs")

    # Compute residuals and distances for the resampled data
    resampled_residuals <- resampled_obs_cor - implied_cor
    d_ULS_samples[i] <- sum(resampled_residuals^2)
    d_G_samples[i] <- sqrt(sum(log(diag(solve(implied_cor) %*% resampled_obs_cor))^2))

    # Update progress bar
    setTxtProgressBar(pb, i)
  }

  # Close progress bar
  close(pb)

  # Step 11: Calculate confidence intervals for d_ULS and d_G
  lower_bound <- (1 - ci) / 2
  upper_bound <- 1 - lower_bound
  d_ULS_ci <- quantile(d_ULS_samples, probs = c(lower_bound, upper_bound), na.rm = TRUE)
  d_G_ci <- quantile(d_G_samples, probs = c(lower_bound, upper_bound), na.rm = TRUE)

  # Format confidence intervals as strings
  d_ULS_ci_str <- paste0("[", round(d_ULS_ci[1], digits), ", ", round(d_ULS_ci[2], digits), "]")
  d_G_ci_str <- paste0("[", round(d_G_ci[1], digits), ", ", round(d_G_ci[2], digits), "]")

  # Step 12: Evaluate SRMR, NFI, and p-value fit
  srmr_eval <- ifelse(srmr <= 0.05, "매우적합(<0.05)",
                      ifelse(srmr <= 0.08, "적합(<.08)", "부적합(>0.1)"))
  nfi_eval <- ifelse(nfi >= 0.90, "적합(>.90)", "부적합")
  p_value_eval <- ifelse(p_value > 0.05, "적합(>0.05)", "부적합(<.05)")

  # Step 13: Organize results into a data frame
  result <- data.frame(
    model = "modelfit_index",
    Chisq = round(proposed_chi_square, digits),
    df = round(df, digits),
    p_value = round(p_value, digits),
    SRMR = round(srmr, digits),
    NFI = round(nfi, digits),
    d_ULS_EuclideanDist = round(d_ULS, digits),
    d_G_GeodesicDist = round(d_G, digits)
  )

  # Add evaluation criteria as a separate row
  evaluation <- data.frame(
    model = "evaluation",
    Chisq = "-",
    df = "-",
    p_value = p_value_eval,
    SRMR = srmr_eval,
    NFI = nfi_eval,
    d_ULS_EuclideanDist = d_ULS_ci_str,
    d_G_GeodesicDist = d_G_ci_str
  )

  # Combine results and evaluations
  final_result <- rbind(result, evaluation)

  # Optionally transpose the result
  if (trans) {
    final_result <- final_result %>% t() %>% rowdata2col(1) %>% data.frame()
  }

  # Return the final result
  return(final_result)
}
