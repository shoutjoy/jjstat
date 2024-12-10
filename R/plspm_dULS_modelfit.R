#' Bollen-Stine Bootstrap Based d_ULS and d_G Calculation
#'
#' This function calculates the d_ULS and d_G metrics for a PLS model using the Bollen-Stine bootstrap method.
#' Confidence intervals are estimated for these indices.
#'
#' @param pls_model A PLS model object containing model results.
#' @param blocks A list defining the measurement model blocks. Each block should contain variable names.
#' @param bootstrap The number of bootstrap samples to use. Default is 1000.
#' @param ci The confidence level for the intervals. Default is 0.95.
#'
#' @return A data frame with the calculated d_ULS and d_G metrics, along with their confidence intervals and significance indicators.
#' The columns include:
#' \itemize{
#'   \item{\code{Method}: Bootstrap method used (e.g., "Bollen-Stine").}
#'   \item{\code{d_ULS}: Computed d_ULS value.}
#'   \item{\code{d_ULS_95CI}: Confidence interval for d_ULS.}
#'   \item{\code{dUsig}: Significance of d_ULS (whether it falls within the confidence interval).}
#'   \item{\code{d_G}: Computed d_G value.}
#'   \item{\code{d_G_95CI}: Confidence interval for d_G.}
#'   \item{\code{dGsig}: Significance of d_G (whether it falls within the confidence interval).}
#' }
#'
#' @examples
#' \dontrun{
#' # Example call with 1000 bootstrap samples
#' result <- plspm_dULS_modelfit(pls_model = edt_pls, blocks = edblock, bootstrap = 1000, ci = 0.95)
#' print(result)
#' }
#'
#' @export
plspm_dULS_modelfit <- function(pls_model, blocks, bootstrap = 1000, ci = 0.95) {
  # Item Parceling function for plspm
  plspm_ip <- function(data, blocks) {
    parcelled_data <- data.frame(matrix(nrow = nrow(data), ncol = 0))
    for (block_name in names(blocks)) {
      block_vars <- blocks[[block_name]]
      if (!all(block_vars %in% colnames(data))) {
        stop(paste("Some variables in block", block_name, "are not in the data"))
      }
      parcelled_data[[block_name]] <- rowMeans(data[, block_vars], na.rm = TRUE)
    }
    return(parcelled_data)
  }

  # Parcel the data
  parcelled_data <- plspm_ip(pls_model$data, blocks)

  # Observed correlation matrix
  obs_cor <- cor(parcelled_data, use = "pairwise.complete.obs")

  # Implied correlation matrix
  implied_cor <- cor(pls_model$scores, use = "pairwise.complete.obs")

  # d_ULS calculation
  residuals <- obs_cor - implied_cor
  d_ULS <- sum(residuals^2)

  # d_G calculation
  d_G <- sqrt(sum(log(diag(solve(implied_cor) %*% obs_cor))^2))

  # Bootstrap confidence intervals
  d_ULS_samples <- numeric(bootstrap)
  d_G_samples <- numeric(bootstrap)
  pb <- txtProgressBar(min = 0, max = bootstrap, style = 3)

  for (i in 1:bootstrap) {
    resample_idx <- sample(1:nrow(pls_model$data), replace = TRUE)
    resampled_data <- pls_model$data[resample_idx, ]
    resampled_parcelled_data <- plspm_ip(resampled_data, blocks)

    resampled_obs_cor <- cor(resampled_parcelled_data, use = "pairwise.complete.obs")
    resampled_residuals <- resampled_obs_cor - implied_cor
    d_ULS_samples[i] <- sum(resampled_residuals^2)
    d_G_samples[i] <- sqrt(sum(log(diag(solve(implied_cor) %*% resampled_obs_cor))^2))
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Confidence interval calculation
  lower_bound <- (1 - ci) / 2
  upper_bound <- 1 - lower_bound
  d_ULS_ci <- quantile(d_ULS_samples, probs = c(lower_bound, upper_bound), na.rm = TRUE)
  d_G_ci <- quantile(d_G_samples, probs = c(lower_bound, upper_bound), na.rm = TRUE)

  # Significance checks
  dULS_sig <- ifelse(d_ULS >= d_ULS_ci[1] & d_ULS <= d_ULS_ci[2], "*", "")
  d_G_sig <- ifelse(d_G >= d_G_ci[1] & d_G <= d_G_ci[2], "*", "")

  # Result formatting
  d_ULS_ci_str <- paste0("[", round(d_ULS_ci[1], 3), ", ", round(d_ULS_ci[2], 3), "]")
  d_G_ci_str <- paste0("[", round(d_G_ci[1], 3), ", ", round(d_G_ci[2], 3), "]")

  result <- data.frame(
    Method = "Bollen-Stine",
    d_ULS = round(d_ULS, 3),
    d_ULS_95CI = d_ULS_ci_str,
    dUsig = dULS_sig,
    d_G = round(d_G, 3),
    d_G_95CI = d_G_ci_str,
    dGsig = d_G_sig
  )
  rownames(result) <- "index"
  return(result)
}



# plspm_dULS_modelfit <- function(pls_model, blocks, bootstrap = 1000, ci = 0.95) {
#   # Item Parceling function for plspm
#   plspm_ip <- function(data, blocks) {
#     # Create an empty data frame with the same number of rows as the original data
#     parcelled_data <- data.frame(matrix(nrow = nrow(data), ncol = 0))
#
#     # Iterate over each block in the blocks list
#     for (block_name in names(blocks)) {
#       block_vars <- blocks[[block_name]]
#
#       # Check if all block variables exist in the data
#       if (!all(block_vars %in% colnames(data))) {
#         stop(paste("Some variables in block", block_name, "are not in the data"))
#       }
#
#       # Calculate the mean of the block variables (Item Parceling)
#       parcelled_data[[block_name]] <- rowMeans(data[, block_vars], na.rm = TRUE)
#     }
#
#     # Return the parcelled data as a data frame
#     return(parcelled_data)
#   }
#
#   # Create parcelled data
#   parcelled_data <- plspm_ip(pls_model$data, blocks)
#
#   # 관측된 상관 행렬 (Observed Correlation Matrix)
#   obs_cor <- cor(parcelled_data)
#
#   # 모델에 의해 예측된 상관 행렬 (Implied Correlation Matrix)
#   implied_cor <- cor(pls_model$scores)
#
#   # d_ULS (유클리드 거리) 계산: 관측된 상관 행렬과 예측된 상관 행렬 간의 차이를 제곱합으로 계산
#   residuals <- obs_cor - implied_cor
#   d_ULS <- sum(residuals^2)
#
#   # d_G (지오데식 거리) 계산: 지오데식 거리 계산
#   d_G <- sqrt(sum(log(diag(solve(implied_cor) %*% obs_cor))^2))
#
#   # 부트스트랩을 통한 신뢰구간 계산
#   d_ULS_samples <- numeric(bootstrap)
#   d_G_samples <- numeric(bootstrap)
#
#   # Progress bar 초기화
#   pb <- txtProgressBar(min = 0, max = bootstrap, style = 3)
#
#   for (i in 1:bootstrap) {
#     # Bootstrap resampling
#     resample_idx <- sample(1:nrow(pls_model$data), replace = TRUE)
#     resampled_data <- pls_model$data[resample_idx, ]
#     resampled_parcelled_data <- plspm_ip(resampled_data, blocks)
#
#     # 관측된 상관 행렬 (Observed Correlation Matrix) for resample
#     resampled_obs_cor <- cor(resampled_parcelled_data)
#
#     # d_ULS and d_G for resample
#     resampled_residuals <- resampled_obs_cor - implied_cor
#     d_ULS_samples[i] <- sum(resampled_residuals^2)
#     d_G_samples[i] <- sqrt(sum(log(diag(solve(implied_cor) %*% resampled_obs_cor))^2))
#
#     # Progress 업데이트
#     setTxtProgressBar(pb, i)
#   }
#
#   # Progress bar 닫기
#   close(pb)
#
#   # 신뢰구간 계산
#   lower_bound <- (1 - ci) / 2
#   upper_bound <- 1 - lower_bound
#
#   d_ULS_ci <- quantile(d_ULS_samples, probs = c(lower_bound, upper_bound))
#   d_G_ci <- quantile(d_G_samples, probs = c(lower_bound, upper_bound))
#
#   # 신뢰구간 내에 있으면 별표 표시
#   dULS_sig <- ifelse(d_ULS >= d_ULS_ci[1] & d_ULS <= d_ULS_ci[2], "*", "")
#   d_G_sig <- ifelse(d_G >= d_G_ci[1] & d_G <= d_G_ci[2], "*", "")
#
#   # 신뢰구간을 문자열 형태로 변환
#   d_ULS_ci_str <- paste0("[", round(d_ULS_ci[1], 3), ", ", round(d_ULS_ci[2], 3), "]")
#   d_G_ci_str <- paste0("[", round(d_G_ci[1], 3), ", ", round(d_G_ci[2], 3), "]")
#
#   # 결과 반환
#   result <- data.frame(
#     Method="Bollen-Stine",
#     d_ULS = round(d_ULS, 3),
#     d_ULS_95CI = d_ULS_ci_str,
#     dUsig = dULS_sig,
#     d_G = round(d_G, 3),
#     d_G_95CI = d_G_ci_str,
#     dGsig = d_G_sig
#   )
#   rownames(result)="index"
#   return(result)
# }
