#'  Bollen-Stine Bootstrap based d_ULS and d_G calculation
#'
#' @param pls_model plsreslut
#' @param blocks model block
#' @param bootstrap default 1000
#' @param ci 95% ci
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' # d_ULS 및 d_G 지표에 대한 신뢰구간을 계산하려면,
#'  #Bollen-Stine 부트스트랩 절차를 통해 신뢰구간을
#'  #제시해야 합니다. 이 부트스트랩 절차는 일반적인
#'  #부트스트랩 방식과는 다르게 관측된 데이터를 여러
#'  #번 재추출하여 신뢰구간을 계산하는 방식입니다.
#'  #PLS-SEM에서 Bollen-Stine 부트스트랩을 사용하는
#'  #이유는 모형의 정확도를 평가하고, 특정 지표(d_ULS, d_G, SRMR 등)의
#'  #신뢰구간을 도출하기 위해서입니다.
#' #d_ULS는 마치 지도에서 두 지점 사이의 직선 거리를 측정하는 것과 비슷하다.
#' #즉, 각 변수 간의 차이를 개별적으로 계산해, 그 차이를 더하는 방식이다.
#'  # d_G는 산을 넘어가는 경로의 거리를 계산하는 것과 비슷하다.
#'   # 두 지점 사이의 거리를 단순히 직선으로 잴 수 없는 경우, 산의
#'   # 기울기와 복잡한 지형을 고려하여 최단 경로를 계산하는 방식이다.
#'   # 즉, 단순한 값 차이가 아니라, 데이터가 가지고 있는 전체적인
#'   # 구조적 특성을 고려하여 평가하는 것이다#'
#' #'
#' # Example call to the dULS_modelfit function with 1000 bootstrap samples
#' plspm_dULS_modelfit(edt_pls, edblock, bootstrap = 1000, ci = 0.95)
#' #'
#'   }
plspm_dULS_modelfit <- function(pls_model, blocks, bootstrap = 1000, ci = 0.95) {
  # Item Parceling function for plspm
  plspm_ip <- function(data, blocks) {
    # Create an empty data frame with the same number of rows as the original data
    parcelled_data <- data.frame(matrix(nrow = nrow(data), ncol = 0))

    # Iterate over each block in the blocks list
    for (block_name in names(blocks)) {
      block_vars <- blocks[[block_name]]

      # Check if all block variables exist in the data
      if (!all(block_vars %in% colnames(data))) {
        stop(paste("Some variables in block", block_name, "are not in the data"))
      }

      # Calculate the mean of the block variables (Item Parceling)
      parcelled_data[[block_name]] <- rowMeans(data[, block_vars], na.rm = TRUE)
    }

    # Return the parcelled data as a data frame
    return(parcelled_data)
  }

  # Create parcelled data
  parcelled_data <- plspm_ip(pls_model$data, blocks)

  # 관측된 상관 행렬 (Observed Correlation Matrix)
  obs_cor <- cor(parcelled_data)

  # 모델에 의해 예측된 상관 행렬 (Implied Correlation Matrix)
  implied_cor <- cor(pls_model$scores)

  # d_ULS (유클리드 거리) 계산: 관측된 상관 행렬과 예측된 상관 행렬 간의 차이를 제곱합으로 계산
  residuals <- obs_cor - implied_cor
  d_ULS <- sum(residuals^2)

  # d_G (지오데식 거리) 계산: 지오데식 거리 계산
  d_G <- sqrt(sum(log(diag(solve(implied_cor) %*% obs_cor))^2))

  # 부트스트랩을 통한 신뢰구간 계산
  d_ULS_samples <- numeric(bootstrap)
  d_G_samples <- numeric(bootstrap)

  # Progress bar 초기화
  pb <- txtProgressBar(min = 0, max = bootstrap, style = 3)

  for (i in 1:bootstrap) {
    # Bootstrap resampling
    resample_idx <- sample(1:nrow(pls_model$data), replace = TRUE)
    resampled_data <- pls_model$data[resample_idx, ]
    resampled_parcelled_data <- plspm_ip(resampled_data, blocks)

    # 관측된 상관 행렬 (Observed Correlation Matrix) for resample
    resampled_obs_cor <- cor(resampled_parcelled_data)

    # d_ULS and d_G for resample
    resampled_residuals <- resampled_obs_cor - implied_cor
    d_ULS_samples[i] <- sum(resampled_residuals^2)
    d_G_samples[i] <- sqrt(sum(log(diag(solve(implied_cor) %*% resampled_obs_cor))^2))

    # Progress 업데이트
    setTxtProgressBar(pb, i)
  }

  # Progress bar 닫기
  close(pb)

  # 신뢰구간 계산
  lower_bound <- (1 - ci) / 2
  upper_bound <- 1 - lower_bound

  d_ULS_ci <- quantile(d_ULS_samples, probs = c(lower_bound, upper_bound))
  d_G_ci <- quantile(d_G_samples, probs = c(lower_bound, upper_bound))

  # 신뢰구간 내에 있으면 별표 표시
  dULS_sig <- ifelse(d_ULS >= d_ULS_ci[1] & d_ULS <= d_ULS_ci[2], "*", "")
  d_G_sig <- ifelse(d_G >= d_G_ci[1] & d_G <= d_G_ci[2], "*", "")

  # 신뢰구간을 문자열 형태로 변환
  d_ULS_ci_str <- paste0("[", round(d_ULS_ci[1], 3), ", ", round(d_ULS_ci[2], 3), "]")
  d_G_ci_str <- paste0("[", round(d_G_ci[1], 3), ", ", round(d_G_ci[2], 3), "]")

  # 결과 반환
  result <- data.frame(
    Method="Bollen-Stine",
    d_ULS = round(d_ULS, 3),
    d_ULS_95CI = d_ULS_ci_str,
    dUsig = dULS_sig,
    d_G = round(d_G, 3),
    d_G_95CI = d_G_ci_str,
    dGsig = d_G_sig
  )
  rownames(result)="index"
  return(result)
}
