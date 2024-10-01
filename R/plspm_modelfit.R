#' plspm_modelfit
#'
#' @param pls_model plsresul
#' @param blocks model block
#' @param bootstrap bootstrap 1000
#' @param ci ci 95%
#' @param digits defaul 5
#' @param trans trans result
#'
#' @return datatable
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #'
#' #meausrement
#' edblock = plspm_blocks(
#'   SUX = item(paste0("sux",1:6)),
#'   FLOW = item(paste0("flow",1:5)),
#'   PEOU = item(paste0("peou",1:5)),
#'   PU = item(paste0("pu",1:5)),
#'   ATT = item(paste0("att",1:3)),
#'   USE = item(paste0("use",1:3))
#'   # dataset = ed_tam2
#' )
#' edblock
#' #Seed value: 19995
#' edt_pls = plspm_sem(Data=ed_tam2, edmodel, edblock)
#'
#'
#' # Example call to the plspm_modelfit function
#' plspm_modelfit(edt_pls, edblock)
#' #' # Example call to the plspm_modelfit function with 1000 bootstrap samples
#' rest= plspm_modelfit(edt_pls, edblock, bootstrap = 1000, ci = 0.95, trans=FALSE)
#' rest
#' rest%>%t()%>%rowdata2col(1)%>%data.frame()
#'
#' rest2= plspm_modelfit(edt_pls, edblock, bootstrap = 1000, ci = 0.95, trans=TRUE)
#' rest2

#' }
#'
#'
plspm_modelfit <- function(pls_model, blocks,
                           bootstrap = 1000, ci = 0.95, digits = 5,
                           trans= TRUE) {
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
       parcelled_data
  }

  # Create parcelled data
  parcelled_data <- plspm_ip(pls_model$data, blocks)

  # 관측된 상관 행렬 (Observed Correlation Matrix)
  obs_cor <- cor(parcelled_data)

  # 모델에 의해 예측된 상관 행렬 (Implied Correlation Matrix)
  implied_cor <- cor(pls_model$scores)

  # Null Model (diagonal correlation matrix)
  null_cor <- diag(diag(obs_cor))

  # Chi-square for the proposed model
  proposed_chi_square <- sum(diag(solve(implied_cor) %*% obs_cor)) - ncol(obs_cor) +
    log(det(implied_cor)) - log(det(obs_cor))

  # Chi-square for the null model
  null_chi_square <- sum(diag(solve(null_cor) %*% obs_cor)) - ncol(obs_cor) +
    log(det(null_cor)) - log(det(obs_cor))

  # 자유도 (degrees of freedom)
  df <- (ncol(obs_cor) * (ncol(obs_cor) - 1)) / 2

  # p-value 계산 (Chi-square 분포의 상위 테일에서 p값 계산)
  p_value <- 1 - pchisq(proposed_chi_square, df)

  # SRMR 계산: 잔차 행렬의 하삼각 행렬에 대한 제곱 평균 오차의 제곱근
  residuals <- obs_cor - implied_cor
  srmr <- sqrt(mean(residuals[lower.tri(residuals)]^2))

  # NFI 계산
  nfi <- (null_chi_square - proposed_chi_square) / null_chi_square

  # d_ULS와 d_G 계산
  d_ULS <- sum(residuals^2)
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

  # 신뢰구간을 문자열 형태로 변환
  d_ULS_ci_str <- paste0("[", round(d_ULS_ci[1], digits), ", ", round(d_ULS_ci[2], digits), "]")
  d_G_ci_str <- paste0("[", round(d_G_ci[1], digits), ", ", round(d_G_ci[2], digits), "]")

  # SRMR 기준 해석
  srmr_eval <- ifelse(srmr <= 0.05, "매우적합(<0.05)",
                      ifelse(srmr <= 0.08, "적합(<.08)", "부적합(>0.1)"))

  # NFI 기준 해석
  nfi_eval <- ifelse(nfi >= 0.90, "적합(>.90)", "부적합")

  # p-value 기준 해석 (p > 0.05일 때 적합)
  p_value_eval <- ifelse(p_value > 0.05, "적합(>0.05)", "부적합(<.05)")

  # 결과를 데이터 프레임으로 반환
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

  # 적합도 기준을 두 번째 행에 추가
  evaluation <- data.frame(
    model = "evaluation",
    Chisq = "-",  # Chi-square 평가 제외
    df = "-",     # df 평가 제외
    p_value = p_value_eval, # p-value 기준 추가
    SRMR = srmr_eval,
    NFI = nfi_eval,
    d_ULS_EuclideanDist = d_ULS_ci_str,
    d_G_GeodesicDist = d_G_ci_str
  )

  # 결과에 적합도 평가 기준을 추가하여 반환
  final_result <- rbind(result, evaluation)

  if(trans){
    final_result=final_result%>%t()%>%rowdata2col(1)%>%data.frame()
    return(final_result)
  }else{
    return(final_result)
  }

}
