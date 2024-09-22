#' 지표의 균일성 평가를 위한 Permutation Test 함수 (병렬 처리 적용)
#'
#' @param Data data
#' @param path_matrix path
#' @param blocks measurement
#' @param grp grp
#' @param n_perm permutation number
#' @param n_boot boot n
#' @param n_cores cores=8 (recently 12~15), exceapt one !!!
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시: 지표의 균일성 평가 실행 (병렬 처리 적용)
#' result_df2 <- plspm_indicator_test(Data = jut7c, path_matrix = jutpath1, blocks = jut_blocks, grp = "P01")
#' print(result_df2)
#' }
#'
plspm_indicator_test <- function(Data, path_matrix, blocks, grp, n_perm = 100, n_boot = 100, n_cores = 8) {
  library(plspm)
  library(parallel)
  library(progress)

  # 병렬 처리 설정
  if (is.null(n_cores)) {
    n_cores <- min(8, parallel::detectCores() - 1)  # 사용 가능한 코어 수를 감지 (기본값 8, 시스템 제한 고려)
  }

  cl <- parallel::makeCluster(n_cores)  # 클러스터 설정
  clusterEvalQ(cl, library(plspm))  # 각 클러스터 노드에서 'plspm' 패키지 로드

  # 필요한 객체를 클러스터에 전달 (path_matrix, blocks 등)
  clusterExport(cl, varlist = c("path_matrix", "blocks", "n_boot"), envir = environment())

  # 그룹 변수를 기준으로 두 그룹으로 나누기
  unique_groups <- unique(Data[[grp]])  # 그룹 변수에서 고유한 값(예: 'male', 'female')을 추출

  if (length(unique_groups) != 2) {
    stop("그룹 변수는 두 개의 고유한 값만 가져야 합니다.")  # 2개의 그룹만 지원
  }

  # 첫 번째 그룹 데이터
  grp1 <- Data[Data[[grp]] == unique_groups[1], ]

  # 두 번째 그룹 데이터
  grp2 <- Data[Data[[grp]] == unique_groups[2], ]

  # 병렬 처리로 부트스트랩 적용된 PLS 분석 수행
  results <- parallel::parLapply(cl, list(grp1, grp2), function(data) {
    plspm(data, path_matrix, blocks, modes = rep("A", length(blocks)), boot.val = TRUE, br = n_boot)
  })

  plspm_grp1 <- results[[1]]  # 첫 번째 그룹의 PLS 결과
  plspm_grp2 <- results[[2]]  # 두 번째 그룹의 PLS 결과

  # 로딩 값 및 부트스트랩 표준 오차 추출
  grp1_loadings <- plspm_grp1$boot$loadings$Original
  grp2_loadings <- plspm_grp2$boot$loadings$Original
  grp1_se <- plspm_grp1$boot$loadings$Std.Error
  grp2_se <- plspm_grp2$boot$loadings$Std.Error

  # 원래 데이터에서의 표준 오차 차이 계산
  se_diff_orig <- grp1_se - grp2_se

  # progress bar 설정 (메인 프로세스에서 관리)
  pb <- progress_bar$new(
    format = "  Permutation Test 진행 [:bar] :percent eta: :eta",
    total = n_perm, clear = FALSE, width = 60
  )

  # Permutation Test 수행
  perm_results <- vector("list", n_perm)  # 결과 저장
  for (i in 1:n_perm) {
    pb$tick()  # 진행 상태 업데이트

    # 병렬 처리로 Permutation 부트스트랩 분석 수행
    perm_results[[i]] <- parallel::parLapply(cl, 1:n_cores, function(core_id) {
      # 두 그룹 데이터를 무작위로 섞음
      perm_data <- combined_data[sample(nrow(combined_data)), ]

      # 섞은 데이터를 다시 두 그룹으로 나눔
      perm_grp1 <- perm_data[1:nrow(grp1), ]
      perm_grp2 <- perm_data[(nrow(grp1) + 1):nrow(combined_data), ]

      # 병렬 처리로 Permutation 부트스트랩 분석 수행
      perm_plspm_grp1 <- plspm(perm_grp1, path_matrix, blocks, modes = rep("A", length(blocks)), boot.val = TRUE, br = n_boot)
      perm_plspm_grp2 <- plspm(perm_grp2, path_matrix, blocks, modes = rep("A", length(blocks)), boot.val = TRUE, br = n_boot)

      # 각 그룹의 부트스트랩 표준 오차 추출
      perm_grp1_se <- perm_plspm_grp1$boot$loadings$Std.Error
      perm_grp2_se <- perm_plspm_grp2$boot$loadings$Std.Error

      # 각 그룹의 표준 오차 차이를 반환 (각 지표별로 차이 계산)
      return(perm_grp1_se - perm_grp2_se)
    })
  }

  perm_se_diff <- do.call(rbind, perm_results)

  # p-값 계산: 각 지표별로 Permutation된 차이가 원래 차이보다 큰 경우의 비율
  p_values <- colMeans(abs(perm_se_diff) >= matrix(rep(abs(se_diff_orig), n_perm), nrow = n_perm, byrow = TRUE))

  # 병렬 작업 종료
  parallel::stopCluster(cl)

  # 결과 데이터프레임 생성 (로딩 값과 부트스트랩 표준 오차를 모두 포함)
  result_df <- data.frame(
    indicator = rownames(plspm_grp1$boot$loadings),
    grp1_loading = grp1_loadings,
    grp2_loading = grp2_loadings,
    grp1_se = grp1_se,
    grp2_se = grp2_se,
    se_diff = se_diff_orig,
    p_value = p_values
  )

  return(result_df)
}
