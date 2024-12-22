#' Perform Bootstrap and Permutation Tests for PLS-PM Indicators Across Groups
#'
#' This function performs bootstrap and permutation tests for PLS-PM indicators across
#' selected group pairs or all possible group pairs in the dataset. The results include
#' group-specific estimates, standard errors, p-values, and invariance checks.
#'
#' @param Data A data frame containing the dataset to be analyzed.
#' @param path_matrix A square matrix defining the path relationships among constructs in the PLS-PM model.
#' @param blocks A list defining the measurement blocks (i.e., sets of observed variables) for each construct.
#' @param grp A string specifying the name of the grouping variable in `Data`.
#' @param n_perm Number of permutation tests to perform. Default is 50.
#' @param n_boot Number of bootstrap resamples to perform. Default is 50.
#' @param n_cores Number of cores to use for parallel processing. Default is 12.
#' @param grp_select A character vector of length 2 specifying the group pair to compare. Default is NULL (compares all pairs).
#'
#' @return A tibble containing the test results for all indicators, including estimates, standard errors, p-values, and comparisons.
#' The returned tibble includes the following columns:
#' - `indicator`: The indicator being tested.
#' - `g1_est`: The estimate for the first group.
#' - `g2_est`: The estimate for the second group.
#' - `g1_se`: The standard error for the first group.
#' - `g2_se`: The standard error for the second group.
#' - `se_diff`: The difference in standard errors between the groups.
#' - `p_value`: The p-value for the test.
#' - `invariance`: Whether the indicator is invariant between the groups (`"OK"` or `"ns"`).
#' - `comparison`: The comparison between the two groups (e.g., "group1_vs_group2").
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- plspm_indicator_test_multi(
#'   Data = ed_tam2_st,
#'   path_matrix = edmodel,
#'   blocks = edblock,
#'   grp = "Class",
#'   grp_select = c("1", "3"),
#'   n_perm = 100,
#'   n_boot = 50,
#'   n_cores = 13
#' )
#' print(result)
#' }
plspm_indicator_test_multi <- function(Data, path_matrix,
                                       blocks, grp, n_perm = 50,
                                       n_boot = 50, n_cores = 12,
                                       grp_select = NULL) {
  # 타이머 시작
  start_time <- base::Sys.time()

  # 병렬 처리 설정 (future 패키지 사용)
  future::plan(future::multicore, workers = n_cores)

  # 그룹 확인
  unique_groups <- base::as.character(base::unique(Data[[grp]]))

  if (base::length(unique_groups) < 2) {
    base::stop("그룹 변수는 최소 두 개의 고유 값을 가져야 합니다.")
  }

  # 특정 그룹 선택
  if (!base::is.null(grp_select)) {
    if (base::length(grp_select) != 2) {
      base::stop("grp_select는 길이가 2인 문자 벡터여야 합니다.")
    }

    if (base::any(!grp_select %in% unique_groups)) {
      base::stop("grp_select에 유효하지 않은 그룹 이름이 포함되어 있습니다.")
    }

    # 선택된 그룹 쌍
    group_pairs <- base::list(grp_select)
  } else {
    # 모든 그룹 쌍 생성
    group_pairs <- utils::combn(unique_groups, 2, simplify = FALSE)
  }

  # 메시지 출력
  base::message("\nStarting bootstrap and indicator tests for all group pairs...")

  # 그룹 쌍별 처리
  results_list <- lapply(group_pairs, function(pair) {
    # 현재 시간과 경과 시간 계산
    current_time <- base::Sys.time()
    elapsed_time <- base::difftime(current_time, start_time, units = "secs")

    # 진행 메시지 출력
    base::message(base::sprintf("Processing pair: %s vs %s | Elapsed time: %f seconds",
                                pair[1], pair[2], elapsed_time))

    grp1 <- Data[Data[[grp]] == pair[1], ]
    grp2 <- Data[Data[[grp]] == pair[2], ]

    # 데이터 샘플링 (옵션)
    grp1_sample <- grp1[base::sample(1:base::nrow(grp1),
                                     size = base::min(100, base::nrow(grp1))), ]
    grp2_sample <- grp2[base::sample(1:base::nrow(grp2),
                                     size = base::min(100, base::nrow(grp2))), ]
    pair_data <- base::rbind(grp1_sample, grp2_sample)

    # plspm_indicator_test 실행
    test_result <- plspm_indicator_test(pair_data, path_matrix,
                                        blocks, grp, n_perm, n_boot, n_cores)

    # 결과 저장
    test_result$group1 <- pair[1]
    test_result$group2 <- pair[2]
    return(test_result)
  })

  # 결과를 데이터프레임으로 변환
  results_df <- base::do.call(base::rbind, lapply(results_list, function(res) {
    res <- dplyr::as_tibble(res)  # tibble 형식으로 변환
    res <- dplyr::mutate(res, comparison = base::paste(res$group1[1],
                                                       res$group2[1], sep = "_vs_"))
    return(res)
  }))

  # 열 이름 변경 및 g1, g2 열 제거
  colnames(results_df) <- c("indicator", "g1_est", "g2_est", "g1_se",
                            "g2_se", "se_diff", "p_value", "invariance",
                            "g1", "g2", "comparison")
  results_df <- dplyr::select(results_df, -g1, -g2)

  # 타이머 종료
  end_time <- base::Sys.time()
  elapsed_time <- base::difftime(end_time, start_time, units = "secs")
  base::message("\nAll pairwise tests have been completed!")
  base::message(base::sprintf("Total time taken: %f seconds", elapsed_time))

  return(results_df)
}
