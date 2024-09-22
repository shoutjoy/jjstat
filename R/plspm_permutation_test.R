#' 잠재변수 평균값과 분산의 동일성을 평가하는 Permutation Test 함수
#'
#' @param Data Data
#' @param path_matrix   path_matrix
#' @param blocks  blocks
#' @param grp grp = "gender"
#' @param n_perm default 100
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' #'
#' jutpath1 = plspm_paths(
#'   row_names = c("SelfEff","CarMot","CarAtt","CarAct"),
#'   relationship = list(
#'     path(from="SelfEff", to=c("CarMot","CarAtt","CarAct")),
#'     path("CarMot", c("CarAct")),
#'     path("CarAtt", c("CarAct"))
#'   )  )
#' jutpath1
#'
#'
#' jut_blocks <- plspm_blocks(
#'   SelfEff = list("sef01", "sef02", "sef03", "sef04", "sef05"),
#'   CarMot = list("mot01","mot02","mot03"),
#'   #   CarAtt = item( "att01", "att02","att03","att04","att05" ),
#'   CarAtt = list("att03","att04","att05" ),
#'   CarAct = list("act01", "act02","act03")
#' )
#' jut_blocks
#'
#'
#' result_df <- plspm_permutation_test(Data = jut7c, path_matrix = jutpath1, blocks = jut_blocks, grp = "P01")
#'
#'
#' result_df
#' result_df %>%Round(17)%>%dall()
#'
#'
#' }
#'
plspm_permutation_test <- function(Data, path_matrix, blocks, grp, n_perm = 100) {
  #micom_permutation_test
  library(progress)
  # progress 패키지 로드
  # 그룹 변수를 기준으로 두 그룹으로 나누기
  unique_groups <- unique(Data[[grp]])  # 그룹 변수에서 고유한 값(예: 'male', 'female')을 추출

  if (length(unique_groups) != 2) {
    stop("그룹 변수는 두 개의 고유한 값만 가져야 합니다.")  # 2개의 그룹만 지원
  }

  # 첫 번째 그룹 데이터
  grp1 <- Data[Data[[grp]] == unique_groups[1], ]

  # 두 번째 그룹 데이터
  grp2 <- Data[Data[[grp]] == unique_groups[2], ]

  # 각 그룹에 대해 PLS 분석 실행
  plspm_grp1 <- plspm(grp1, path_matrix, blocks, modes = rep("A", length(blocks)))
  plspm_grp2 <- plspm(grp2, path_matrix, blocks, modes = rep("A", length(blocks)))

  # 각 그룹의 잠재변수 점수 추출
  grp1_scores <- plspm_grp1$scores  # 잠재변수 점수
  grp2_scores <- plspm_grp2$scores  # 잠재변수 점수

  # 각 그룹에 대해 잠재변수의 평균값과 분산 계산
  grp1_means <- colMeans(grp1_scores)
  grp2_means <- colMeans(grp2_scores)
  grp1_vars <- apply(grp1_scores, 2, var)
  grp2_vars <- apply(grp2_scores, 2, var)

  # Permutation Test 수행
  perm_means_diff <- matrix(NA, nrow = n_perm, ncol = length(grp1_means))
  perm_vars_diff <- matrix(NA, nrow = n_perm, ncol = length(grp1_vars))

  # 그룹 데이터를 합친 후 섞기
  combined_data <- rbind(grp1, grp2)  # 두 그룹 데이터를 합침

  # progress bar 설정
  pb <- progress_bar$new(
    format = "  Permutation Test 진행 [:bar] :percent eta: :eta",
    total = n_perm, clear = FALSE, width = 60
  )

  for (i in 1:n_perm) {
    # 진행 상태 업데이트
    pb$tick()  # progress bar 업데이트

    # 두 그룹 데이터를 무작위로 섞음
    perm_data <- combined_data[sample(nrow(combined_data)), ]

    # 섞은 데이터를 다시 두 그룹으로 나눔
    perm_grp1 <- perm_data[1:nrow(grp1), ]
    perm_grp2 <- perm_data[(nrow(grp1) + 1):nrow(combined_data), ]

    # 무작위로 재구성된 그룹으로 PLS 모형 실행
    plspm_perm1 <- plspm(perm_grp1, path_matrix, blocks, modes = rep("A", length(blocks)))
    plspm_perm2 <- plspm(perm_grp2, path_matrix, blocks, modes = rep("A", length(blocks)))

    # 각 그룹의 잠재변수 점수 추출
    perm_grp1_scores <- plspm_perm1$scores
    perm_grp2_scores <- plspm_perm2$scores

    # 각 그룹의 잠재변수 평균값과 분산 계산
    perm_means_diff[i, ] <- colMeans(perm_grp1_scores) - colMeans(perm_grp2_scores)
    perm_vars_diff[i, ] <- apply(perm_grp1_scores, 2, var) - apply(perm_grp2_scores, 2, var)
  }

  # 평균값의 차이에 대한 p-값 계산
  means_diff_orig <- grp1_means - grp2_means
  p_values_means <- colMeans(abs(perm_means_diff) >= matrix(rep(abs(means_diff_orig), n_perm), nrow = n_perm, byrow = TRUE))

  # 분산의 차이에 대한 p-값 계산
  vars_diff_orig <- grp1_vars - grp2_vars
  p_values_vars <- colMeans(abs(perm_vars_diff) >= matrix(rep(abs(vars_diff_orig), n_perm), nrow = n_perm, byrow = TRUE))

  # 결과 데이터프레임 생성
  result_df <- data.frame(
    latent = colnames(grp1_scores),
    grp1_mean = grp1_means,
    grp2_mean = grp2_means,
    mean_diff = means_diff_orig,
    mean_p = p_values_means,
    grp1_var = grp1_vars,
    grp2_var = grp2_vars,
    var_diff = vars_diff_orig,
    var_p = p_values_vars
  )

  return(result_df)
}
