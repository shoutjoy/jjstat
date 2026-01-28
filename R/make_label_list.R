#' Create Label Mapping List for Node Renaming
#'
#' 이 함수는 (원래이름, 변경라벨) 쌍으로 구성된 문자 벡터를 입력받아,
#' 각 쌍을 `list(node = ..., to = ...)` 형태의 리스트로 변환한다.
#' 주로 semPlot, qgraph, 사용자 정의 시각화 함수에서
#' 노드 이름 일괄 변경을 위해 사용된다.
#'
#' @param x character vector.
#'   반드시 (원래이름, 라벨) 쌍으로 구성되어야 하며,
#'   길이는 짝수여야 한다.
#'
#' @return A list of lists.
#'   각 원소는 다음 구조를 가진다:
#'   \itemize{
#'     \item node: 원래 변수명
#'     \item to: 변경할 라벨
#'   }
#'
#' @details
#' 입력 벡터의 길이가 홀수인 경우,
#' (원래이름, 라벨) 쌍이 완성되지 않으므로 오류를 발생시킨다.
#'
#' 내부적으로는 `split()`을 사용하여 2개씩 그룹화한 뒤,
#' `lapply()`를 통해 리스트 형태로 변환한다.
#'
#' @export
#'
#' @examples
#' # Example 1: 기본 사용
#' labels <- c(
#'   "REFF", "연구효능감",
#'   "INTR", "진로흥미",
#'   "OEXP", "결과기대"
#' )
#'
#' make_label_list(labels)
#'
#' # Example 2: semPlot/qgraph용 라벨 매핑
#' var_named <- c(
#'   "efficacy2", "연구효능2",
#'   "efficacy3", "연구효능3",
#'   "efficacy4", "연구효능4"
#' )
#'
#' label_list <- make_label_list(var_named)
#'
#' # label_list[[1]]
#' # $node
#' # [1] "efficacy2"
#' # $to
#' # [1] "연구효능2"
#'
make_label_list <- function(x) {

  # -------------------------
  # 1. 입력값 검증
  # -------------------------
  if (!is.vector(x) || !is.character(x)) {
    stop("x는 character vector여야 합니다.")
  }

  if (length(x) %% 2 != 0) {
    stop("입력 벡터는 (원래이름, 라벨) 쌍이어야 합니다.")
  }

  # -------------------------
  # 2. (원래이름, 라벨) 쌍으로 분할
  # -------------------------
  split_x <- split(x, rep(seq_len(length(x) / 2), each = 2))

  # -------------------------
  # 3. node-to 리스트 생성
  # -------------------------
  res <- lapply(split_x, function(v) {
    list(
      node = v[1],
      to   = v[2]
    )
  })

  return(res)
}
