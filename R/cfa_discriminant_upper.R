#' CFA 판별타당도 표의 상삼각 부분 표시
#'
#' @description
#' 잠재변수 이름을 나타내는 lv 열과 잠재변수 간 상관계수 열을 포함한
#' 표에서 대각선과 하삼각 부분을 비우고 상삼각 부분만 표시한다.
#' 상관계수는 지정한 소수점 자리수의 문자열로 변환하며,
#' 상관계수 이외의 열과 원래 행 및 열 순서는 유지한다.
#'
#' @param data 데이터프레임 또는 tibble. lv 열에는 각 행의 잠재변수
#'   이름이 있어야 하며, 각 잠재변수와 같은 이름의 숫자형 상관계수
#'   열이 모두 존재해야 한다. lv 값은 결측값 없이 고유해야 한다.
#' @param digits 0부터 15까지의 정수 하나. 표시할 소수점 자리수이다.
#'   기본값은 3이다.
#' @param blank 빈 셀을 표시할 문자열 하나. 기본값은 빈 문자열이다.
#'   대각선, 하삼각 부분 및 기존 상관계수의 결측값에 적용한다.
#'   NA_character_도 지정할 수 있다.
#'
#' @details
#' 상삼각 부분은 lv 열에 나타난 잠재변수 순서를 기준으로 정한다.
#' 따라서 상관계수 열의 배치 순서가 lv의 행 순서와 달라도
#' 각 잠재변수를 올바르게 대응시킨다.
#'
#' 이 함수는 이미 계산된 결과를 보고용으로 정리하는 함수이다.
#' CFA 모형 추정, AVE 계산, HTMT 계산 또는 판별타당도 충족 여부의
#' 검정을 수행하지 않는다. 숫자로 표시된 값이 상관계수인지 여부도
#' 별도로 검증하지 않는다.
#'
#' 빈 문자열과 고정 소수점 표시를 사용하므로 상관계수 열은
#' 문자형으로 반환된다. 추가 계산에는 변환 전의 자료를 사용한다.
#'
#' @return 입력과 같은 행 수와 열 순서를 갖는 tibble.
#'   잠재변수 상관계수 열은 문자형으로 변환되며, 대각선과
#'   하삼각 부분 및 기존 결측값은 blank로 대체된다.
#'   lv와 그 밖의 열은 입력값을 유지한다.
#'
#' @seealso \code{lower.tri}, \code{sprintf}, \code{tibble::as_tibble}
#' @export
#'
#' @examples
#' # 1. 기본 사용: lv와 같은 이름의 상관계수 열을 준비한다.
#' result <- data.frame(
#'   lv = c("F1", "F2", "F3"),
#'   AVE = c(0.60, 0.55, 0.65),
#'   F1 = c(1.00, 0.45, 0.38),
#'   F2 = c(0.45, 1.00, 0.52),
#'   F3 = c(0.38, 0.52, 1.00)
#' )
#' cfa_discriminant_upper(result)
#'
#' # 2. 소수점 두 자리 및 빈 셀 표시 변경
#' cfa_discriminant_upper(result, digits = 2, blank = "-")
#'
#' # 3. 빈 셀을 문자형 결측값으로 반환
#' cfa_discriminant_upper(result, blank = NA_character_)
#'
#' # 4. 상관계수 열의 배치가 달라도 lv 순서를 기준으로 처리
#' reordered <- result[c("lv", "AVE", "F3", "F1", "F2")]
#' cfa_discriminant_upper(reordered)
#'
#' # 5. 잠재변수가 하나이면 상관계수 셀은 비워진다.
#' single <- data.frame(lv = "F1", AVE = 0.60, F1 = 1)
#' cfa_discriminant_upper(single)
#'
#' # 6. 상삼각 부분에 있는 기존 결측값도 blank로 표시
#' missing <- result
#' missing$F3[1] <- NA_real_
#' cfa_discriminant_upper(missing, blank = "-")
cfa_discriminant_upper <- function(data, digits = 3, blank = "") {

  # 1. 입력 자료와 표시 옵션 확인
  if (!is.data.frame(data)) {
    stop("data는 데이터프레임 또는 tibble이어야 합니다.", call. = FALSE)
  }
  if (anyNA(names(data)) || any(!nzchar(names(data))) ||
      anyDuplicated(names(data))) {
    stop("열 이름은 결측값이나 빈 문자열 없이 고유해야 합니다.",
         call. = FALSE)
  }
  if (!"lv" %in% names(data)) {
    stop("data에 잠재변수 이름을 담은 lv 열이 필요합니다.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L ||
      is.na(digits) || !is.finite(digits) ||
      digits < 0 || digits > 15 || digits != floor(digits)) {
    stop("digits는 0부터 15까지의 정수 하나여야 합니다.", call. = FALSE)
  }
  if (!is.character(blank) || length(blank) != 1L) {
    stop("blank는 문자열 하나 또는 NA_character_이어야 합니다.",
         call. = FALSE)
  }

  # 2. 잠재변수 이름 및 대응하는 상관계수 열 확인
  df <- as.data.frame(data)
  lv <- as.character(df$lv)
  if (length(lv) == 0L || anyNA(lv) || any(!nzchar(trimws(lv))) ||
      anyDuplicated(lv) || "lv" %in% lv) {
    stop("lv는 비어 있지 않은 고유한 이름이어야 하며, 이름 'lv'는 사용할 수 없습니다.",
         call. = FALSE)
  }
  absent <- setdiff(lv, names(df))
  if (length(absent) > 0L) {
    stop("다음 잠재변수의 상관계수 열이 없습니다: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  numeric_cols <- vapply(df[lv], function(x) {
    is.numeric(x) && is.null(dim(x))
  }, logical(1))
  if (!all(numeric_cols)) {
    stop("상관계수 열은 숫자형 벡터여야 합니다: ",
         paste(lv[!numeric_cols], collapse = ", "), call. = FALSE)
  }

  # 3. 행의 잠재변수 순서에 맞게 열을 정렬하고 대각선 및 하삼각 제거
  M <- as.matrix(df[, lv, drop = FALSE])
  M[lower.tri(M, diag = TRUE)] <- NA_real_

  # 4. 차원 축소 없이 각 열을 고정 소수점 문자열로 변환
  formatted <- lapply(seq_along(lv), function(j) {
    x <- M[, j]
    out <- rep(blank, length(x))
    keep <- !is.na(x)
    out[keep] <- sprintf(paste0("%.", as.integer(digits), "f"), x[keep])
    out
  })
  names(formatted) <- lv

  # 5. 이름으로 원래 열에 대입하여 나머지 열과 전체 열 순서 유지
  df[lv] <- formatted
  tibble::as_tibble(df)
}
