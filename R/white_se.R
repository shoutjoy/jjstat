#' White의 수정된 표준오차를 계산하는 함수 정의
#'
#' @param original_se original_se
#' @param n sample size
#' @param k number of regression coefs
#' @param type all, white, roburst
#'
#' @return se data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 함수 사용 예시
#' original_se <- 0.009619  # 예시 표준오차
#' n <- 100  # 예시 표본 크기
#' k <- 2  # 예시 회귀 계수 개수 (절편 포함)
#'
#' # 함수 호출 및 결과 출력
#' white_se(original_se= 0.009619, n= 100, k=2)

#' }
#'
white_se <- function(original_se, n=NULL, k=2, type="white") {
  if (is.null(n)) {
    stop("You must input sample size n.")
  }

  # 헤테로스케다스티시티 조정 계수 계산
  adjustment_factor <- sqrt(n / (n - k))

  # 강건 표준오차 계산
  robust_se <- original_se * adjustment_factor


  # 결과 데이터프레임 생성
  all <- bind_cols(
    original_se = original_se,
    robust_se = robust_se
  )

  res1 <- all %>% pull(starts_with("robust"))
  names(res1) <- paste0("se_", letters[1:length(original_se)])

  res2 <- all %>% pull(starts_with("original"))

  switch(type, all = all, robust = res1, white = res1, ori = res2)
}
