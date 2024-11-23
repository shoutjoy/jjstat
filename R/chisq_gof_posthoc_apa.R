
#' chisq_gof_posthoc_apa
#'
#' @param df df
#' @param adj adj
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'
chisq_gof_posthoc_apa <- function(df, adj = FALSE) {
  # 결과 저장용 벡터
  results <- c("카이제곱 적합도 검정 사후분석(Post Hoc) 결과, ")

  # 각 열의 데이터 타입을 확인 및 변환
  df$chisq <- as.numeric(df$chisq)  # chisq 열을 숫자로 변환
  df$df <- as.integer(df$df)        # df 열을 정수로 변환
  df$p.value <- as.numeric(df$p.value)  # p.value 열을 숫자로 변환
  df$adj.p <- as.numeric(df$adj.p)  # adj.p 열을 숫자로 변환 (필요 시)

  # 각 행을 순회하며 결과 생성
  for (i in seq_len(nrow(df))) {
    # 각 열의 값 가져오기
    pairwise <- df$pairwise[i]
    chisq_value <- df$chisq[i]
    df_value <- df$df[i]
    p_value <- if (adj) df$adj.p[i] else df$p.value[i]
    p_sig <- if (adj) df$adj.p_sig[i] else df$p_sig[i]

    # "ns" 값을 빈 문자열로 변경
    if (p_sig == "ns") {
      p_sig <- ""
    }

    # 통계적 판단 메시지
    conclusion <- if (p_value < 0.05) {
      "통계적으로 유의한 차이가 나타났다"
    } else {
      "통계적으로 유의한 차이는 나타나지 않았다"
    }

    # 결과 설명 생성
    result <- sprintf(
      "가설[%d]: %s은 %s (chisq(df=%d) = %.2f %s, p = %.4e).",
      i, pairwise, conclusion, df_value, chisq_value, p_sig, p_value
    )

    # 결과 저장
    results <- c(results, result)
  }

  # 결과 반환
  return(cat(paste(results, collapse = " "), "\n"))
}
