#' Freq_rowcounter
#'
#' @param df df
#' @param type counter, term, freq_counter, freq_term
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  Freq_rowcounter(df = eduteck2, type = "counter")
#' Freq_rowcounter(df = eduteck2, type = "freq_counter")
#' }
Freq_rowcounter <- function(df, type = "counter") {
  # 1. ""가 아닌 것의 개수를 세서 counter 열을 추가
  df$counter <- apply(df, 1, function(x) sum(x != "" & !is.na(x)))

  # 2. term 열을 추가하여 "초", "중", "고" 값을 구분하고 첫 번째 유효한 학년을 추출
  df$term <- apply(df, 1, function(x) {
    term <- x[grep("초|중|고", x)]  # "초", "중", "고" 중 첫 번째 학년 값만 추출
    if (length(term) > 0) {
      return(substr(term[1], 1, 1))  # 첫 번째 값에서 "초", "중", "고"만 반환
    } else {
      return("Missing")
    }
  })

  # 3. counter 열의 빈도 분석
  freq_counter <- df %>%
    group_by(counter) %>%
    summarise(Freq = n()) %>%
    mutate(`prop(%)` = round((Freq / sum(Freq)) * 100, 2))

  # 4. term 열의 빈도 분석
  freq_term <- df %>%
    group_by(term) %>%
    summarise(Freq = n()) %>%
    mutate(`prop(%)` = round((Freq / sum(Freq)) * 100, 2))

  # 5. type 옵션에 따른 반환 값
  result <- switch(type,
                   "counter" = df %>% select(counter),
                   "term" = df %>% select(term),
                   "freq_counter" = freq_counter,
                   "freq_term" = freq_term)

  return(result)
}
