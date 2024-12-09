
#' Interpreting frequency analysis results from Freq_table
#'
#' @param df data
#' @param title title =""
#' @param print df output
#' @param digits 2
#'
#' @return apa
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' Mtcars = mtcars
#'
#' Mtcars$am = factor(Mtcars$am, levels=c(0,1), labels= c("automatic","manual" ))
#' Mtcars$vs  = factor(Mtcars$vs, levels=c(0,1), labels= c("V-shaped","straight" ))
#' Mtcars$cyl  = factor(Mtcars$cyl, levels=c(4,6,8), labels= c("cyl-4","cyl-6","cyl-8"))
#'
#' Mtcars %>% Freq_table("am","vs", prop=T) %>% Freq_table_apa()
#' Mtcars %>% table_freq("vs","am","cyl") %>% Freq_table_apa()
#' Mtcars %>% table_freq("vs","am","cyl", angle=90)%>% Freq_table_apa()
#'}
#'

Freq_table_apa <- function(df, title = "", print = TRUE, digits = 2) {

  # 컬럼명 추출
  cols <- colnames(df)

  # Freq와 prop(%)는 각각 "횟수"와 "비율"로 읽어냄
  freq_col <- "Freq"
  prop_col <- "prop(%)"

  # 나머지 컬럼 처리 (Freq와 prop(%) 제외)
  term_cols <- setdiff(colnames(df), c(freq_col, prop_col))

  if (length(term_cols) > 1) {
    # 여러 컬럼이 있으면 이를 하나의 term으로 합침, prop(%)는 제외
    df$term_combined <- apply(df[, term_cols], 1, paste, collapse = "_")
    term_col <- "term_combined"
  } else {
    term_col <- term_cols[1]
  }

  # 괄호를 제거하고 앞부분만 반환하는 함수
  remove_parentheses <- function(term) {
    sub("\\(.*\\)", "", term)
  }

  # 받침이 있는지 확인하는 함수
  has_batchim <- function(term) {
    last_char <- substr(term, nchar(term), nchar(term))
    # 유니코드 값 확인
    code <- utf8ToInt(last_char)
    (code - 44032) %% 28 != 0
  }

  # 빈도와 비율을 포함한 경우
  if (prop_col %in% colnames(df)) {
    result <- paste0(title, "빈도분석 결과, 각 빈도와 비율은 다음과 같았다. ")

    for (i in 1:nrow(df)) {
      term_no_parentheses <- remove_parentheses(df[[term_col]][i])  # 괄호 제거
      particle <- ifelse(has_batchim(term_no_parentheses), "은", "는")
      result <- paste0(result, df[[term_col]][i], particle, " ", df[[freq_col]][i], "명(",
                       round(df[[prop_col]][i], digits), "%)",
                       ifelse(i == nrow(df), "이였다.", ", "))
    }

    # 빈도만 있는 경우
  } else {
    result <- paste0(title, "빈도분석 결과, 각 빈도와 비율은 다음과 같았다. ")

    for (i in 1:nrow(df)) {
      term_no_parentheses <- remove_parentheses(df[[term_col]][i])  # 괄호 제거
      particle <- ifelse(has_batchim(term_no_parentheses), "은", "는")
      result <- paste0(result, df[[term_col]][i], particle, " ", df[[freq_col]][i], "명",
                       ifelse(i == nrow(df), "이다.", ", "))
    }
  }

  # df에서 term_combined 제거
  df <- df[, !(colnames(df) %in% "term_combined")]

  # df를 print로 출력
  if (print) {
    print(df)
  }

  # 결과 출력
  cat("\n")
  cat(result, "\n")
  cat("\n")
}

