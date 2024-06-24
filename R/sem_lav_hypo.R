#' Hypothesis generation functions
#'
#' @param model lavaan syntax
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예제 모델
#' model1a <- "
#' EXPE ~ H1*IMAG
#' QUAL ~ H2*EXPE
#' VAL ~  H3*QUAL + H4*EXPE
#' SAT ~ H5*IMAG + H6*EXPE + H7*QUAL + H8*VAL
#' LOY ~ H9*SAT + H10*IMAG
#' "
#'
#' # 함수 실행
#' sem_lav_hypo(model1a)
#' #'
#'
#' # Example usage with the provided model
#' model10 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#' lav_extract_sm(model10)%>%sem_lav_hypo()
#' lav_extract_sm(model10, prefix="H")%>%sem_lav_hypo()
#'
#'
#'
#' #'
#' }
#'
sem_lav_hypo <- function(model) {
  lines <- strsplit(model, "\n")[[1]]
  hypotheses <- c()
  count <- 1

  for (line in lines) {
    if (grepl("~", line)) {
      parts <- strsplit(line, "~")[[1]]
      dependent_var <- trimws(parts[1])
      independent_vars <- unlist(strsplit(parts[2], "\\+"))

      for (var in independent_vars) {
        var <- trimws(var)
        if (grepl("\\*", var)) {
          parts_var <- strsplit(var, "\\*")[[1]]
          hyp_number <- trimws(parts_var[1])
          independent_var <- trimws(parts_var[2])
        } else {
          hyp_number <- paste0("H", count)
          independent_var <- var
          count <- count + 1
        }
        hypothesis <- paste0("가설[", hyp_number, "] : ", independent_var, "는 ", dependent_var, "에 통계적으로 유의미한 효과를 미칠 것이다.")
        hypotheses <- c(hypotheses, hypothesis)
      }
    }
  }

  cat("\n", paste(hypotheses, collapse = "\n"),"\n\n")

}




#
#' Function that takes path data as input and outputs an indirect effect hypothesis.
#'
#' @param paths indpath
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 모델 문자열
#' model1a <- "
#' EXPE ~ H1*IMAG
#' QUAL ~ H2*EXPE
#' VAL ~ H3*QUAL + H4*EXPE
#' SAT ~ H5*IMAG + H6*EXPE + H7*QUAL + H8*VAL
#' LOY ~ H9*SAT + H10*IMAG
#' "
#'
#' model1 <- "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~ QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#' lav_extract_path(model1a)%>%
#'   lav_generate_ind_paths() %>%
#'   lav_ind_hypo()
#'
#' }
#'
sem_lav_ind_hypo <- function(paths) {

  cat("\n",
      "구조모형에서 나타나는 간접효과 가설은 다음과 같다. \n")
  # 각 경로에 대해 반복합니다.
  for (i in seq_along(paths)) {
    # 경로를 나누어 각 변수로 분리합니다.
    path_elements <- unlist(strsplit(paths[i], " -> "))
    # IMAG와 LOY를 제외한 중간 변수를 추출합니다.
    intermediates <- path_elements[2:(length(path_elements) - 1)]
    # 간접효과 가설 문구를 생성합니다.
    hypothesis <- paste("  간접효과[H", i, "]: 간접효과 IMAG -> ", paste(intermediates, collapse = " -> "), " -> LOY 는 통계적으로 유의할 것이다.", sep = "")
    # 가설을 출력합니다.
    cat(hypothesis, "\n")
  }
  cat("\n")
}
