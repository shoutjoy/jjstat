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
