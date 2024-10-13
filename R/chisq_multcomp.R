#' Pairwise comparisons after a chi-squared goodness-of-fit test
#'
#' @param x numeric vector (counts).
#' @param p.method method for p-values correction. See help of p.adjust
#' @param ko TRUE variable names is changed to korean language
#' @describeIn Performs pairwise comparisons after a global chi-squared goodness-of-fit test.
#'
#' @return result
#' @export

#' @examples
#' \dontrun{
#' #'
#' counts <- c(49,30,63,59)
#' chisq.test(counts)
#' chisq_multcomp(counts)
#' # compare
#' chisq.multcomp(counts)
#'
#' }
chisq_multcomp2 <- function(x, p.method = "fdr", ko=TRUE) {
  library(dplyr)
  library(tidyr)
  library(stats)

  # 벡터의 이름을 추출
  data_names <- names(x)

  # 입력된 벡터를 문자형으로 변환하여 저장
  printx = as.character(x)

  # 입력된 벡터를 수치형으로 변환
  x = as.numeric(x)

  # p-value 계산을 위한 함수
  fun.p <- function(i, j) {
    xi <- x[i]
    xj <- x[j]
    suppressWarnings(chisq.test(c(xi, xj)))$p.value
  }

  # pairwise 테이블 생성 및 p-value 계산
  tab.p <- pairwise.table(fun.p, as.character(x), p.adjust.method = p.method)

  # 결과를 데이터프레임으로 변환 및 포맷팅
  plist <- as.data.frame(as.table(tab.p)) %>%
    dplyr::rename(cell_1 = Var1, cell_2 = Var2, p.value = Freq) %>%
    dplyr::mutate(
      cell_1 = gsub("X", "", cell_1),
      cell_2 = gsub("X", "", cell_2)
    ) %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*", "ns")))) %>%
    dplyr::select(cell_2, cell_1, p.value, sig)

  # cell_1과 cell_2를 숫자로 변환
  plist$cell_2 = as.numeric(plist$cell_2)
  plist$cell_1 = as.numeric(plist$cell_1)

  # pairwise 열 생성 (초기 값은 숫자로 결합)
  plist <- plist %>% tidyr::unite(pairwise, cell_2, cell_1, sep = " / ", remove = FALSE)


  # change_name 리스트 생성 (printx와 data_names를 묶음)
  change_name <- list(printx = printx, data_names = data_names)

  # gsub을 사용하여 pairwise의 숫자를 대응하는 이름으로 대체
  plist$pairwise <- lapply(plist$pairwise, function(pw) {
    for (i in seq_along(change_name$printx)) {
      pw <- gsub(change_name$printx[i], change_name$data_names[i], pw)
    }
    return(pw)
  })

  # pairwise를 문자형 벡터로 변환
  plist$pairwise <- unlist(plist$pairwise)

  # 결과 출력
  cat("\n")
  cat(paste0("p adjust method: ", p.method))
  cat("\n")

  # 데이터 이름 출력
  if (!is.null(data_names)) {
    cat(paste0("Your Data names: ", paste(data_names, collapse = ",")))
    cat("\n")
  } else {
    cat("Your Data names: NULL\n")
  }

  # 데이터 값 출력
  cat(paste0("Your Data counts: ", paste(printx, collapse = ",")))
  cat("\n")

  chisq = chisq.test(x) %>% broom::tidy()
  chisq_apa  =chisq.test(x) %>% chisq_apa()
  print(chisq)
  cat(chisq_apa)
  cat("\n")

  # 결과 반환
  plist = plist %>%mutate_col(diff= .$cell_2 - .$cell_1, col=4) %>%
    mutate(p.value= ifelse(p.value<0.001, " <.001", round(p.value, 3)))

  if(ko){
    plist= plist%>% rename(비교그룹=pairwise, 값2=cell_2, 값1 = cell_1, 빈도차이=diff)
  }


  return(plist %>% tibble::tibble())
}


#' RVAideMemoire::chisq.multcomp, Pairwise comparisons after a chi-squared goodness-of-fit test
#'
#' @param x numeric vector (counts).
#' @param p.method 	method for p-values correction. See help of p.adjust.
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' counts <- c(49,30,63,59)
#' chisq.test(counts)
#' chisq.multcomp(counts)
#' #comapre
#' chisq_multcomp(counts)
#'
#' }
chisq.multcomp = function (x, p.method = "fdr")
{
  x <- sort(x)
  fun.p <- function(i, j) {
    xi <- x[i]
    xj <- x[j]
    suppressWarnings(chisq.test(c(xi, xj)))$p.value
  }
  tab.p <- pairwise.table(fun.p, as.character(x), p.adjust.method = p.method)
  call <- match.call()
  dname.x <- if (length(call$x) == 1) {
    call$x
  }
  else {
    paste(call$x[1], "(", paste(call$x[-1], collapse = ","),
          ")", sep = "")
  }
  result <- list(method = "chi-squared tests", data.name = dname.x,
                 p.adjust.method = p.method, p.value = tab.p)
  class(result) <- "pairwise.htest"
  return(result)
}
