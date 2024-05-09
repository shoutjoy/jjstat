
#' Chi-square analysis by cell
#'
#' @param data table
#' @param type all, res, cramer, ratio
#'
#' @return chisq
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#'
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"),
#'                     party = c("Democrat","Independent", "Republican"))
#' M %>% chisq_test_cell()
#'
#' #notsig
#' examtest  = matrix(c(7,14,13,6), nrow=2,
#'                    dimnames= list(c("사전검사","사후검사"),
#'                                   c("합격","불합격")))
#' examtest
#' fisher.test(examtest)
#'
#' examtest %>% chisq_test_cell()
#' }
#'
#'
chisq_test_cell <- function(data, type="all") {
  # 카이제곱 검정 수행
  chi_test <- chisq.test(data)


  chi_test$observed <- as.matrix(chi_test$observed)
  chi_test$expected <- as.matrix(chi_test$expected)
  cor_ratio = chi_test$observed / chi_test$expected
  cramer = cramers_v(data)
  # residuals 제곱값 계산
  residuals_squared <- chi_test$residuals^2

  # 자유도(df)
  # df <- chi_test$parameter  #이건 적용하면 안됨
  df = 1

  # 각 셀별 p값 계산
  p_values <- pchisq(residuals_squared, df, lower.tail = FALSE)

  # 행렬로 출력
  p_matrix <- matrix(p_values, nrow = nrow(data), ncol = ncol(data))
  colnames(p_matrix) <- colnames(data)
  rownames(p_matrix) <- rownames(data)

  p_sig = jjstat::add_significance_symbols(p_matrix)

  res = jjstat::combine_data( round(residuals_squared, 2),   p_sig )
  ratio = jjstat::combine_data( round(cor_ratio, 2),   p_sig )

  all = list(
    # chisq=chi_test,
    cell_chisq = res,
    cramer=cramer,
    ratio = ratio
  )
  print(chi_test)

  switch(type,
         all = all, res= res,cramer=cramer,
         ratio=ratio)

}
