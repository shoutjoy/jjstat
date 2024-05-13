
#' Create oserved/expected tapes after chisq calculation and give them individual significance
#'
#' @param observed contigency table
#' @param type data, Res(all), chisq_test, cramers v, observed_over_expected_ratios,cell_p_values, p_sig, data, data2
#' @param exact yYates's continuity correction
#' @param digits digits= 3
#'
#'
#' @return  matrix, data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # # calculate_chi_sig(data)
#' # Example data
#' matrix(c(36, 67, 11,
#'           0, 35, 44,
#'          41, 108, 1,
#'           56, 87, 54),
#'            nrow = 4, byrow = TRUE,
#'    dimnames = list(c("공명음", "마찰음",
#'      "유기음_경음", "평파열음_평파찰음"),  c("H", "H(H)", "L"))) %>%
#'      calculate_chi_sig("p_sig")
#'
#'
#'      matrix(c(36, 67, 11,
#'               40, 35, 44,
#'               41, 108, 1,
#'               56, 87, 54), nrow = 4, byrow = TRUE,
#'                      dimnames = list(c("공명음", "마찰음",
#'                                     "유기음_경음", "평파열음_평파찰음"),
#'                                  c("H", "H(H)", "L"))) %>%
#'                 calculate_chi_sig(simple = TRUE)
#'
#'
#'
#'   calculate_chi_sig(matrix(c(36, 67, 11,
#'                       40, 35, 44,
#'                    41, 108, 1,
#'               56, 87, 54), nrow = 4, byrow = TRUE,
#'                dimnames = list(c("공명음", "마찰음",
#'               "유기음_경음", "평파열음_평파찰음"),
#'               c("H", "H(H)", "L")))   )
#'
#'
#'  # calculate_chi_sig(data)
#' }
#'
calculate_chi_sig <- function(observed, type = "data", digits=3, exact = TRUE) {
  # 기대값 계산
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  grand_total <- sum(observed)
  expected <- outer(row_totals, col_totals, "*") / grand_total

  # 카이제곱 통계량 계산
  if (exact) {
    #Yates' continuity correction
    # cat("\n Yates' continuity correction\n")
    chi_square_statistic <- sum(((observed - expected) - 1/2)^2 / expected)
  } else {
    chi_square_statistic <- sum((observed - expected)^2 / expected)
  }

  # over all 자유도
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)
  # overall  p-값 계산
  p_value <- pchisq(chi_square_statistic, df, lower.tail = FALSE)

  # Calulate Cell Unite

  # 분산 계산
  sr <- rowSums(observed)
  sc <- colSums(observed)
  n <- sum(observed)
  v <- function(r, c, n) c * r * (n - r) * (n - c) / n^3
  V <- outer(sr, sc, v, n)

  # 표준편차 계산
  std_dev <- sqrt(V)

  # 표준화 잔차 계산
  standardized_residuals <- (observed - expected) / std_dev
  residuals <- (observed - expected) / sqrt(expected)
  chisq_res_sq <- residuals^2

  # 필요하다면 표준화 잔차를 반올림하거나 다른 형식으로 조정할 수 있습니다.
  standardized_residuals <- round(standardized_residuals, digits+2)

  #  cell_significant using stdres : 표준화 잔차를 이용한 셀별 유의성
  p_sig <- matrix("", nrow = nrow(standardized_residuals),
                  ncol = ncol(standardized_residuals))
  # # Mark values based on thresholds
  p_sig[abs(standardized_residuals) >= 1.96] <- "*"
  p_sig[abs(standardized_residuals) >= 2.58] <- "**"
  p_sig[abs(standardized_residuals) >= 3.09] <- "***"


  # observed_over_expected_ratios
  observed_over_expected_ratios <- observed / expected

  # 처리한 값 : 관측값과 관측기대비율 합치기
  chi_combine <- combine_data(observed,
                              observed_over_expected_ratios,
                              left = "(", right = ")")
  # 관측기대비율과 유의성 합치기
  chi_sig <- combine_data( round(observed_over_expected_ratios,digits), p_sig,"")
  #셀별 카이제곱과 유의성 합치기
  chisq_byCell_star <- combine_data(round(chisq_res_sq,digits), p_sig,"")
  # 관측값과 관측기대비율, 유의성 합치기
  chi_sig2 <- format(combine_data(chi_combine, p_sig), digits+2)


  # cramers' v
  cramersv <- cramers_v(observed)
  # chisq
  chisq <- chisq.test(observed) %>% suppressWarnings()

  # Res에 표준화 잔차 추가
  Res <- list(
    chisq_test = chisq,
    observed = observed %>% addmargins(),
    expected = expected %>% addmargins(),
    residuals = residuals,
    stdres = standardized_residuals,  # 표준화 잔차 추가
    chisq_cell = chisq_byCell_star,
    chisq_cell_margins = chisq_res_sq%>% addmargins(),
    cramersv = cramersv,
    observed_over_expected_ratios = observed_over_expected_ratios,
    OE_sig = chi_sig,
    Obs_OE_sig = chi_sig2

  )

  switch(type,
         res = Res,
         all = Res,
         chisq_test = chisq,
         expected = expected,
         cramersv = cramersv,
         observed_over_expected_ratios = observed_over_expected_ratios,
         V = V,  # 분산 추가
         std_dev = std_dev,  # 표준편차 추가
         df=df,
         p_value=p_value,
         p_sig = p_sig,
         chisq_cell = chisq_byCell_star,
         data = chi_sig,
         ratio_cell = chi_sig,
         data2 = chi_sig2

  )
}
#'
#'
#'
# calculate_chi_sig <- function(observed, type = "data", simple=FALSE) {
#   # 기대값 계산
#   row_totals <- rowSums(observed)
#   col_totals <- colSums(observed)
#   grand_total <- sum(observed)
#
#   expected <- outer(row_totals, col_totals, "*") / grand_total
#
#
#
#
#
#
#   # 카이제곱 통계량 계산
#   chi_square_statistic <- sum((observed - expected)^2 / expected)
#
#   # 자유도
#   df <- (nrow(observed) - 1) * (ncol(observed) - 1)
#
#   # p-값 계산
#   p_value <- pchisq(chi_square_statistic, df, lower.tail = FALSE)
#
#   # 각 셀에 대한 p-값 반환
#   cell_p_values <- matrix(0, nrow = nrow(observed), ncol = ncol(observed))
#   for (i in 1:nrow(observed)) {
#     for (j in 1:ncol(observed)) {
#       cell_observed <- observed[i, j]
#       if (cell_observed == 0) {
#         cell_p_values[i, j] <- 1  # If observed is 0, set p-value to 1
#       } else {
#
#         cell_expected <- expected[i, j]
#         cell_chi_square <- ((cell_observed - cell_expected)^2) / cell_expected
#         cell_p_values[i, j] <- pchisq(cell_chi_square, df = 1, lower.tail = FALSE)
#       }}
#
#   }
#
#
#   observed_over_expected_ratios = round(observed / expected, 3)
#
#
#
#   colnames(cell_p_values) = colnames(observed)
#   rownames(cell_p_values) = rownames(observed)
#
#
#   #significant
#   p_sig = add_significance_symbols(cell_p_values, simple= simple)
#   colnames(p_sig) = colnames(observed)
#   rownames(p_sig) = rownames(observed)
#
#
#
#   #처리한 값 :관측값과
#   chi_combine  =  combine_data(observed,
#                                observed_over_expected_ratios,
#                                left = "(", right=")")
#   # chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
#   #처리한 값
#   chi_sig =  combine_data(observed_over_expected_ratios, p_sig)
#   chi_sig2 =  format(combine_data(chi_combine, p_sig), 5)
#
#   chi_sig3 =
#     combine_data(
#     combine_data(observed_over_expected_ratios,
#                            round(cell_p_values,2), left = "(p = ", right = ")"),
#     p_sig, left = "")
#
#
#
#   # cramers' v
#   cramersv = cramers_v(observed)
#   # chisq
#   chisq = chisq.test(observed) %>%suppressWarnings()
#
#   Res = list(
#     # chi_square_statistic = chi_square_statistic,
#     #           degrees_of_freedom = df,
#     #           p_value = p_value,
#     chisq_test = chisq,
#     cramersv = cramersv,
#     observed = observed%>%addmargins(),
#     expected = expected%>%addmargins(),
#     observed_over_expected_ratios = observed_over_expected_ratios,
#     cell_p_values = cell_p_values,
#     p_sig = p_sig,
#     chi_sig=chi_sig,
#     chi_sig2=chi_sig2,
#     chi_sig3= chi_sig3
#   )
#
#
#
#
#   switch(type,
#          res= Res,
#          all= Res,
#          chisq_test= chisq,
#          expected = expected,
#          cramersv= cramersv,
#          observed_over_expected_ratios = observed_over_expected_ratios,
#          cell_p_values = cell_p_values,
#          p_sig = p_sig,
#          data = chi_sig,
#          data2 = chi_sig2,
#          data3 = chi_sig3
#   )
#
# }
#


#' #
#' #' Observation Expectation Table
#' #'
#' #' @param obs_data matrix
#' #'
#' #' @return matrix
#' #' @export
#' #'
#' obs_exp_table = function(obs_data){
#'   obs_data = as.matrix(obs_data)
#'   res = calculate_chi_sig(obs_data, type="observed_over_expected_ratios")
#'   res
#' }
#'


#' #' Observation Expectation Table
#' #'
#' #' @param obs_data matrix
#' #'
#' #' @return matrix
#' #' @export
#' #'
#' p_value_cal = function(obs_data){
#'   obs_data = as.matrix(obs_data)
#'   res = calculate_chi_sig(obs_data, type="cell_p_values")
#'   res
#' }
#'


#' #' T관측 기대 테이블
#' #'
#' #' @param obs_data matrix
#' #'
#' #' @return matrix
#' #' @export
#' #'
#' p_sig_cal = function(obs_data){
#'
#'   res = calculate_chi_sig(obs_data, type="p_sig")
#'   res
#' }





