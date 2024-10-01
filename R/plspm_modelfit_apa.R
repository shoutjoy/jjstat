#' pls_modelfit_apa
#'
#' @param model_fit_data modelfit
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' rest= plspm_modelfit(edt_pls, edblock, bootstrap = 1000, ci = 0.95, trans=FALSE)
#' rest
#' rest%>%t()%>%rowdata2col(1)%>%data.frame()
#'
#' pls_modelfit_apa(rest)
#'
#' }
pls_modelfit_apa <- function(model_fit_data) {

  # 모델 적합도 지표 추출
  chisq <- model_fit_data["Chisq", "modelfit_index"]
  df <- model_fit_data["df", "modelfit_index"]
  p_value <- model_fit_data["p_value", "modelfit_index"]
  srmr <- model_fit_data["SRMR", "modelfit_index"]
  nfi <- model_fit_data["NFI", "modelfit_index"]
  d_uls <- as.numeric(model_fit_data["d_ULS_EuclideanDist", "modelfit_index"])
  d_uls_ci <- model_fit_data["d_ULS_EuclideanDist", "evaluation"]
  d_g <- as.numeric(model_fit_data["d_G_GeodesicDist", "modelfit_index"])
  d_g_ci <- model_fit_data["d_G_GeodesicDist", "evaluation"]

  # 신뢰구간 문자열을 숫자로 변환하는 함수
  parse_ci <- function(ci_str) {
    ci_str <- gsub("[\\[\\]]", "", ci_str)    # 대괄호 제거
    ci_str <- gsub(" ", "", ci_str)           # 공백 제거
    ci_values <- as.numeric(strsplit(ci_str, ",")[[1]]) # 쉼표로 분리하여 숫자로 변환
    return(ci_values)
  }

  # d_ULS 및 d_G 신뢰구간 변환
  d_uls_ci_values <- parse_ci(d_uls_ci)
  d_g_ci_values <- parse_ci(d_g_ci)

  # 결과 해석 생성
  result <- ""

  # Chi-square 해석
  result <- paste0(result, "카이제곱 통계량(Chi-squared)은 ", chisq, "이고, 자유도(df)는 ", df, "이다. ",
                   "p-값은 ", p_value, "로, 모형이 데이터를 매우 잘 설명하고 있다. ",
                   "p-값이 0.05 이상이므로 모형 적합도는 양호하다.")

  # SRMR 해석
  result <- paste0(result, " SRMR(Standardized Root Mean Square Residual) 값은 ", srmr, "이다. ",
                   "SRMR 값이 0.08 이하일 때 모형 적합도가 양호한 것으로 간주된다. ",
                   "SRMR 값이 0.05 이하이면 매우 적합한 모형으로 평가된다. ",
                   "해당 모형의 SRMR 값은 ", srmr, "로 매우 적합한 것으로 평가된다.")

  # NFI 해석
  result <- paste0(result, " NFI(Normed Fit Index) 값은 ", nfi, "로 나타났다. ",
                   "NFI 값이 0.90 이상일 때 모형 적합도가 양호한 것으로 평가된다. ",
                   "이 모형의 NFI 값은 ", nfi, "로 매우 적합한 것으로 평가된다.")

  # d_ULS 해석 (신뢰구간 내 포함 여부 확인)
  if (!is.na(d_uls) && !is.na(d_uls_ci_values[1]) && d_uls >= d_uls_ci_values[1] && d_uls <= d_uls_ci_values[2]) {
    result <- paste0(result, " d_ULS(유클리드 거리 기반 적합도 평가) 값은 ", d_uls, "이고, ",
                     "신뢰구간은 ", d_uls_ci, "이다. d_ULS 값이 신뢰구간 내 포함되지 않으므로 ","모형 적합도가 낮을 수 있다.")

  } else {
    result <- paste0(result, " d_ULS(유클리드 거리 기반 적합도 평가) 값은 ", d_uls, "이고, ",
                     "신뢰구간은 ", d_uls_ci, "이다. d_ULS 값이 신뢰구간 내 포함되어 있으므로 ", "해당 모형의 d_ULS 값은 모형이 데이터를 잘 설명하고 있다.")
  }

  # d_G 해석 (신뢰구간 내 포함 여부 확인)
  if (!is.na(d_g) && !is.na(d_g_ci_values[1]) && d_g >= d_g_ci_values[1] && d_g <= d_g_ci_values[2]) {

    result <- paste0(result, " d_G(지오데식 거리 기반 적합도 평가) 값은 ", d_g, "이고, ",
                     "신뢰구간은 ", d_g_ci, "이다. d_G 값이 신뢰구간 내 포함되지 않으므로 ",
                     "모형의 구조적 적합도가 낮을 수 있다.\n")
  } else {
    result <- paste0(result, " d_G(지오데식 거리 기반 적합도 평가) 값은 ", d_g, "이고, ",
                     "신뢰구간은 ", d_g_ci, "이다. d_G 값이 신뢰구간 내 포함되어 있으므로 ",
                     "모형의 구조적 적합도가 양호하다.\n")
  }

  # 최종 해석 반환
  cat("\n")
  cat(result)
  cat("\n")
}
