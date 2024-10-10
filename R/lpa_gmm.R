#' 잠재프로파일 분석 변수별 군집분포와 결과데이터
#'
#' @param df df
#' @param model G, model N = 4
#' @param modelNames model ="EEE", EII
#'
#' @return df, graph
#' @export
#'
#' @examples
#' \dontrun{
#' lpa_gmm(edlcarename, model=4, "EII")
#' }
lpa_gmm = function(df, model=4, modelNames="EII"){
  set.seed(123)

  gmm_model <- Mclust(df, G=model, modelNames =modelNames )
  # summary(gmm_model, parameters = TRUE)
  lpa = gmm_model$parameters

  g = plot(gmm_model, what = "uncertainty")

  res= list(result = lpa, graph = g)
  return(res)
}
