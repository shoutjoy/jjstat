#' Multilevel CFA summary wrapper
#'
#' 다층 구조방정식(CFA/SEM) 모형의 within / between 수준 결과를
#' 한 번에 요약하여 제공하는 래퍼 함수이다.
#'
#' @param fit lavaan multilevel SEM object
#' @param level character. "within" 또는 "between"
#' @param type character. 출력 제어 옵션
#'   \itemize{
#'     \item{"all"}{모든 결과를 list로 반환}
#'     \item{"model_fit"}{모형 적합도}
#'     \item{"indicator_validity"}{요인부하량 및 지표 타당도}
#'     \item{"CR_AVE"}{잠재변수 수준 CR / AVE}
#'     \item{"item_CR_AVE"}{문항 수준 CR / AVE}
#'     \item{"fornell"}{Fornell–Larcker 판별타당도}
#'     \item{"htmt"}{HTMT 판별타당도}
#'   }
#' @param digits numeric. 소수점 자리수
#'
#' @return data.frame 또는 list
#' @export
#'
#' @examples
#' \dontrun{
#' res_all <- cfa_ml(fit_ml, level = "within", type = "all")
#' res_fit <- cfa_ml(fit_ml, level = "between", type = "model_fit")
#' }
cfa_ml <- function(fit,
                   level = c("within","between"),
                   type  = c("all",
                             "model_fit",
                             "indicator_validity",
                             "CR_AVE",
                             "item_CR_AVE",
                             "fornell",
                             "htmt"),
                   digits = 3) {

  ## -------------------------
  ## 0. Argument check
  ## -------------------------
  level <- match.arg(level)
  type  <- match.arg(type)

  ## -------------------------
  ## 1. Model fit indices
  ## -------------------------
  fit_long   <- mlsem_fit_indices(fit, type = "long", digits = digits)
  model_fit <- mlsem_modelfit_paper(fit_long, type = "data")

  ## -------------------------
  ## 2. Indicator validity
  ## -------------------------
  indicator_validity <- indicator_validity_ml(fit, level, digits)

  ## -------------------------
  ## 3. Construct reliability
  ## -------------------------
  CR_AVE <- mlsem_reliability(fit, level, digits)

  ## -------------------------
  ## 4. Item-level CR & AVE
  ## -------------------------
  item_CR_AVE_tbl <- item_CR_AVE(fit, level, digits)

  ## -------------------------
  ## 5. Fornell–Larcker
  ## -------------------------
  fornell <- mlsem_fornell_larcker(fit, level, digits)

  ## -------------------------
  ## 6. HTMT
  ## -------------------------
  htmt <- htmt_mlsem(fit, level, digits)

  ## -------------------------
  ## 7. Collect results
  ## -------------------------
  res <- list(
    level              = level,
    model_fit          = model_fit,
    indicator_validity = indicator_validity,
    CR_AVE             = CR_AVE,
    item_CR_AVE        = item_CR_AVE_tbl,
    fornell            = fornell,
    htmt               = htmt
  )

  ## -------------------------
  ## 8. Output control
  ## -------------------------
  switch(type,
         all                = res,
         model_fit          = model_fit,
         indicator_validity = indicator_validity,
         CR_AVE             = CR_AVE,
         item_CR_AVE        = item_CR_AVE_tbl,
         fornell            = fornell,
         htmt               = htmt
  )
}
