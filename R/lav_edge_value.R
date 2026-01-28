#' Extract edge labels for semPlot from lavaan / MLSEM object
#'
#' @description
#' lavaan(일반 SEM/CFA) 또는 다층 SEM(ML-SEM) 적합 객체에서
#' semPlot::semPaths()의 edgeLabels 인수에 바로 넣을 수 있는
#' "간선 라벨(edge label) 벡터"를 자동 생성하는 함수이다.
#'
#' 본 함수는 lavaan::parameterEstimates()를 기반으로
#' 측정경로(=~), 구조경로(~), 공분산(~~)을 모두 인식한다.
#' 다층모형의 경우 within/between 수준(block)을 선택할 수 있다.
#'
#' 핵심 설계 목표는 다음과 같다.
#' (1) 기준지표(fixed loading=1)에서 pvalue가 NA로 반환되는 경우에도
#'     라벨에 "NA"가 붙지 않도록 처리한다.
#' (2) semPlot이 요구하는 형식(unnamed character vector)에 맞게 안전 반환한다.
#' (3) 측정경로/공분산 값을 숨기거나, 유의성 별표를 제거하는 옵션을 제공한다.
#' (4) 구조경로(~)만 뽑아 구조모형 라벨에만 쓰는 옵션(path_only)을 제공한다.
#'
#' @param obj lavaan fitted object. (cfa/sem 또는 다층 sem 객체)
#'
#' @param level character. "within" 또는 "between".
#'   다층모형에서만 의미가 있으며, within=block 1, between=block 2로 처리한다.
#'   일반 lavaan 단일수준 모형에서는 block이 0 또는 1로 나올 수 있어,
#'   이 경우 level을 "within"으로 두는 것이 보통 안전하다.
#'
#' @param type character. "est"(비표준화 추정치) 또는 "std"(표준화 계수 std.all).
#'
#' @param sig logical. 유의성 별표(*, **, ***)를 붙일지 여부. 기본 TRUE.
#'   단, lavaan에서 pvalue가 NA인 경우(예: 기준지표 fixed loading=1)는
#'   별표를 붙이지 않는다(NA-safe).
#'
#' @param digits numeric. 반올림 소수점 자리수. 기본 3.
#'
#' @param include_cov logical. 공분산(~~)을 포함할지 여부. 기본 TRUE.
#'   FALSE이면 ~~는 제외된다.
#'
#' @param fillin logical. TRUE이면 측정경로(=~) 및 공분산(~~) 라벨을
#'   사용자가 지정한 문자로 "대체"한다. (예: 측정경로는 '지표'로 숨기고,
#'   구조경로(~)만 수치 라벨로 보이게)
#'
#' @param hide_mea character. fillin=TRUE일 때 =~ 경로 라벨을 이 문자로 대체.
#'   기본 ""(빈 문자열).
#'
#' @param hide_cov character. fillin=TRUE일 때 ~~ 경로 라벨을 이 문자로 대체.
#'   기본 ""(빈 문자열).
#'
#' @param hide_sig logical. TRUE이면 모든 유의성 별표를 제거한다.
#'   주의: hide_sig=TRUE이면 sig 인수는 사실상 무시된다(별표 제거가 우선).
#'
#' @param hide_sig_mea character. hide_sig=TRUE일 때,
#'   =~ 경로에 대해 특정 문자열로 대체하고 싶을 때 사용한다.
#'   기본 ""(대체 안 함).
#'
#' @param hide_sig_cov character. hide_sig=TRUE일 때,
#'   ~~ 경로에 대해 특정 문자열로 대체하고 싶을 때 사용한다.
#'   기본 ""(대체 안 함).
#'
#' @param path_only logical. TRUE이면 구조경로(~)만 추출한다.
#'   구조모형만 그리고 싶을 때 사용한다.
#'   이 경우 fillin 옵션은 적용되지 않는다(측정/공분산을 뽑지 않기 때문).
#'
#' @return unnamed character vector.
#'   semPlot::semPaths(edgeLabels= )에 그대로 투입 가능한 형식이다.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # ============================================================
#' # [0] 전제: fit_mlsem_expB, model_within_plot, diagram_model()이 존재한다고 가정
#' # ============================================================
#'
#' # ------------------------------------------------------------
#' # (1) 기본 사용: within 수준, 표준화 계수(std) + 유의성 별표
#' # ------------------------------------------------------------
#' edge_std <- lav_edge_value(
#'   obj   = fit_mlsem_expB,
#'   level = "within",
#'   type  = "std"
#' )
#' edge_std
#'
#' # diagram_model에 그대로 주입
#' diagram_model(
#'   model      = model_within_plot,
#'   layout     = layout_scct1,
#'   whatLabels = "none",
#'   sizeMan    = 8,
#'   sizeLat    = 10,
#'   edge.label.position = 0.7,
#'   edge.label.cex      = 0.9,
#'   edgeLabels = edge_std,
#'   color = list(lat="black", man="White")
#' )
#'
#' # ------------------------------------------------------------
#' # (2) 비표준화 계수(est) 라벨
#' # ------------------------------------------------------------
#' edge_est <- lav_edge_value(
#'   obj   = fit_mlsem_expB,
#'   level = "within",
#'   type  = "est"
#' )
#' edge_est
#'
#' diagram_model(
#'   model      = model_within_plot,
#'   layout     = layout_scct1,
#'   whatLabels = "none",
#'   edge.label.position = 0.7,
#'   edge.label.cex      = 0.9,
#'   edgeLabels = edge_est
#' )
#'
#' # ------------------------------------------------------------
#' # (3) 측정경로(=~)를 숨기고 구조경로(~)만 숫자로 보여주기
#' #     - fillin=TRUE + hide_mea="지표"
#' #     - 공분산(~~)은 빈문자("")로 숨김
#' # ------------------------------------------------------------
#' edge_std_path <- lav_edge_value(
#'   obj      = fit_mlsem_expB,
#'   level    = "within",
#'   type     = "std",
#'   fillin   = TRUE,
#'   hide_mea = "지표",
#'   hide_cov = ""
#' )
#'
#' diagram_model(
#'   model      = model_within_plot,
#'   layout     = layout_scct1,
#'   whatLabels = "none",
#'   edge.label.position = 0.6,
#'   edge.label.cex      = 0.9,
#'   edgeLabels = edge_std_path
#' )
#'
#' # ------------------------------------------------------------
#' # (4) 유의성 별표(*,**,***)를 모두 숨기고 숫자만 표시
#' #     - hide_sig=TRUE가 최우선
#' # ------------------------------------------------------------
#' edge_std_nostar <- lav_edge_value(
#'   obj      = fit_mlsem_expB,
#'   level    = "within",
#'   type     = "std",
#'   hide_sig = TRUE
#' )
#'
#' diagram_model(
#'   model      = model_within_plot,
#'   layout     = layout_scct1,
#'   whatLabels = "none",
#'   edgeLabels = edge_std_nostar
#' )
#'
#' # ------------------------------------------------------------
#' # (5) 구조경로(~)만 추출: 구조모형 라벨만 뽑기
#' #     - 측정경로/공분산 라벨이 전혀 포함되지 않는다.
#' # ------------------------------------------------------------
#' edge_struct_only <- lav_edge_value(
#'   obj       = fit_mlsem_expB,
#'   level     = "within",
#'   type      = "std",
#'   path_only = TRUE
#' )
#' edge_struct_only
#'
#' # (주의) diagram_model()에서 structural=TRUE를 쓰면
#' # semPlot이 일부 변수를 자동 제외할 수 있어 레이아웃과 충돌 가능성이 있다.
#' # 이 경우에는 whatLabels="none" + fillin을 쓰는 방식이 더 안정적일 수 있다.
#'
#' # ------------------------------------------------------------
#' # (6) between 수준 라벨 추출 (ML-SEM)
#' # ------------------------------------------------------------
#' edge_between <- lav_edge_value(
#'   obj   = fit_mlsem_expB,
#'   level = "between",
#'   type  = "std",
#'   fillin   = TRUE,
#'   hide_mea = "지표",
#'   hide_cov = ""
#' )
#' edge_between
#' }
lav_edge_value <- function(obj,
                           level = c("within","between"),
                           type  = c("est","std"),
                           sig   = TRUE,
                           digits = 3,
                           include_cov = TRUE,
                           fillin = FALSE,
                           hide_mea = "",
                           hide_cov = "",
                           hide_sig = FALSE,
                           hide_sig_mea = "",
                           hide_sig_cov = "",
                           path_only = FALSE) {

  level <- match.arg(level)
  type  <- match.arg(type)
  lv_idx <- ifelse(level == "within", 1, 2)

  pe <- lavaan::parameterEstimates(obj, standardized = TRUE)

  ## -------------------------
  ## 1. level 필터
  ## -------------------------
  pe <- pe |> dplyr::filter(block == lv_idx)

  ## -------------------------
  ## 2. 경로 선택
  ## -------------------------
  if (path_only) {

    pe <- pe |> dplyr::filter(op == "~")

  } else {

    pe <- pe |>
      dplyr::filter(op %in% c("=~","~","~~")) |>
      dplyr::filter(!(op == "~~" & lhs == rhs))

    if (!include_cov) {
      pe <- pe |> dplyr::filter(op != "~~")
    }
  }

  ## -------------------------
  ## 3. 값 선택
  ## -------------------------
  val <- if (type == "std") pe$std.all else pe$est
  val <- round(val, digits)

  ## 값 자체가 NA인 경우 제거
  keep <- !is.na(val)
  pe  <- pe[keep, ]
  val <- val[keep]

  ## -------------------------
  ## 4. 유의성 처리 (NA-safe)
  ## -------------------------
  if (sig && !hide_sig) {

    stars <- rep("", length(val))

    ok <- !is.na(pe$pvalue)
    stars[ok & pe$pvalue < .001] <- "***"
    stars[ok & pe$pvalue < .01]  <- "**"
    stars[ok & pe$pvalue < .05]  <- "*"

    lab <- paste0(val, stars)

  } else {
    lab <- as.character(val)
  }

  ## -------------------------
  ## 5. fill-in 처리
  ## -------------------------
  if (fillin && !path_only) {
    lab[pe$op == "=~"] <- hide_mea
    lab[pe$op == "~~"] <- hide_cov
  }

  ## -------------------------
  ## 6. hide_sig 세부 옵션
  ## -------------------------
  if (hide_sig) {

    lab <- gsub("\\*+", "", lab)

    if (!path_only) {
      if (hide_sig_mea != "") {
        lab[pe$op == "=~"] <- hide_sig_mea
      }
      if (hide_sig_cov != "") {
        lab[pe$op == "~~"] <- hide_sig_cov
      }
    }
  }

  ## -------------------------
  ## 7. semPlot 안전 반환
  ## -------------------------
  lab <- as.character(lab)
  lab <- unname(lab)

  lab
}
