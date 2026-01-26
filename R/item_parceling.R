#' Item Parceling Utility Function
#'
#' This function creates item parcels from observed item-level data using
#' aggregation methods (mean, sum, weighted mean) or
#' factor-score-based methods (EFA / CFA).
#'
#' @param data A data.frame containing observed item variables.
#' @param ... Parcel definitions.
#' @param fun Scoring method. Options include "mean", "sum", "median", "wmean",
#'   "factor_efa", "fs_efa", "factor_cfa", "fs_cfa".
#'   Abbreviated forms "efa" and "cfa" are allowed with a warning.
#' @param type Output type:
#'   "new" (parcel data only),
#'   "data" (append parcels to original data),
#'   "all" (data + metadata).
#' @param weights Numeric vector or a named list of numeric vectors.
#'   For fun = "wmean", parcel-specific weights can be supplied as a list.
#'   Parcels without weights will be aggregated using mean().
#' @param standardize_items Logical. If TRUE, items are standardized before scoring.
#' @param min_valid Minimum number of valid items required per row.
#' @param factor_score Factor score method for EFA ("regression", "bartlett").
#' @param lavaan_estimator Estimator for CFA.
#' @param std.lv Logical. If TRUE, latent variance is fixed to 1 in CFA.
#' @param name_in_vector Logical or "auto".
#'   - TRUE: c("ParcelName","item1","item2",...)
#'   - FALSE: ParcelName = c("item1","item2",...) (default)
#'   - "auto": if the first element is not a column name, treat it as parcel name.
#' @param verbose Logical. If TRUE, prints reliability summaries.
#'
#' @return Depending on type, a data.frame or a list.
#' @export
#'
#' @examples
#' ## -------------------------------------------------
#' ## Example data
#' ## -------------------------------------------------
#' set.seed(123)
#' shindata_example <- data.frame(
#'   efficacy2 = rnorm(100, 3.5, 0.8),
#'   efficacy3 = rnorm(100, 3.6, 0.7),
#'   efficacy4 = rnorm(100, 3.7, 0.6),
#'
#'   Accessibility1 = rnorm(100, 4.0, 0.9),
#'   Accessibility2 = rnorm(100, 3.9, 0.8),
#'   Accessibility3 = rnorm(100, 4.1, 0.7),
#'   Accessibility4 = rnorm(100, 4.0, 0.6),
#'   Accessibility5 = rnorm(100, 3.8, 0.9),
#'
#'   Interest1 = rnorm(100, 3.2, 0.7),
#'   Interest2 = rnorm(100, 3.3, 0.8),
#'   Interest3 = rnorm(100, 3.4, 0.6),
#'   Interest4 = rnorm(100, 3.5, 0.7)
#' )
#'
#' ## -------------------------------------------------
#' ## Basic example: sum
#' ## -------------------------------------------------
#' parcel_sum <- item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   연구접근성 = c("Accessibility1","Accessibility2",
#'                   "Accessibility3","Accessibility4","Accessibility5"),
#'   fun = "sum",
#'   type = "new"
#' )
#' head(parcel_sum)
#'
#' ## -------------------------------------------------
#' ## Weighted mean: single parcel
#' ## -------------------------------------------------
#' item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   fun = "wmean",
#'   weights = c(0.40, 0.35, 0.25),
#'   type = "new"
#' )
#'
#' ## -------------------------------------------------
#' ## Weighted mean: parcel-specific weights (recommended)
#' ## -------------------------------------------------
#' parcel_wmean <- item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   연구접근성 = c("Accessibility1","Accessibility2",
#'                   "Accessibility3","Accessibility4","Accessibility5"),
#'   fun = "wmean",
#'   weights = list(
#'     효능감 = c(0.40, 0.35, 0.25)
#'   ),
#'   type = "new"
#' )
#' head(parcel_wmean)
#'
#' \dontrun{
#' ## -------------------------------------------------
#' ## Error example: invalid weights usage
#' ## -------------------------------------------------
#' item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   연구접근성 = c("Accessibility1","Accessibility2",
#'                   "Accessibility3","Accessibility4","Accessibility5"),
#'   fun = "wmean",
#'   weights = c(0.40, 0.35, 0.25)
#' )
#' }
#'
#' ## -------------------------------------------------
#' ## Factor score (EFA)
#' ## -------------------------------------------------
#' parcel_fs_efa <- item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   연구접근성 = c("Accessibility1","Accessibility2",
#'                   "Accessibility3","Accessibility4","Accessibility5"),
#'   fun = "factor_efa",
#'   factor_score = "regression",
#'   type = "all"
#' )
#' parcel_fs_efa$score_info$효능감
#'
#' ## -------------------------------------------------
#' ## EFA alias (warning example)
#' ## -------------------------------------------------
#' item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   fun = "efa",
#'   type = "new"
#' )
#'
#' ## -------------------------------------------------
#' ## CFA factor score
#' ## -------------------------------------------------
#' parcel_fs_cfa <- item_parceling(
#'   data = shindata_example,
#'   효능감 = c("efficacy2","efficacy3","efficacy4"),
#'   fun = "factor_cfa",
#'   type = "all"
#' )
#' parcel_fs_cfa$score_info$효능감$CR
#'
#' ## -------------------------------------------------
#' ## name_in_vector usage
#' ## -------------------------------------------------
#' item_parceling(
#'   data = shindata_example,
#'   c("효능감","efficacy2","efficacy3","efficacy4"),
#'   c("진로흥미","Interest1","Interest2","Interest3","Interest4"),
#'   fun = "factor_efa",
#'   name_in_vector = TRUE,
#'   type = "new"
#' )
#'
#' item_parceling(
#'   data = shindata_example,
#'   c("efficacy2","efficacy3","efficacy4"),
#'   진로흥미 = c("Interest1","Interest2","Interest3","Interest4"),
#'   fun = "factor_efa",
#'   name_in_vector = "auto",
#'   type = "new"
#' )
item_parceling <- function(
    data,
    ...,
    fun = "mean",
    type = c("new", "data", "all"),
    weights = NULL,
    standardize_items = FALSE,
    min_valid = 1,
    factor_score = c("regression", "bartlett"),
    lavaan_estimator = "ML",
    std.lv = FALSE,
    name_in_vector = FALSE,
    verbose = FALSE
) {

  # =================================================
  # [BLOCK 1] 입력 인자 정규화
  # - type, factor_score는 허용된 옵션 중 하나로 강제
  # =================================================
  type <- match.arg(type)
  factor_score <- match.arg(factor_score)

  # =================================================
  # [BLOCK 2] name_in_vector = auto (따옴표 없는 auto) 방지
  # - R은 name_in_vector = auto 를 객체로 평가하려다 즉시 에러 발생
  # - 이를 친절한 안내 메시지로 교체하여 사용자 UX 개선
  # =================================================
  niv_expr <- substitute(name_in_vector)
  if (is.symbol(niv_expr) && deparse(niv_expr) == "auto") {
    stop(
      "name_in_vector = auto 는 객체로 해석됩니다.\n",
      "문자열로 입력해 주세요:\n",
      "  name_in_vector = \"auto\"",
      call. = FALSE
    )
  }

  # =================================================
  # [BLOCK 3] fun alias 정규화
  # - efa/cfa 같은 약어 입력을 허용하되, 경고 후 표준 키로 정규화
  # - fs_efa/fs_cfa도 내부적으로 factor_efa/factor_cfa로 통일
  # =================================================
  if (is.character(fun)) {
    fun_raw <- tolower(fun)
    fun <- switch(
      fun_raw,
      "efa" = {
        warning("Use \"factor_efa\" or \"fs_efa\" instead of \"efa\".", call. = FALSE)
        "factor_efa"
      },
      "fs_efa" = "factor_efa",
      "factor_efa" = "factor_efa",
      "cfa" = {
        warning("Use \"factor_cfa\" or \"fs_cfa\" instead of \"cfa\".", call. = FALSE)
        "factor_cfa"
      },
      "fs_cfa" = "factor_cfa",
      "factor_cfa" = "factor_cfa",
      fun_raw
    )
  }

  # =================================================
  # [BLOCK 4] 사용자가 정의한 parcel 정보 수집
  # - ... 안에 들어온 parcel 정의들을 리스트로 받음
  # - term_names는 ParcelName = c(items) 형태에서 이름을 추출
  # =================================================
  terms <- list(...)
  term_names <- names(terms)

  # =================================================
  # [BLOCK 5] 결과 저장용 객체 초기화
  # - itemparceling_data: 새로 만든 parcel만 모은 데이터프레임
  # - cols_check: 각 parcel에 어떤 item이 들어갔는지 기록
  # - score_info: 계산 방식/가중치/적재치 등 메타정보 기록
  # =================================================
  itemparceling_data <- data.frame(row.names = rownames(data))
  cols_check <- list()
  score_info <- list()

  # =================================================
  # [BLOCK 6] parcel 점수 계산 함수
  # - mat: 특정 parcel에 해당하는 item 행렬
  # - pname: parcel 이름(가중치 list 매칭, 메타정보 기록에 사용)
  # - 처리 단계:
  #   (1) 표준화 옵션
  #   (2) 유효 응답 개수(min_valid) 조건 처리
  #   (3) fun에 따라 점수 계산
  # =================================================
  .score_from_fun <- function(mat, pname) {

    # (1) 표준화: 각 item을 z-score로 변환 (결측은 유지)
    if (standardize_items) mat <- scale(mat)

    # (2) 유효값 개수 기준으로 행 단위 점수 계산 여부 결정
    ok <- rowSums(!is.na(mat)) >= min_valid
    out <- rep(NA_real_, nrow(mat))
    mat_ok <- mat[ok, , drop = FALSE]

    # -------------------------------------------------
    # (3-A) weighted mean (wmean): parcel별 가중치 지원
    # -------------------------------------------------
    if (fun == "wmean") {

      # 1) parcel별 weights(list)가 있으면 우선 적용
      # 2) 없으면 기존 호환(숫자 벡터 weights) 적용
      # 3) 둘 다 없으면 mean으로 자동 fallback
      w_use <- NULL

      if (is.list(weights) && !is.null(weights[[pname]])) {
        w_use <- weights[[pname]]
      }

      if (is.null(w_use) && is.numeric(weights)) {
        w_use <- weights
      }

      # weights가 전혀 없으면 mean으로 처리
      if (is.null(w_use)) {
        out[ok] <- rowMeans(mat_ok, na.rm = TRUE)
        return(list(score = out, info = list(method = "mean (fallback)")))
      }

      # weights 길이 불일치 시: 사용자 안내 메시지 제공
      if (length(w_use) != ncol(mat_ok)) {

        # 다중 parcel 상황에서 numeric weights를 넣은 흔한 실수를 친절히 안내
        if (is.numeric(weights) && !is.list(weights)) {
          stop(
            "Invalid use of weights with fun = \"wmean\".\n\n",
            "Multiple parcels with different item counts detected.\n",
            "Use a named list for weights, e.g.:\n\n",
            "  weights = list(\n",
            "    효능감 = c(0.40, 0.35, 0.25)\n",
            "  )\n\n",
            "Parcels without weights will use mean().",
            call. = FALSE
          )
        }

        # list weights를 썼지만 특정 parcel 가중치 길이가 틀린 경우
        stop(
          "Length of weights does not match number of items in parcel: ",
          pname,
          call. = FALSE
        )
      }

      # 가중치 정규화 (합이 1이 되도록)
      w <- w_use / sum(w_use)

      # 행 단위 가중평균(결측은 해당 문항 가중치 재정규화 후 적용)
      out[ok] <- apply(mat_ok, 1, function(x) {
        idx <- !is.na(x)
        sum(x[idx] * (w[idx] / sum(w[idx])))
      })

      return(list(score = out, info = list(method = "wmean", weights = w)))
    }

    # -------------------------------------------------
    # (3-B) 기본 집계: mean/sum/median 등
    # - 현재 버전은 mean만 기본 구현(필요시 sum/median 확장 가능)
    # -------------------------------------------------
    if (fun == "mean") {
      out[ok] <- rowMeans(mat_ok, na.rm = TRUE)
      return(list(score = out, info = list(method = "mean")))
    }

    if (fun == "sum") {
      out[ok] <- rowSums(mat_ok, na.rm = TRUE)
      return(list(score = out, info = list(method = "sum")))
    }

    if (fun == "median") {
      out[ok] <- apply(mat_ok, 1, median, na.rm = TRUE)
      return(list(score = out, info = list(method = "median")))
    }

    # -------------------------------------------------
    # (3-C) EFA factor score (psych::fa)
    # - nfactors=1 고정
    # - complete cases만으로 요인점수 계산(결측은 NA 유지)
    # -------------------------------------------------
    if (fun == "factor_efa") {

      if (!requireNamespace("psych", quietly = TRUE)) {
        stop("psych package is required for fun = \"factor_efa\".", call. = FALSE)
      }

      cc <- complete.cases(mat_ok)

      fa_fit <- psych::fa(
        mat_ok[cc, , drop = FALSE],
        nfactors = 1,
        fm = "ml",
        scores = factor_score
      )

      sc <- rep(NA_real_, nrow(mat_ok))
      sc[cc] <- as.numeric(fa_fit$scores[, 1])
      out[ok] <- sc

      if (verbose) {
        cat("\n[EFA Parcel Summary:", pname, "]\n")
        cat("Loadings:\n")
        print(round(as.numeric(fa_fit$loadings[, 1]), 3))
      }

      return(list(
        score = out,
        info = list(
          method   = "factor_efa",
          loadings = as.numeric(fa_fit$loadings[, 1]),
          factor_score = factor_score
        )
      ))
    }

    # -------------------------------------------------
    # (3-D) CFA factor score (lavaan::cfa + lavPredict)
    # - 단일 요인 CFA를 자동 구성: F =~ item1 + item2 + ...
    # - std.lv 옵션 지원
    # - 표준화 적재치 기반 CR/AVE 요약 산출
    # -------------------------------------------------
    if (fun == "factor_cfa") {

      if (!requireNamespace("lavaan", quietly = TRUE)) {
        stop("lavaan package is required for fun = \"factor_cfa\".", call. = FALSE)
      }

      items <- colnames(mat_ok)
      model <- paste0("F =~ ", paste(items, collapse = " + "))

      fit <- lavaan::cfa(
        model,
        data      = as.data.frame(mat_ok),
        estimator = lavaan_estimator,
        missing   = "fiml",
        std.lv    = std.lv
      )

      fs <- lavaan::lavPredict(fit)
      out[ok] <- as.numeric(fs[, 1])

      pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
      lam <- pe$std.all[pe$op == "=~"]

      lam2 <- lam^2
      CR  <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam2))
      AVE <- sum(lam2) / length(lam2)

      if (verbose) {
        cat("\n[CFA Parcel Summary:", pname, "]\n")
        cat("std.lv:", std.lv, "\n")
        cat("Loadings:\n")
        print(round(lam, 3))
        cat("CR:", round(CR, 3), "AVE:", round(AVE, 3), "\n")
      }

      return(list(
        score = out,
        info = list(
          method   = "factor_cfa",
          std.lv   = std.lv,
          loadings = lam,
          CR       = CR,
          AVE      = AVE,
          estimator = lavaan_estimator
        )
      ))
    }

    # -------------------------------------------------
    # (3-E) 지원하지 않는 fun 입력
    # -------------------------------------------------
    stop("Unsupported fun option: ", fun, call. = FALSE)
  }

  # =================================================
  # [BLOCK 7] Parcel 정의를 순회하면서 점수 생성
  # - name_in_vector / auto / named-argument 방식을 모두 처리
  # - 각 parcel에 대해:
  #   (1) parcel 이름(pname)과 item 목록(cols)을 결정
  #   (2) data에 cols 존재 여부 검사
  #   (3) 점수 계산 및 결과 저장
  # =================================================
  for (i in seq_along(terms)) {

    term <- terms[[i]]

    # (1) ParcelName = c(items) 형태 (추천)
    if (!is.null(term_names) && term_names[i] != "") {
      pname <- term_names[i]
      cols  <- term

      # (2) c("ParcelName", items) 형태 (명시적으로 TRUE)
    } else if (isTRUE(name_in_vector)) {
      pname <- term[1]
      cols  <- term[-1]

      # (3) auto: 첫 요소가 data 변수명이 아니면 이름으로 간주
    } else if (identical(name_in_vector, "auto") && !(term[1] %in% colnames(data))) {
      pname <- term[1]
      cols  <- term[-1]

      # (4) 그 외: 자동 이름(parcel_i) 부여, term 전체를 cols로 사용
    } else {
      pname <- paste0("parcel_", i)
      cols  <- term
    }

    # (2) 컬럼 존재성 체크: 누락 변수는 즉시 중단
    miss <- setdiff(cols, colnames(data))
    if (length(miss) > 0) {
      stop(
        "Can't subset columns that don't exist: ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
    }

    # (3) 점수 계산
    scored <- .score_from_fun(as.matrix(data[cols]), pname)

    # (4) 결과 저장
    itemparceling_data[[pname]] <- scored$score
    data[[pname]] <- scored$score
    cols_check[[pname]] <- cols
    score_info[[pname]] <- scored$info
  }

  # =================================================
  # [BLOCK 8] 출력 타입에 따른 반환
  # - new : parcel만 반환
  # - data: 원본 data + parcel 추가 후 반환
  # - all : data + parcel + 메타정보(list) 반환
  # =================================================
  if (type == "new") return(itemparceling_data)
  if (type == "data") return(data)

  list(
    data = data,
    itemparceling_data = itemparceling_data,
    cols_check = cols_check,
    score_info = score_info
  )
}



#' #' item_parceling: Mutate-like Item Parceling Function
#' #'
#' #' This function creates composite (parcel) variables in a data.frame
#' #' similar to `dplyr::mutate()`. The left-hand side specifies the name
#' #' of the new parcel variable, and the right-hand side specifies the
#' #' columns to be aggregated.
#' #'
#' #' @param data A data.frame.
#' #' @param ... Named arguments. The name is the new parcel variable,
#' #'   and the value specifies columns using names, indices, or ranges.
#' #' @param fun A function to apply across selected columns (default = mean).
#' #' @param type Output type:
#' #'   \describe{
#' #'     \item{data}{Return data.frame with parcel variables (default).}
#' #'     \item{res}{Same as data.}
#' #'     \item{new}{Return only newly created parcel variables.}
#' #'     \item{cols_check}{Return columns used for each parcel.}
#' #'     \item{all}{Return list(data, cols_check).}
#' #'   }
#' #'
#' #' @return A data.frame or list depending on `type`.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #'
#' #' ############################################################
#' #' ## Part 1. mtcars examples
#' #' ############################################################
#' #'
#' #' ## 1. Create parcels and return full data
#' #' item_parceling(
#' #'   mtcars,
#' #'   Mean_col1 = c("mpg", "cyl"),
#' #'   Mean_col2 = c("disp", "hp")
#' #' )
#' #'
#' #'
#' #' ## 2. Return only newly created parcel variables
#' #' item_parceling(
#' #'   mtcars,
#' #'   Mean_col1 = c("mpg", "cyl"),
#' #'   Mean_col2 = c("disp", "hp"),
#' #'   type = "new"
#' #' )
#' #'
#' #'
#' #' ############################################################
#' #' ## Part 2. stat_onl examples (survey data)
#' #' ############################################################
#' #'
#' #' ## 3. Satisfaction-related parcels
#' #' stat_onl <- item_parceling(
#' #'   stat_onl,
#' #'   Satisfy_Mean = c("satisfy", "On_Satisfy", "On_Joy"),
#' #'   Learning_Engage = c("learning:S_Focus_on"),
#' #'   fun  = mean,
#' #'   type = "data"
#' #' )
#' #'
#' #'
#' #' ## 4. Extract only parcel scores for SEM / CFA
#' #' parcel_scores <- item_parceling(
#' #'   stat_onl,
#' #'   Satisfy_Mean = c("satisfy", "On_Satisfy", "On_Joy"),
#' #'   Learning_Engage = c("learning:S_Focus_on"),
#' #'   Online_Exp = c("On_Easy:Intension_use"),
#' #'   type = "new"
#' #' )
#' #'
#' #' }
#' item_parceling <- function(data, ..., fun = mean, type = "data") {
#'
#'   terms <- list(...)
#'   parcel_names <- names(terms)
#'
#'   if (is.null(parcel_names) || any(parcel_names == "")) {
#'     stop("All parcel specifications must be named (e.g., NewVar = c(\"x:y\")).")
#'   }
#'
#'   cols_check <- list()
#'   new_data <- data.frame(row.names = rownames(data))
#'
#'   for (i in seq_along(terms)) {
#'
#'     cols <- terms[[i]]
#'
#'     ## Numeric index selection
#'     if (is.numeric(cols)) {
#'       cols <- colnames(data)[cols]
#'     }
#'
#'     ## Range selection (e.g., "a:b")
#'     if (is.character(cols) && length(cols) == 1 && grepl(":", cols)) {
#'       rng <- strsplit(cols, ":")[[1]]
#'       cols <- names(data)[
#'         which(names(data) == rng[1]) :
#'           which(names(data) == rng[2])
#'       ]
#'     }
#'
#'     cols_check[[parcel_names[i]]] <- cols
#'
#'     new_data[[parcel_names[i]]] <-
#'       apply(data[cols], 1, fun, na.rm = TRUE)
#'
#'     data[[parcel_names[i]]] <- new_data[[parcel_names[i]]]
#'   }
#'
#'   res <- list(
#'     data = data,
#'     new  = new_data,
#'     cols_check = cols_check
#'   )
#'
#'   switch(
#'     type,
#'     data       = data,
#'     res        = data,
#'     new        = new_data,
#'     cols_check = cols_check,
#'     all        = res
#'   )
#' }
