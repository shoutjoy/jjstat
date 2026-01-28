
#' Extract Model Fit Indices for Multilevel SEM
#'
#' @description
#' This function extracts model fit indices from a fitted
#' multilevel SEM object (lavaan), with robust fit measures prioritized
#' when available. The output can be returned in long (APA-style) or
#' wide format depending on the selected type.
#'
#' @param fit A fitted lavaan object (e.g., from \code{sem()} with
#'   \code{cluster} specified).
#' @param type Output type.
#'   One of \code{"data"}, \code{"long"}, \code{"wide"}, or \code{"apa"}.
#'   \itemize{
#'     \item \code{"data"}: APA-style long format (Index–Value).
#'     \item \code{"long"}: Same as \code{"data"}.
#'     \item \code{"wide"}: One-row wide data.frame.
#'     \item \code{"apa"}: Same as \code{"wide"} (alias for reporting).
#'   }
#' @param robust Logical. If TRUE (default), robust fit indices
#'   (e.g., robust CFI, TLI, RMSEA) are used when available.
#' @param digits Integer. Number of decimal places for rounding.
#'
#' @return
#' A data.frame containing model fit indices.
#' The structure depends on \code{type}.
#'
#' @details
#' This function is designed to be used upstream of
#' \code{mlsem_modelfit_paper()}.
#' It performs no table formatting and returns numeric values only.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ############################################
#' # Example 1. Typical usage after ML-SEM
#' ############################################
#'
#' fit_tab_long <- mlsem_fit_indices(fit_mlsem, type = "long")
#' fit_tab_data <- mlsem_fit_indices(fit_mlsem, type = "data")
#' fit_tab_wide <- mlsem_fit_indices(fit_mlsem, type = "wide")
#' fit_tab_apa  <- mlsem_fit_indices(fit_mlsem, type = "apa")
#'
#' # Pipe-friendly usage
#' mlsem_fit_indices(fit_mlsem) %>%
#'   mlsem_modelfit_paper(type = "kable", format = "markdown")
#'
#'
#' ############################################
#' # Example 2. Simple Multilevel SEM example
#' ############################################
#'
#' library(lavaan)
#'
#' # Simulated multilevel data
#' set.seed(123)
#' dat <- data.frame(
#'   university = rep(1:25, each = 40),
#'   x1 = rnorm(1000),
#'   x2 = rnorm(1000),
#'   y1 = rnorm(1000),
#'   y2 = rnorm(1000)
#' )
#'
#' # Multilevel SEM model
#' model_ml <- '
#'   level: 1
#'     X =~ x1 + x2
#'     Y =~ y1 + y2
#'     Y ~ X
#'
#'   level: 2
#'     Y ~ X
#' '
#'
#' fit_mlsem <- sem(
#'   model_ml,
#'   data      = dat,
#'   cluster   = "university",
#'   estimator = "MLR"
#' )
#'
#' # Extract fit indices (APA long format)
#' fit_tab <- mlsem_fit_indices(fit_mlsem, type = "long")
#'
#' # Convert to paper-ready table
#' mlsem_modelfit_paper(fit_tab, type = "kable", format = "markdown")
#' }
mlsem_fit_indices <- function(fit,
                              type = c("data", "long", "wide", "apa"),
                              robust = TRUE,
                              digits = 3) {

  type <- match.arg(type)

  # -------------------------
  # 1. Safe extraction of fitMeasures
  # -------------------------
  req <- c(
    "chisq","df","pvalue",
    "cfi","tli",
    "cfi.robust","tli.robust",
    "rmsea","rmsea.ci.lower","rmsea.ci.upper",
    "rmsea.robust","rmsea.ci.lower.robust","rmsea.ci.upper.robust",
    "srmr_within","srmr_between",
    "aic","bic","sabic","bic2",
    "logl","npar","nobs"
  )

  fm <- tryCatch(
    lavaan::fitMeasures(fit, req),
    error = function(e) numeric(0)
  )

  fm <- as.list(fm)

  # -------------------------
  # 2. SABIC handling (priority)
  # -------------------------
  SABIC <- if (!is.null(fm$sabic)) {
    fm$sabic
  } else if (!is.null(fm$bic2)) {
    fm$bic2
  } else if (!anyNA(c(fm$logl, fm$npar, fm$nobs))) {
    (-2 * fm$logl) + fm$npar * log((fm$nobs + 2) / 24)
  } else {
    NA_real_
  }

  # -------------------------
  # 3. Robust-first selector
  # -------------------------
  pick <- function(r, p) {
    if (robust && !is.null(fm[[r]])) fm[[r]] else fm[[p]]
  }

  out <- data.frame(
    chisq        = fm$chisq,
    df           = fm$df,
    pvalue       = fm$pvalue,
    CFI          = pick("cfi.robust", "cfi"),
    TLI          = pick("tli.robust", "tli"),
    RMSEA        = pick("rmsea.robust", "rmsea"),
    RMSEA_L      = pick("rmsea.ci.lower.robust", "rmsea.ci.lower"),
    RMSEA_U      = pick("rmsea.ci.upper.robust", "rmsea.ci.upper"),
    SRMR_within  = fm$srmr_within,
    SRMR_between = fm$srmr_between,
    AIC          = fm$aic,
    BIC          = fm$bic,
    SABIC        = SABIC,
    stringsAsFactors = FALSE
  )

  out$chisq_df <- out$chisq / out$df

  # -------------------------
  # 4. Rounding
  # -------------------------
  out[] <- lapply(out, function(x) {
    if (is.numeric(x)) round(x, digits) else x
  })

  # -------------------------
  # 5. Output control
  # -------------------------
  # wide-style outputs
  if (type %in% c("wide", "apa")) {
    return(out)
  }

  # long-style outputs (data, long)
  data.frame(
    Index = names(out),
    Value = as.numeric(out[1, ]),
    stringsAsFactors = FALSE
  )
}
