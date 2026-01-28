
#' Model Fit Table for Multilevel SEM (Paper-ready)
#'
#' @description
#' This function formats multilevel SEM model fit indices extracted by
#' \code{mlsem_fit_indices()} into a publication-ready table.
#' It supports raw data output as well as markdown/html tables via kable.
#'
#' @param x A data.frame in APA long format with columns \code{Index} and \code{Value}.
#'   Typically returned from \code{mlsem_fit_indices(fit, type = "long")}.
#' @param type Output type. One of \code{"data"} or \code{"kable"}.
#' @param format Output format when \code{type = "kable"}.
#'   One of \code{"raw"}, \code{"markdown"}, or \code{"html"}.
#' @param caption Table caption used for kable output.
#'
#' @return
#' A data.frame (when \code{type = "data"} or \code{format = "raw"}),
#' or a formatted kable object (markdown/html).
#'
#' @details
#' This function is designed to be used after \code{mlsem_fit_indices()}.
#' All internal formatting helpers are intentionally hidden (prefixed with \code{.})
#' to keep the public API minimal and stable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ############################################
#' # Example 1. Typical workflow
#' ############################################
#'
#' fit_tab <- mlsem_fit_indices(fit_mlsem, type = "long")
#'
#' mlsem_modelfit_paper(fit_tab)
#' mlsem_modelfit_paper(fit_tab, type = "data")
#' mlsem_modelfit_paper(fit_tab, type = "kable")
#' mlsem_modelfit_paper(fit_tab, type = "kable", format = "raw")
#' mlsem_modelfit_paper(fit_tab, type = "kable", format = "markdown")
#' mlsem_modelfit_paper(fit_tab, type = "kable", format = "html")
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
#' # Simulated data
#' set.seed(123)
#' dat <- data.frame(
#'   univ = rep(1:30, each = 40),
#'   x1 = rnorm(1200),
#'   x2 = rnorm(1200),
#'   y1 = rnorm(1200),
#'   y2 = rnorm(1200)
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
#'   data    = dat,
#'   cluster = "univ",
#'   estimator = "MLR"
#' )
#'
#' # Extract fit indices
#' fit_tab <- mlsem_fit_indices(fit_mlsem, type = "long")
#'
#' # Paper-ready table
#' mlsem_modelfit_paper(fit_tab, type = "kable", format = "markdown")
#' }
mlsem_modelfit_paper <- function(x,
                                 type   = c("data", "kable"),
                                 format = c("raw", "markdown", "html"),
                                 caption = "Model Fit Indices (Multilevel SEM)") {

  type   <- match.arg(type)
  format <- match.arg(format)

  # -------------------------
  # 1. APA long → named vector
  # -------------------------
  vals <- setNames(x$Value, x$Index)

  # -------------------------
  # 2. formatter (internal)
  # -------------------------
  .df_int <- function(v) {
    if (is.na(v)) return(NA_character_)
    as.character(as.integer(v))
  }

  .p_fmt <- function(v) {
    if (is.na(v)) return(NA_character_)
    if (v < 0.001) "< .001" else formatC(v, digits = 3, format = "f")
  }

  .num_fmt <- function(v) {
    if (is.na(v)) return(NA_character_)
    formatC(v, digits = 3, format = "f")
  }

  # -------------------------
  # 3. Paper-style table
  # -------------------------
  out_data <- data.frame(
    term = c("criterian", "Value"),

    chisq = c(
      "Chisq",
      paste0(
        .num_fmt(vals["chisq"]),
        " (df = ",
        .df_int(vals["df"]),
        ")"
      )
    ),

    pvalue = c(
      "p > .05",
      .p_fmt(vals["pvalue"])
    ),

    RMSEA = c(
      "RMSEA < .08",
      .num_fmt(vals["RMSEA"])
    ),

    SRMR_within = c(
      "SRMR (within) < .08",
      .num_fmt(vals["SRMR_within"])
    ),

    SRMR_between = c(
      "SRMR (between) < .08",
      .num_fmt(vals["SRMR_between"])
    ),

    CFI = c(
      "CFI > .90",
      .num_fmt(vals["CFI"])
    ),

    TLI = c(
      "TLI > .90",
      .num_fmt(vals["TLI"])
    ),

    stringsAsFactors = FALSE
  )

  # named vector로 인한 행이름 제거
  rownames(out_data) <- NULL

  # -------------------------
  # 4. Output control
  # -------------------------
  if (type == "data") {
    return(out_data)
  }

  if (type == "kable") {

    if (format == "raw") {
      return(out_data)
    }

    if (format == "markdown") {
      return(
        knitr::kable(
          out_data,
          format  = "markdown",
          caption = caption
        )
      )
    }

    if (format == "html") {
      return(
        knitr::kable(
          out_data,
          format  = "html",
          caption = caption
        ) |>
          kableExtra::kable_styling(full_width = FALSE)
      )
    }
  }
}
