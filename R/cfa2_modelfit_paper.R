#' Model Fit Table for Paper
#'
#' This function formats model fit indices into a publication-ready table
#' or returns the underlying data.frame depending on the selected options.
#' It is designed to be used after extracting model fit information
#' from a CFA or SEM object (e.g., lavaan output).
#'
#' @param x A data.frame or matrix containing model fit information.
#'   The object must include rows named \code{"criterian"} and \code{"Value"},
#'   and columns such as \code{chisq}, \code{df}, \code{pvalue},
#'   \code{rmsea}, \code{srmr}, \code{cfi}, and \code{tli}.
#' @param type Character. Output type.
#'   One of \code{"kable"} (default) or \code{"data"}.
#' @param format Character. Output format when \code{type = "kable"}.
#'   One of \code{"markdown"}, \code{"raw"}, or \code{"html"}.
#' @param caption Character. Table caption used for kable output.
#'
#' @return
#' \itemize{
#'   \item If \code{type = "data"}: a data.frame.
#'   \item If \code{type = "kable"} and \code{format = "raw"}: a data.frame.
#'   \item If \code{type = "kable"} and \code{format = "markdown"} or
#'   \code{"html"}: a formatted table object.
#' }
#'
#' @details
#' The function internally converts the degrees of freedom (df) value
#' to an integer to avoid outputs such as 97.000 in publication tables.
#' Formatting using \code{knitr::kable()} is applied only at the final
#' output stage, ensuring that raw data remain accessible for further
#' processing when needed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming modelfitdata is returned from cfa2(type = "modelfit")
#'
#' # Raw data.frame
#' modelfit_paper(modelfitdata, type = "data")
#'
#' # Markdown table for papers
#' modelfit_paper(modelfitdata, type = "kable", format = "markdown")
#'
#' # HTML table for Viewer or browser
#' modelfit_paper(modelfitdata, type = "kable", format = "html")
#' }

modelfit_paper <- function(x,
                           type   = c("kable", "data"),
                           format = c("markdown", "raw", "html"),
                           caption = "Model Fit Indices") {

  type   <- match.arg(type)
  format <- match.arg(format)

  mf <- x |>
    t() |>
    as.data.frame(stringsAsFactors = FALSE)

  mf[] <- lapply(mf, function(v) trimws(as.character(v)))

  df_int <- function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    as.character(as.integer(as.numeric(v)))
  }

  # ★ p-value formatter 추가
  p_fmt <- function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    num <- suppressWarnings(as.numeric(v))
    if (!is.na(num) && num < 0.001) {
      "< .001"
    } else {
      as.character(v)
    }
  }

  out_data <- data.frame(
    term = c("criterian", "Value"),

    chisq = c(
      mf["criterian","chisq"],
      paste0(
        mf["Value","chisq"],
        " (df = ",
        df_int(mf["Value","df"]),
        ")"
      )
    ),

    pvalue = c(
      mf["criterian","pvalue"],
      p_fmt(mf["Value","pvalue"])
    ),

    RMSEA = c(
      mf["criterian","rmsea"],
      mf["Value","rmsea"]
    ),

    SRMR = c(
      mf["criterian","srmr"],
      mf["Value","srmr"]
    ),

    CFI = c(
      mf["criterian","cfi"],
      mf["Value","cfi"]
    ),

    TLI = c(
      mf["criterian","tli"],
      mf["Value","tli"]
    ),

    stringsAsFactors = FALSE
  )

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

