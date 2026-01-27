
#' Print and Inspect Data with Multiple Output Modes
#'
#' A unified data inspection function supporting console printing,
#' RStudio viewing, interactive DataTables (DT), and publication-ready
#' tables via knitr::kable().
#'
#' In DT mode, the table is rendered in a web browser by default, with
#' automatic variable-type detection and a summary panel displayed above
#' the table.
#'
#' In kable mode, output is routed automatically:
#' markdown / pipe / latex -> console
#' html                    -> web browser
#'
#' @param data A data.frame or tibble.
#' @param n Number of rows to display. Default is Inf.
#' @param type Output type: "print", "view", "dt", or "kable".
#'
#' @param dt_browser Logical. If TRUE (default), DT output opens in browser.
#' @param dt_page Number of rows per DT page.
#' @param dt_scroll_y Vertical scroll height (px) for DT.
#' @param dt_digits Decimal digits for numeric variables in DT.
#' @param dt_highlight_na Logical; highlight missing values in DT.
#'
#' @param kable_format Output format for kable:
#'   "markdown", "html", "latex", or "pipe".
#' @param kable_digits Decimal digits for numeric values in kable output.
#'
#' @param max_n Safety limit for maximum rows.
#' @param summary Logical; print row/column summary.
#' @param return_data Logical; return sliced data invisibly.
#'
#' @return Invisibly returns the sliced data.
#' @export
#'
#' @examples
#' pall(mtcars, n = 10)
#' pall(iris, type = "dt")
#' pall(head(mtcars), type = "kable", kable_format = "markdown")
#' pall(head(mtcars), type = "kable", kable_format = "htmml")
#'
pall <- function(data,
                 n = Inf,
                 type = c("print", "view", "dt", "kable"),

                 # DT options
                 dt_browser = TRUE,
                 dt_page = 20,
                 dt_scroll_y = 650,
                 dt_digits = 2,
                 dt_highlight_na = TRUE,

                 # kable options
                 kable_format = c("markdown", "html", "latex", "pipe"),
                 kable_digits = 3,

                 max_n = 5000,
                 summary = TRUE,
                 return_data = FALSE){

  type <- match.arg(type)
  kable_format <- match.arg(kable_format)

  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame or tibble.")
  }

  if (summary) {
    cat("Rows:", nrow(data), " | Cols:", ncol(data), "\n")
  }

  if (nrow(data) > max_n && n == Inf) {
    warning("Rows exceed max_n. Consider setting n explicitly.")
  }

  data_out <- dplyr::slice_head(data, n = n)

  # -------------------------------------------------
  # Variable type detection
  # -------------------------------------------------
  .detect_var_type <- function(x) {
    if (is.factor(x)) return(ifelse(is.ordered(x), "ordered", "factor"))
    if (is.character(x)) return("factor")
    if (is.numeric(x)) {
      ux <- unique(na.omit(x))
      if (length(ux) <= 2) return("binary")
      if (all(ux %in% 1:7)) return("likert")
      return("numeric")
    }
    "other"
  }

  var_types <- sapply(data_out, .detect_var_type)
  var_tab   <- table(var_types)

  # -------------------------------------------------
  # DT MODE (browser default)
  # -------------------------------------------------
  if (type == "dt") {

    summary_html <- paste0(
      "<div style='padding:10px;
                   margin-bottom:8px;
                   background:#f8f9fa;
                   border-left:6px solid #2c7be5;
                   font-family:Arial;'>
       <b>Variable Type Summary</b><ul>",
      paste0(
        "<li>", names(var_tab), ": ", as.integer(var_tab), "</li>",
        collapse = ""
      ),
      "</ul></div>"
    )

    container <- htmltools::withTags(
      table(
        class = "display",
        thead(tr(lapply(colnames(data_out), th))),
        tbody(),
        caption = htmltools::HTML(summary_html)
      )
    )

    dt <- DT::datatable(
      data_out,
      rownames = FALSE,
      filter = "top",
      container = container,
      extensions = "Scroller",
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = dt_scroll_y,
        scroller = TRUE,
        pageLength = dt_page,
        searchHighlight = TRUE
      )
    )

    # Likert highlighting
    likert_cols <- which(var_types == "likert")
    if (length(likert_cols) > 0) {
      dt <- DT::formatStyle(
        dt,
        likert_cols,
        backgroundColor = DT::styleColorBar(
          range(data_out[, likert_cols], na.rm = TRUE),
          "#cce5ff"
        ),
        backgroundSize = "95% 80%",
        backgroundRepeat = "no-repeat"
      )
    }

    # Numeric formatting
    num_cols <- which(var_types == "numeric")
    if (length(num_cols) > 0) {
      dt <- DT::formatRound(dt, num_cols, dt_digits)
    }

    # Factor / binary alignment
    fac_cols <- which(var_types %in% c("factor", "binary", "ordered"))
    if (length(fac_cols) > 0) {
      dt <- DT::formatStyle(dt, fac_cols, textAlign = "center")
    }

    # NA highlighting
    if (dt_highlight_na) {
      dt <- DT::formatStyle(
        dt,
        columns = names(data_out),
        backgroundColor = DT::styleEqual(NA, "#ffecec")
      )
    }

    if (dt_browser) {
      tf <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(dt, tf, selfcontained = TRUE)
      utils::browseURL(tf)
      invisible(dt)
    } else {
      return(dt)
    }
  }

  # -------------------------------------------------
  # PRINT
  # -------------------------------------------------
  if (type == "print") {
    print(data_out, n = n)
  }

  # -------------------------------------------------
  # VIEW
  # -------------------------------------------------
  if (type == "view") {
    View(data_out)
  }

  # -------------------------------------------------
  # KABLE (format-based routing)
  # -------------------------------------------------
  if (type == "kable") {

    kb <- knitr::kable(
      data_out,
      format = kable_format,
      digits = kable_digits
    )

    if (kable_format == "html") {

      tf <- tempfile(fileext = ".html")
      htmltools::save_html(htmltools::HTML(kb), file = tf)
      utils::browseURL(tf)
      invisible(kb)

    } else {
      # markdown / pipe / latex -> console
      print(kb)
      invisible(kb)
    }
  }

  if (return_data) {
    return(data_out)
  } else {
    invisible(data_out)
  }
}
