
#' Print a Markdown Table to the R Console
#'
#' Convert a data frame or matrix to a Markdown pipe table and
#' print it to the console. Missing values are displayed as empty
#' cells without modifying the original data.
#'
#' @param data A data frame, tibble, or matrix to display.
#' @param caption Optional character scalar for the table caption.
#' @param nicetable Logical; if TRUE, apply nice_table().
#' @param digits Non-negative integer; decimal places. Default 3.
#' @param print Logical; if TRUE, print the Markdown table.
#'
#' @return Invisibly returns a character vector containing the
#'   Markdown table. Also prints it when print = TRUE.
#' @seealso [knitr::kable()]
#' @export
#'
#' @examples
#' # Example 1: Basic table
#' md_console(head(mtcars), nicetable = FALSE)
#'
#' # Example 2: Table with caption
#' md_console(head(iris), caption = "Iris data",
#'            nicetable = FALSE)
#'
#' # Example 3: Missing values
#' dat <- data.frame(
#'   group = c("A", NA, "C"),
#'   score = c(3.14159, NA, 5.67891)
#' )
#' md_console(dat, nicetable = FALSE)
#'
#' # Example 4: Change decimal places
#' md_console(dat, digits = 2, nicetable = FALSE)
#'
#' # Example 5: Capture Markdown without printing
#' txt <- md_console(dat, nicetable = FALSE, print = FALSE)
#' cat(txt, sep = "\n")
#'
#' # Example 6: Matrix input
#' md_console(matrix(c(1, NA, 3, 4), nrow = 2),
#'            nicetable = FALSE)
#'
#' # Example 7: Preserve the original sentinel value
#' md_console(data.frame(x = c(-123456.789, NA)),
#'            nicetable = FALSE)
#'
#' # Example 8: Use nice_table() in jjstat
#' \dontrun{
#' md_console(head(mtcars), nicetable = TRUE)
#' }

md_console <- function(data, caption = NULL,
                       nicetable = TRUE,
                       digits = 3, print = TRUE) {

  # 1. Validate input data
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame, tibble, or matrix.",
         call. = FALSE)
  }

  # 2. Validate logical arguments
  if (!is.logical(nicetable) ||
      length(nicetable) != 1L || is.na(nicetable)) {
    stop("'nicetable' must be TRUE or FALSE.",
         call. = FALSE)
  }

  if (!is.logical(print) ||
      length(print) != 1L || is.na(print)) {
    stop("'print' must be TRUE or FALSE.",
         call. = FALSE)
  }

  # 3. Validate decimal places
  if (!is.numeric(digits) || length(digits) != 1L ||
      is.na(digits) || !is.finite(digits) ||
      digits < 0 || digits != floor(digits)) {
    stop("'digits' must be a non-negative integer.",
         call. = FALSE)
  }

  # 4. Validate caption
  if (!is.null(caption) &&
      (!is.character(caption) ||
       length(caption) != 1L || is.na(caption))) {
    stop("'caption' must be NULL or a character scalar.",
         call. = FALSE)
  }

  # 5. Prepare data without changing original values
  out <- as.data.frame(data, optional = TRUE)

  # 6. Apply nice_table() when requested
  if (nicetable) {
    out <- nice_table(out)

    if (!is.data.frame(out) && !is.matrix(out)) {
      stop("'nice_table()' must return a data frame or matrix.",
           call. = FALSE)
    }
  }

  # 7. Generate Markdown table
  # knitr::kable() handles missing values directly
  result <- knitr::kable(
    out,
    format = "pipe",
    digits = digits,
    row.names = FALSE,
    caption = caption,
    na = ""
  )

  # 8. Print Markdown to console
  if (print) {
    cat(result, sep = "\n")
  }

  # 9. Return Markdown text invisibly
  invisible(result)
}
