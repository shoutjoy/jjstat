
#' Create a Nice Table by Removing Consecutive Duplicate Values
#'
#' Replaces consecutive duplicate values with empty strings ("")
#' in selected columns of a data frame.
#' Useful for formatting statistical tables and research reports.
#'
#' @param data A data frame to be formatted.
#' @param col NULL, numeric, or character vector.
#'   Columns to process. NULL selects all columns.
#'   Supports column names, indices, and ranges such as
#'   "Var:subfactor".
#' @param exclude NULL, numeric, or character vector.
#'   Columns to exclude from processing.
#'   Supports the same selection syntax as col.
#'
#' @return A data frame with consecutive duplicate values
#'   replaced by empty strings in selected columns.
#'   Unselected columns remain unchanged.
#'
#' @details
#' Only consecutive duplicate values are replaced.
#' Non-consecutive repeated values are retained.
#' Numeric and factor columns selected for processing
#' are converted to character.
#' Missing values are preserved.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   Var = c("a", "a", "a", "b", "b", "c"),
#'   alpha = c(.85, .85, .85, .80, .80, .53),
#'   item = c("B01", "B02", "B03", "B04", "B05", "B06"),
#'   score = c(1, 1, 2, 2, 3, 3)
#' )
#'
#' # Example 1: All columns
#' nice_table(df)
#'
#' # Example 2: Select one column
#' nice_table(df, col = "Var")
#'
#' # Example 3: Select multiple columns
#' nice_table(df, col = c("Var", "alpha"))
#'
#' # Example 4: Select columns by position
#' nice_table(df, col = c(1, 2))
#'
#' # Example 5: Select a column range
#' nice_table(df, col = "Var:alpha")
#'
#' # Example 6: Exclude columns
#' nice_table(df, exclude = "item")
#'
#' # Example 7: Select and exclude
#' nice_table(df, col = 1:3, exclude = 2)
#'
#' # Example 8: Keep numeric score unchanged
#' nice_table(df, col = "Var")
#'
nice_table <- function(data, col = NULL, exclude = NULL) {

  # 1. Validate input data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  # 2. Helper: Resolve column selections
  resolve_cols <- function(selection, data) {

    if (is.null(selection)) {
      return(character(0))
    }

    cn <- names(data)

    # Numeric column indices
    if (is.numeric(selection)) {

      if (anyNA(selection) ||
          any(!is.finite(selection)) ||
          any(selection != as.integer(selection)) ||
          any(selection < 1 | selection > ncol(data))) {
        stop("Invalid column indices.")
      }

      return(unique(cn[selection]))
    }

    # Character column names or ranges
    if (is.character(selection)) {

      result <- character(0)

      for (item in selection) {

        # Exact column name takes priority
        if (item %in% cn) {
          result <- c(result, item)
          next
        }

        # Range expression, e.g. "Var:alpha"
        if (grepl(":", item, fixed = TRUE)) {

          parts <- strsplit(item, ":", fixed = TRUE)[[1]]

          if (length(parts) != 2L ||
              !all(parts %in% cn)) {
            stop("Invalid column range: ", item)
          }

          start <- match(parts[1], cn)
          end <- match(parts[2], cn)

          result <- c(
            result,
            cn[seq.int(start, end)]
          )

        } else {
          stop("Column not found: ", item)
        }
      }

      return(unique(result))
    }

    stop("'col' and 'exclude' must be numeric, ",
         "character, or NULL.")
  }

  # 3. Select target columns
  if (is.null(col)) {
    selected_cols <- names(data)
  } else {
    selected_cols <- resolve_cols(col, data)
  }

  # 4. Resolve excluded columns
  excluded_cols <- resolve_cols(exclude, data)

  # 5. Remove excluded columns from selection
  selected_cols <- setdiff(selected_cols, excluded_cols)

  # 6. Process selected columns only
  for (column in selected_cols) {

    x <- data[[column]]

    # Convert selected columns to character
    x <- as.character(x)

    # Skip empty or single-row columns
    if (length(x) <= 1L) {
      data[[column]] <- x
      next
    }

    # Compare each value with its previous value
    same <- !is.na(x[-1]) &
      !is.na(x[-length(x)]) &
      x[-1] == x[-length(x)]

    # Replace consecutive duplicates
    x[c(FALSE, same)] <- ""

    # Update selected column
    data[[column]] <- x
  }

  # 7. Return formatted data
  return(data)
}
