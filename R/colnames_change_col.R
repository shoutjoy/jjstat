#' Change Selected Column Names by Index or Name
#'
#' Rename selected columns in a data frame using numeric positions,
#' existing column names, or a mixture of both.
#'
#' The function preserves unselected column names and supports
#' partial renaming. Empty strings ("") and NA in impute preserve
#' the corresponding original column names.
#'
#' @param df A data.frame or tibble whose column names will be changed.
#' @param col NULL, numeric vector, character vector, or list.
#'   Specifies columns to rename.
#'   Numeric values indicate column positions (starting at 1).
#'   Character values indicate existing column names.
#'   A list can combine numeric and character selectors.
#'   If NULL, columns are selected sequentially from the first column.
#' @param impute Character vector of replacement column names.
#'   Must have the same length as the selected columns when col
#'   is not NULL. When col is NULL, names are replaced from
#'   the first column, and shorter vectors are permitted.
#'   "" or NA preserves the original column name.
#'
#' @details
#' Column positions and names are resolved against the original
#' data frame before any renaming takes place.
#'
#' Duplicate column selections, unknown column names,
#' invalid positions, and duplicate resulting column names
#' generate errors.
#'
#' No additional packages are required.
#'
#' @return A data frame with updated column names.
#'   The original data values and column order are preserved.
#'
#' @seealso [base::names()], [base::colnames()]
#'
#' @export
#'
#' @examples
#' # Example 1: Rename the first column
#' colnames_change_col(mtcars, col = 1, impute = "MPG")
#'
#' # Example 2: Rename a column by its name
#' colnames_change_col(mtcars, col = "mpg",
#'                     impute = "Miles_per_Gallon")
#'
#' # Example 3: Rename multiple columns by position
#' colnames_change_col(mtcars, col = c(1, 3, 5),
#'                     impute = c("MPG", "DISP", "DRAT"))
#'
#' # Example 4: Rename multiple columns by name
#' colnames_change_col(mtcars, col = c("mpg", "cyl"),
#'                     impute = c("Mileage", "Cylinders"))
#'
#' # Example 5: Mixed numeric and character selectors
#' colnames_change_col(mtcars, col = list(1, "cyl", 5),
#'                     impute = c("MPG", "CYL", "DRAT"))
#'
#' # Example 6: Preserve a selected column name
#' colnames_change_col(mtcars, col = 1:3,
#'                     impute = c("MPG", "", "DISP"))
#'
#' # Example 7: Rename columns sequentially
#' colnames_change_col(mtcars, impute = c("MPG", "CYL"))
#'
#' # Example 8: Rename all columns
#' colnames_change_col(iris, col = NULL,
#'                     impute = paste0("V", 1:5))
#'
#' # Example 9: Mixed selector with a numeric range
#' colnames_change_col(mtcars, col = list(1:3, "wt"),
#'                     impute = c("MPG", "CYL", "DISP", "WEIGHT"))
#'
#' # Example 10: Preserve a name using NA
#' colnames_change_col(mtcars, col = 1:3,
#'                     impute = c("MPG", NA, "DISP"))
#'
#' # Example 11: No change
#' colnames_change_col(mtcars)
#'
#' # Example 12: Invalid position (error)
#' \dontrun{
#' colnames_change_col(mtcars, col = 100, impute = "NEW")
#' }
#'
colnames_change_col <- function(df,
                                col = NULL,
                                impute = NULL) {


  # Step 1. Validate input data


  if (!is.data.frame(df)) {
    stop("df must be a data.frame or tibble.",
         call. = FALSE)
  }

  old_names <- names(df)
  n <- ncol(df)

  if (anyNA(old_names) || anyDuplicated(old_names)) {
    stop("Original column names must be unique and non-missing.",
         call. = FALSE)
  }

  # No replacement names: return original data
  if (is.null(impute)) {
    return(df)
  }


  # Step 2. Validate replacement names


  if (!is.character(impute)) {
    stop("impute must be a character vector.",
         call. = FALSE)
  }

  if (length(impute) == 0L) {
    return(df)
  }

  # --------------------------------------------------
  # Step 3. Resolve column selectors
  # --------------------------------------------------

  if (is.null(col)) {

    # Select columns sequentially from the beginning
    if (length(impute) > n) {
      stop("impute contains more names than available columns.",
           call. = FALSE)
    }

    positions <- seq_along(impute)

  } else {

    # Convert numeric/character selectors into a list
    selectors <- if (is.list(col)) col else list(col)

    # Flatten nested lists while preserving element types
    selectors <- unlist(
      selectors,
      recursive = TRUE,
      use.names = FALSE
    )

    if (length(selectors) == 0L) {
      stop("col must contain at least one column selector.",
           call. = FALSE)
    }

    # Important:
    # A mixed atomic vector created by c(1, "mpg")
    # converts all elements to character.
    # Character numbers are interpreted as positions
    # only if they do not match existing column names.

    positions <- integer(length(selectors))

    for (i in seq_along(selectors)) {

      value <- selectors[i]

      if (is.na(value)) {
        stop("col cannot contain NA.", call. = FALSE)
      }

      if (is.numeric(value)) {

        # Numeric column index
        if (!is.finite(value) ||
            value != as.integer(value) ||
            value < 1 ||
            value > n) {
          stop("Invalid numeric column position: ", value,
               call. = FALSE)
        }

        positions[i] <- as.integer(value)

      } else if (is.character(value)) {

        # Match existing column name first
        idx <- match(value, old_names)

        if (!is.na(idx)) {
          positions[i] <- idx

        } else if (grepl("^[0-9]+$", value)) {

          # Allow numeric-looking character selectors
          pos <- suppressWarnings(as.numeric(value))

          if (!is.finite(pos) || pos < 1 ||
              pos > n || pos != floor(pos)) {
            stop("Invalid column position: ", value,
                 call. = FALSE)
          }

          positions[i] <- as.integer(pos)

        } else {
          stop("Column not found: ", value,
               call. = FALSE)
        }

      } else {
        stop("col must contain numeric or character values.",
             call. = FALSE)
      }
    }

    # Reject repeated column selections
    if (anyDuplicated(positions)) {
      stop("Duplicate columns selected in col.",
           call. = FALSE)
    }

    if (length(impute) != length(positions)) {
      stop(
        "The length of impute must match the number ",
        "of selected columns.",
        call. = FALSE
      )
    }
  }


  # Step 4. Replace selected column names


  new_names <- old_names

  # "" and NA preserve original column names
  replace <- !is.na(impute) & impute != ""

  if (any(replace)) {
    new_names[positions[replace]] <- impute[replace]
  }

  # --------------------------------------------------
  # Step 5. Check resulting column names
  # --------------------------------------------------

  if (anyDuplicated(new_names)) {
    duplicated_names <- unique(
      new_names[duplicated(new_names)]
    )

    stop(
      "Duplicate column names after renaming: ",
      paste(duplicated_names, collapse = ", "),
      call. = FALSE
    )
  }

  # --------------------------------------------------
  # Step 6. Apply changes and return data
  # --------------------------------------------------

  names(df) <- new_names

  return(df)
}
