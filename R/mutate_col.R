


#' Add or Modify Columns at a Specified Position
#'
#' Adds or modifies columns using dplyr-style expressions.
#' Existing columns can be referenced directly inside `...`.
#' Newly created columns can be placed at a specified position.
#'
#' @param data A data.frame or tibble.
#' @param ... Named expressions defining new or modified columns.
#' @param col Integer. Position of the first new column.
#'   Default is the last position (append).
#'
#' @return A data.frame or tibble with added or modified columns.
#' @export
#' @importFrom dplyr mutate
#'
#' @examples
#' # Example 1: Add a numeric column
#' mutate_col(iris, New = Sepal.Length * 2)
#'
#' # Example 2: Add a column at position 1
#' mutate_col(iris, ID = seq_len(nrow(iris)), col = 1)
#'
#' # Example 3: Create multiple columns
#' mutate_col(iris,
#'   Ratio = Sepal.Length / Sepal.Width,
#'   Total = Sepal.Length + Petal.Length)
#'
#' # Example 4: Recode a categorical variable
#' mutate_col(iris,
#'   Species_KR = dplyr::recode(
#'     as.character(Species),
#'     setosa = "세토사",
#'     versicolor = "버시컬러",
#'     virginica = "버지니카"
#'   )
#' )
#'
#' # Example 5: Add a column at position 3
#' mutate_col(iris, New = Sepal.Length * 2, col = 3)
#'
#' #' fm <- data.frame(
#' #'   age = c("20대", "20대", "20대", "30대",
#' #'   "30대", "30대", "40대", "40대", "40대"),
#' #'   manufacturer = c("google", "apple", "samsung",
#' #'   "google", "apple", "samsung", "google",
#' #'    "apple", "samsung"),
#' #'   values = c(794, 804, 1243, 987, 1055,
#' #'   2517, 1528, 1519, 2255)
#' #' )
#' #' fm
#' #'
#' #' #data
#' #' fm %>% mutate_col(gender= rep("a",9))
#' #' fm %>% mutate_col(gender= rep("a",9), col = 2)
#' #'
#' #' #position
#' #' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 2)
#' #' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 1)
#' #' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 3)
#' #' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 4)
#' #'
#' #' #Build and adjust with data from within
#' #' fm %>% mutate(gender = age)
#' #' fm %>% mutate_col(age_int = substring(fm$age,1,2))
#' #' fm %>% mutate_col(gender = paste(fm$age,fm$values,sep="//"))
#' #'
#' #' fm %>% mutate_col(gender = paste(.$age,.$values,sep="//"), col=2)
#' #' # Double data generation
#' #' fm %>%
#' #'   mutate_col(insert_2 = rep("female", nrow(fm)), col = 2) %>%
#' #'   mutate_col( insert_4 = rep("a",9), col = 4)
#' #'
#' #'
#' #' # second data
#' #' data2 <- data.frame(
#' #'   weight = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14,
#' #'              4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69,
#' #'              6.31, 5.12, 5.54, 5.50, 5.37, 5.29, 4.92, 6.15, 5.80, 5.26),
#' #'   ids = c(paste0("C", 1:10), paste0("T",1:10), paste0("S", 1:10))
#' #' )
#' #' data2 %>%mutate_col(group= .$weight, col=2)
#'
mutate_col <- function(data, ..., col = ncol(data) + 1L) {

  # Step 1: Validate input data
  if (!is.data.frame(data)) {
    stop("data must be a data.frame or tibble.")
  }

  # Step 2: Capture new column names without evaluation
  exprs <- as.list(substitute(list(...)))[-1L]
  new_names <- names(exprs)

  if (length(exprs) == 0L ||
      is.null(new_names) ||
      anyNA(new_names) ||
      any(!nzchar(new_names)) ||
      anyDuplicated(new_names)) {
    stop("All new columns must have unique names.")
  }

  # Step 3: Validate insertion position
  original_names <- names(data)
  original_ncol <- ncol(data)

  if (length(col) != 1L ||
      !is.numeric(col) ||
      is.na(col) ||
      !is.finite(col) ||
      col != as.integer(col) ||
      col < 1L ||
      col > original_ncol + 1L) {
    stop("col must be an integer between 1 and ncol(data) + 1.")
  }

  # Step 4: Evaluate expressions within the data mask
  # This allows direct use of existing column names.
  result <- dplyr::mutate(data, ...)

  # Step 5: Position new or modified columns
  remaining <- original_names[
    !original_names %in% new_names
  ]

  insert_at <- sum(
    original_names[seq_len(col - 1L)] %in% remaining
  )

  ordered_names <- append(
    remaining,
    new_names,
    after = insert_at
  )

  # Step 6: Return data with requested column order
  result[, ordered_names, drop = FALSE]
}
