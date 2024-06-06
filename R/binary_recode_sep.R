#' Multi-recoding binary variables
#'
#' @param data data
#' @param ... recode list
#' @param new_names newnaem list
#' @param tail tail _grp
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'
#'
#'
#' # 예제 데이터
#' data <- data.frame(
#'   학력 = factor(c("고졸", "대학졸", "석박사졸", "고졸", "석박사졸")),
#'   grade = 1:5
#' )
#' data
#'
#'
#' # 함수 호출
#' binary_recode_sep(  data,
#'                     "학력", list(저학년 = 1:2, 고학년 = 3),
#'                     "grade", list(저학년 = 1:2, 고학년 = 3:5),
#'                     new_names = c("학력_grp", "grade_grp")
#' )
#'
#'
#'
#'

#' }
binary_recode_sep <- function(data, ..., new_names=NULL, tail = "_grp") {
  # Get the list of variable and recode_list pairs
  args <- list(...)
  num_pairs <- length(args) / 2

  # Check if the number of arguments is even
  if (length(args) %% 2 != 0) {
    stop("Please provide variable and recode_list pairs.")
  }

  # Initialize a counter for new_names
  new_name_counter <- 1

  for (i in seq(1, length(args), by=2)) {
    variable <- args[[i]]
    recode_list <- args[[i+1]]

    # Convert the variable name to a string
    variable_name <- deparse(substitute(variable))

    # Evaluate the variable name within the data frame
    var <- eval(substitute(data[[variable]]), data, parent.frame())

    # Check if the variable exists in the data
    if (is.null(var)) {
      stop("Variable not found in the data")
    }

    # Check if the variable is a character, factor, or numeric
    if (!is.character(var) && !is.factor(var) && !is.numeric(var)) {
      stop("Variable must be a character, factor, or numeric")
    }

    # If the variable is a factor, get the levels
    if (is.factor(var)) {
      levels <- levels(var)
      # Create a named list of levels to their indices
      level_indices <- setNames(seq_along(levels), levels)
    } else {
      levels <- unique(var)
    }

    # Create a new variable with recoded values, converted to character
    new_var <- as.character(var)
    for (group in names(recode_list)) {
      values <- recode_list[[group]]
      if (is.factor(var) && is.numeric(values)) {
        # Convert numeric values to corresponding factor levels
        values <- levels[values]
      }
      new_var[new_var %in% values] <- group
    }

    # Convert the new variable back to factor
    new_var <- factor(new_var)

    # Determine the name of the new variable
    if (!is.null(new_names) && length(new_names) >= new_name_counter) {
      new_variable_name <- new_names[[new_name_counter]]
    } else {
      new_variable_name <- paste0(variable_name, tail)
    }

    # Add the new variable to the data
    data[[new_variable_name]] <- new_var

    # Increment the new_name counter
    new_name_counter <- new_name_counter + 1
  }

  return(data)
}
