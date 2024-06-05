#' binary_recode
#'
#' @param data data
#' @param variable  original variable
#' @param recode_list list recode variable
#' @param new_name new name
#' @param tail tails _grp
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예제 데이터
#' data <- data.frame(
#'   학력 = factor(c("고졸", "대학졸", "석박사졸", "고졸", "석박사졸")),
#'   grade = 1:5
#' )
#' data
#' binary_recode(data, grade, list(미숙자 = 1:3, 숙달자 = 4:5))
#' binary_recode(data, grade, list(미숙자 = c(1,2,3), 숙달자 = 4:5))
#' binary_recode(data, "grade", list(미숙자 = 1:3, 숙달자= 4:5), new_name="수준")
#'
#' binary_recode(data, 학력, list(저학년= 1:2, 고학년= 3))
#' binary_recode(data, "학력", list(저학년= 1:2, 고학년= 3))
#' binary_recode(data, "학력", list(저학년= c("고졸","대학졸"), 고학년= 3))
#' binary_recode(data, 학력, list(저학년= c("고졸","대학졸"), 고학년= 3))
#' binary_recode(data, 학력, list(저학년= c("고졸","대학졸"), 고학년= "석박사졸"))

#'
#' }
#'
binary_recode <- function(data, variable, recode_list, new_name=NULL, tail = "_grp") {
  # Convert the variable to a string name if it's not a character
  variable_name <- if (is.character(substitute(variable))) {
    substitute(variable)
  } else {
    deparse(substitute(variable))
  }

  # Check if the variable exists in the data
  if (!variable_name %in% names(data)) {
    stop("Variable not found in the data")
  }

  # Extract the variable from the data
  var <- data[[variable_name]]

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
  if (is.null(new_name)) {
    new_variable_name <- paste0(variable_name, tail)
  } else {
    new_variable_name <- new_name
  }

  # Add the new variable to the data
  data[[new_variable_name]] <- new_var

  return(data)
}
