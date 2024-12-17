#' UInte data
#'
#' @param data data
#' @param col1 select col1
#' @param col2 select col2
#' @param Col col names
#' @param sep ""
#' @param remove true
#' @param left " or "["
#' @param right ""  or  "]"
#'
#' @return  data
#' @export
#'
#' @examples

#' \dontrun{
#' #'
#' # 예시 데이터 생성
#' data <- data.frame(
#'   block = c("IMAG", "IMAG", "IMAG", "IMAG"),
#'   name = c("imag1", "imag2", "imag3", "imag4"),
#'   Original = c("0.7093265 ***", "0.8773078 ***", "0.8417485 ***", "0.5691934 ***"),
#'   sig = c("***", "***", "***", "***"),
#'   Mean.Boot = c(0.7088215, 0.8750787, 0.8401757, 0.5647333),
#'   Std.Error = c(0.05949724, 0.02402834, 0.02970117, 0.07686419),
#'   perc.025 = c(0.5774529, 0.8231642, 0.7784983, 0.3876417),
#'   perc.975 = c(0.8105249, 0.9146114, 0.8863100, 0.6961572)
#' )
#'
#' # 예시 호출
#'
#' # Unite using column numbers
#' result1 <- Unite(data, 3, 4)
#' print(result1)
#'
#' # Unite using column numbers with a new column name
#' result2 <- Unite(data, 3, 4, "New")
#' print(result2)
#'
#' # Unite using column names
#' result3 <- Unite(data, "Original", "sig")
#' print(result3)
#'
#' # Unite using column names with a new column name
#' result4 <- Unite(data, "Original", "sig", "New")
#' print(result4)
#'
#'
#' }
#'#
Unite <- function(data, col1, col2, Col=NULL, sep="", remove=TRUE) {
  # Check if col1 and col2 are numeric
  if (is.numeric(col1) && is.numeric(col2)) {
    col1 <- names(data)[col1]
    col2 <- names(data)[col2]
  }

  if (is.null(Col)) {
    # Use tidyr::unite to combine the columns
    data <- tidyr::unite(data, col={{col1}}, all_of(c(col1, col2)), sep=sep, remove=remove)
  } else {
    # Use tidyr::unite to combine the columns
    data <- tidyr::unite(data, col={{Col}}, all_of(c(col1, col2)), sep=sep, remove=remove)
  }
  return(data)
}

#' Unite Columns in a Data Frame
#'
#' @param data A data frame.
#' @param col1 The first column to unite. Can be numeric (column index) or character (column name).
#' @param col2 The second column to unite. Can be numeric (column index) or character (column name).
#' @param Col Optional, name for the new column (default = NULL).
#' @param sep Separator string to use between the combined values (default = ",").
#' @param remove Logical, whether to remove the original columns after uniting (default = TRUE).
#' @param left String to add to the left side of the combined values (default = "[").
#' @param right String to add to the right side of the combined values (default = "]").
#'
#' @return A data frame with the combined column.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(
#'   block = c("IMAG", "IMAG", "IMAG", "IMAG"),
#'   name = c("imag1", "imag2", "imag3", "imag4"),
#'   Original = c("0.7093265 ***", "0.8773078 ***", "0.8417485 ***", "0.5691934 ***"),
#'   sig = c("***", "***", "***", "***"),
#'   Mean.Boot = c(0.7088215, 0.8750787, 0.8401757, 0.5647333),
#'   Std.Error = c(0.05949724, 0.02402834, 0.02970117, 0.07686419),
#'   perc.025 = c(0.5774529, 0.8231642, 0.7784983, 0.3876417),
#'   perc.975 = c(0.8105249, 0.9146114, 0.8863100, 0.6961572)
#' )
#'
#' # Unite using column numbers
#' result1 <- Unite_col(data, 3, 4)
#' print(result1)
#'
#' # Unite using column numbers with a new column name
#' result2 <- Unite_col(data, 3, 4, "New")
#' print(result2)
#'
#' # Unite using column names
#' result3 <- Unite_col(data, "Original", "sig")
#' print(result3)
#'
#' # Unite using column names with a new column name
#' result4 <- Unite_col(data, "Original", "sig", "New")
#' print(result4)
#' }
Unite_col <- function(data, col1, col2, Col = NULL, sep = ",", remove = TRUE, left = "[", right = "]") {
  # Check if col1 and col2 are numeric
  if (is.numeric(col1) && is.numeric(col2)) {
    col1 <- names(data)[col1]
    col2 <- names(data)[col2]
  }

  if (is.null(Col)) {
    # Create a new column name if Col is not provided
    new_col <- paste0(left, col1, sep, col2, right)
  } else {
    # Use the provided Col name
    new_col <- Col
  }

  # Combine the columns with the specified separators
  data <- tidyr::unite(data, col = {{new_col}},
                       all_of(c(col1, col2)), sep = sep, remove = remove)

  # Add left and right to the combined column
  data[[new_col]] <- paste0(left, data[[new_col]], right)

  return(data)
}


#' Unite Columns Confint in a Data Frame
#'
#' @param data A data frame.
#' @param col1 The first column to unite. Can be numeric (column index) or character (column name).
#' @param col2 The second column to unite. Can be numeric (column index) or character (column name).
#' @param Col Optional, name for the new column (default = NULL).
#' @param sep Separator string to use between the combined values (default = ",").
#' @param remove Logical, whether to remove the original columns after uniting (default = TRUE).
#' @param left String to add to the left side of the combined values (default = "[").
#' @param right String to add to the right side of the combined values (default = "]").
#'
#' @return A data frame with the combined column.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(
#'   block = c("IMAG", "IMAG", "IMAG", "IMAG"),
#'   name = c("imag1", "imag2", "imag3", "imag4"),
#'   Original = c("0.7093265 ***", "0.8773078 ***", "0.8417485 ***", "0.5691934 ***"),
#'   sig = c("***", "***", "***", "***"),
#'   Mean.Boot = c(0.7088215, 0.8750787, 0.8401757, 0.5647333),
#'   Std.Error = c(0.05949724, 0.02402834, 0.02970117, 0.07686419),
#'   perc.025 = c(0.5774529, 0.8231642, 0.7784983, 0.3876417),
#'   perc.975 = c(0.8105249, 0.9146114, 0.8863100, 0.6961572)
#' )
#'
#' # Unite using column numbers
#' result1 <- Unite_confint(data, 3, 4)
#' print(result1)
#'
#' # Unite using column numbers with a new column name
#' result2 <- Unite_confint(data, 3, 4, "New")
#' print(result2)
#'
#' # Unite using column names
#' result3 <- Unite_confint(data, "Original", "sig")
#' print(result3)
#'
#' # Unite using column names with a new column name
#' result4 <- Unite_confint(data, "Original", "sig", "New")
#' print(result4)
#' }
Unite_confint <- function(data, col1, col2, Col = NULL,
                           sep = ", ", remove = TRUE, left = "[", right = "]") {
  # Check if col1 and col2 are numeric
  if (is.numeric(col1) && is.numeric(col2)) {
    col1 <- names(data)[col1]
    col2 <- names(data)[col2]
  }

  if (is.null(Col)) {
    # Create a new column name if Col is not provided
    new_col <- paste0(left, col1, sep, col2, right)
  } else {
    # Use the provided Col name
    new_col <- Col
  }

  # Combine the columns with the specified separators
  data <- tidyr::unite(data, col = {{new_col}},
                       all_of(c(col1, col2)), sep = sep, remove = remove)

  # Add left and right to the combined column
  data[[new_col]] <- paste0(left, data[[new_col]], right)

  return(data)
}
