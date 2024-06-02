#' 같은 것이 반복되면 ""처리
#'
#' @param data data
#' @param exclude not adapt col
#'
#' @return data ressult
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' data <- data.frame(
#'   Var = c("a", "a", "a", "a", "a", "a", "a", "b",
#'   "b", "b", "b", "b", "c", "c", "c", "c"),
#'   alpha_95.CI = c("alpha = 0.855, 95%CI[0.8, 0.9]",
#'   "alpha = 0.855, 95%CI[0.8, 0.9]",
#'                   "alpha = 0.855, 95%CI[0.8, 0.9]",
#'                   "alpha = 0.855, 95%CI[0.8, 0.9]",
#'                   "alpha = 0.855, 95%CI[0.8, 0.9]",
#'                   "alpha = 0.855, 95%CI[0.8, 0.9]",
#'                   "alpha = 0.855, 95%CI[0.8, 0.9]",
#'                   "alpha = 0.805, 95%CI[0.74, 0.86]",
#'                   "alpha = 0.805, 95%CI[0.74, 0.86]",
#'                   "alpha = 0.805, 95%CI[0.74, 0.86]",
#'                   "alpha = 0.805, 95%CI[0.74, 0.86]",
#'                   "alpha = 0.805, 95%CI[0.74, 0.86]",
#'                   "alpha = 0.534, 95%CI[0.36, 0.67]",
#'                   "alpha = 0.534, 95%CI[0.36, 0.67]",
#'                   "alpha = 0.534, 95%CI[0.36, 0.67]",
#'                   "alpha = 0.534, 95%CI[0.36, 0.67]"),
#'   subfactor = c("B02", "B14", "B10", "B04", "B01", "B15",
#'    "B03", "B09", "B25", "B07", "B06", "B05", "B17",
#'    "B18", "B21", "B22"),
#'   cronbach_alpha = c(0.8175800, 0.8192332,
#'   0.8375942, 0.8163753, 0.8505183, 0.8488448,
#'    0.8445658, 0.7607713, 0.7494337,
#'                      0.7534147, 0.7620378,
#'                       0.8099670, 0.4376783,
#'                        0.5111512, 0.4434714, 0.4554295)
#' )
#'
#' # Apply the function to the data
#' nice_table(data)
#'
#' }
#'
#'
nice_table <- function(data, exclude=NULL) {
  # Convert exclude to a character vector of column names if not already
  if (!is.null(exclude)) {
    if (is.numeric(exclude)) {
      exclude <- names(data)[exclude]
    } else if (is.character(exclude)) {
      if (any(grepl(":", exclude))) {
        exclude <- unlist(lapply(exclude, function(col) {
          if (grepl(":", col)) {
            col_range <- unlist(strsplit(col, ":"))
            start_col <- which(names(data) == col_range[1])
            end_col <- which(names(data) == col_range[2])
            return(names(data)[start_col:end_col])
          } else {
            return(col)
          }
        }))
      }
    }
  } else {
    exclude <- character(0)
  }

  # Loop through each column in the data frame
  for (col in names(data)) {
    # Skip the columns that are in the exclude list
    if (col %in% exclude) next

    # Initialize the previous value variable
    prev_value <- ""

    # Apply the function to each element in the current column
    data[[col]] <- sapply(data[[col]], function(x) {
      if (x == prev_value) {
        return("")
      } else {
        prev_value <<- x
        return(x)
      }
    })
  }

  return(data)
}
