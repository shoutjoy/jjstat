#'  Reorder rows in a dataframe with a given row number
#'
#' @param data data.
#' @param ... row order
#'
#' @return Reordered data
#' @export
#'
#' @examples
#' \dontrun{
#' Create a dataframe
#' df <- data.frame(
#'   response = c("그렇다", "그렇지않다", "매우그렇다", "보통이다", "전혀그렇지않다"),
#'   Freq = c(5, 13, 4, 14, 4)
#' )
#' df
#' # Call a function to reorder rows
#' reorder_row(df, 5, 2, 4, 1,3)  # Put the row numbers in order.
#'
#' #data process
#' data_long <- tribble(
#'   ~response, ~freq,
#'   "전혀그렇지않다", 1,
#'   "그렇지않다", 2,
#'   "보통이다", 3,
#'   "그렇다", 2,
#'   "매우그렇다", 0,
#'   "전혀그렇지않다", 0,
#'   "그렇지않다", 4,
#'   "보통이다", 4,
#'   "그렇다", 0,
#'   "매우그렇다", 0,
#'   "전혀그렇지않다", 1,
#'   "그렇지않다", 3,
#'   "보통이다", 4,
#'   "그렇다", 0,
#'   "매우그렇다", 0,
#'   "전혀그렇지않다", 1,
#'   "그렇지않다", 4,
#'   "보통이다", 3,
#'   "그렇다", 0,
#'   "매우그렇다", 0
#' )
#' data_long <- data_long %>%as_trt("response")
#' data_long
#'
#' data_long %>%add_rows_freq() %>% rowid_to_column("person")%>%
#'   select(response)%>%table()%>%dall()%>%
#'   reorder_row(5,2,4,1,3)
#'
#' }
#'
reorder_row <- function(data, ...) {
  # Reorder rows in a dataframe with a given row number
  data <- data[c(...), ]
  return(data)
}
