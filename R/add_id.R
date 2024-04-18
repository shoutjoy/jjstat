#' Add row id
#'
#' @param data data.frame
#' @param sel select column
#' @param each each =TRUE then 111222....FALSE 123123
#'
#' @return add column data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mtcars %>% mutate(MPG= which(colnames(mtcars) =="mpg"))%>%add_id()
#' mtcars %>% mutate(MPG= which(colnames(mtcars) =="mpg"))%>%add_id("cyl")
#' mtcars[1:3, 1:3] %>% add_id()
#' mtcars[1:3, 1:3] %>% add_id("cyl")
#' mtcars[1:4, 1:4] %>% to_long() %>% add_id()
#' mtcars[1:4, 1:4] %>% to_long() %>% add_id(each=FALSE)
#'
#'
#'
#' }
add_id = function(data, sel = "names", each = TRUE) {
  # Make sure the column name exists
  if (!(sel %in% colnames(data))) {
    cat(paste0("Column '", sel, "' does not exist in the dataset.
           Please select a valid column.
           (JH Park PhD).\n\n"))
    res <- data %>% mutate(id = 1:nrow(data))

  } else {

    # Identify the number of repeating rules in the names column
    num_patterns <- length(unique(data[[sel]]))
    Each <- nrow(data) / num_patterns

    # Match the number of IDs to the total number of rows
    total_rows <- nrow(data)
    if (total_rows != length(rep(1:Each, each = num_patterns))) {

      cat("\n\n","This is not long-format So, we added a row id(JH Park PhD).","\n\n")

      res <- data %>% mutate(id = 1:nrow(data))
    } else {
      #option
      if (each) {
        res <- data %>%
          mutate(id = rep(1:Each, each = num_patterns))
      } else {
        res <- data %>%
          mutate(id = rep(1:Each, num_patterns))
      }
    }
  }
  return(res)
}
