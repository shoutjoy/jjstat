#' Create, modify, and delete columns col position
#'
#' @param data data.frame
#' @param ... creat name
#' @param col position
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' fm <- data.frame(
#'   age = c("20대", "20대", "20대", "30대", "30대", "30대", "40대", "40대", "40대"),
#'   manufacturer = c("google", "apple", "samsung", "google", "apple", "samsung", "google", "apple", "samsung"),
#'   values = c(794, 804, 1243, 987, 1055, 2517, 1528, 1519, 2255)
#' )
#' fm
#'
#' #data
#' fm %>% mutate_col(gender= rep("a",9))
#' fm %>% mutate_col(gender= rep("a",9), col = 2)
#'
#' #position
#' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 2)
#' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 1)
#' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 3)
#' fm %>% mutate_col(gender = rep("female", nrow(fm)), col = 4)
#'
#' #Build and adjust with data from within
#' fm %>% mutate(gender = age)
#' fm %>% mutate_col(age_int = substring(fm$age,1,2))
#' fm %>% mutate_col(gender = paste(fm$age,fm$values,sep="//"))
#'
#' fm %>% mutate_col(gender = paste(.$age,.$values,sep="//"), col=2)
#' # Double data generation
#' fm %>%
#'   mutate_col(insert_2 = rep("female", nrow(fm)), col = 2) %>%
#'   mutate_col( insert_4 = rep("a",9), col = 4)
#' }
#'
mutate_col <- function(data, ..., col = ncol(data)) {
  attach(data)
  # Create a new column
  new_col <- names(list(...))[1]  # Extract names for new columns
  new_col_values <- list(...)[[1]]  # Extract values in a new column
  data[[new_col]] <- new_col_values

  # Adjust the position of a column
  col_names <- colnames(data)
  if (col == 1) {
    col_names <- c(new_col, col_names)
  } else if (col == ncol(data)) {
    col_names <- c(col_names, new_col)
  } else {
    col_names <- c(col_names[1:(col - 1)], new_col, col_names[col:ncol(data)])
  }
  #Remove last column autogeneration
  col_names = col_names[-(length(col_names))]
  data <- data[, col_names]
  #result
  detach(data)
  return(data)

}
