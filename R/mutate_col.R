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
#'   age = c("20대", "20대", "20대", "30대",
#'   "30대", "30대", "40대", "40대", "40대"),
#'   manufacturer = c("google", "apple", "samsung",
#'   "google", "apple", "samsung", "google",
#'    "apple", "samsung"),
#'   values = c(794, 804, 1243, 987, 1055,
#'   2517, 1528, 1519, 2255)
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
#'
#'
#' # second data
#' data2 <- data.frame(
#'   weight = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14,
#'              4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69,
#'              6.31, 5.12, 5.54, 5.50, 5.37, 5.29, 4.92, 6.15, 5.80, 5.26),
#'   ids = c(paste0("C", 1:10), paste0("T",1:10), paste0("S", 1:10))
#' )
#' data2 %>%mutate_col(group= .$weight, col=2)
#'
#' }
#'
mutate_col <- function(data, ..., col = ncol(data)) {

  # Create a new column
  new_col <- names(list(...))[1]  # Extract names for new columns
  new_col_values <- list(...)[[1]]  # Extract values in a new column

  # Add new column using mutate
  data[[new_col]] <- new_col_values
  # data <- data %>% dplyr::mutate(!!new_col := new_col_values)

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
   return(data)

}
