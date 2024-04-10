#' Find and round numeric variables only
#'
#' @param data data.frame
#' @param digits default 2
#' @param exclude exclude variable not adapted
#' @param type data type tibble or data.frame , matrix
#'
#' @return rounding data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # vector
#' Round(12.345654)
#'
#' ##data.frame
#' mtcars%>% Round(1)
#'
#'  ##
#' dff = data.frame(A = c(1.23456, 2.34567, 3.45678),
#'                  B = c(4.56789, 5.67890, 6.78901),
#'                 C = c("a", "b", "c"))
#'  ## rounding 1
#'  dff%>%Round(1)
#'
#' #exclue variable B
#'  dff%>%Round(1, exclude ="B")
#'
#' }
Round <- function(data, digits=2, exclude = NULL, type= "tibble"){
  if(is.data.frame(data)){

    original_order <- colnames(data)
    # Rounding by pulling out excluded variables separately
    excluded_data <- data%>%select(all_of(exclude))
    rounded_data <- data%>%select(- all_of(exclude))

    rounded_data <- rounded_data %>% mutate_if(is.numeric, round, digits)
    # Insert excluded variables in their original column order

    data <- cbind(rounded_data, excluded_data)
    data <- data[, original_order]  # Preserve column order


  }else{
    data <- sapply(data,
                   function(x) {if(is.numeric(x)){round(x, digits)}})
  }

  tibble = tibble::tibble(data)
  data.frame = as.data.frame(data)
  matrix = as.matrix(data)

  switch(type,
         tibble = tibble,
         matirx = matrix,
         data.frame = data.frame)

}
