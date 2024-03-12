#' Find and round numeric variables only
#'
#' @param data data.frame
#' @param digits default 2
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
#' }
Round <- function(data, digits=2){
  if(is.data.frame(data)){
    # data = data%>%rownames_to_column( "rownames")

    data = data %>% dplyr::mutate_if(is.numeric, round, digits)

    #  data = data%>% column_to_rownames("rownames")

  }else{
    data <- sapply(data,
                   function(x) {if(is.numeric(x)){round(x, digits)}})
  }
  data
}
