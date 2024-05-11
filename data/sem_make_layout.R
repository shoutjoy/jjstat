#' SEM make layout matrix
#'
#' @param nc default 6
#' @param nr row NULL
#'
#' @return matrix
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' make_layout(nc=7)
#' make_layout(nc=7, nr=6)
#' make_layout(3,6)
#'
#' lay1 = make_layout(6)
#' lay1
#'
#' edit_layout(lay1)
#' lay1 %>%edit_layout()
#' lay2 = lay1 %>%edit_layout()
#' lay2
#' }
#'
make_layout = function(nc = 10, nr=NULL){
  if(is.null(nr)){
    layout = matrix(c(rep("", nc^2) ), ncol = nc )
  }else{
    layout = matrix(c(rep("", nc^2) ), ncol = nc , nrow = nr )
  }

  rownames(layout)= paste0("row",1:nrow(layout))
  layout = edit(layout)
  layout
}
#' edit layout matrix
#'
#' @param data input data
#'
#' @return matrix
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' make_layout(nc=7)
#' make_layout(nc=7, nr=6)
#' make_layout(3,6)
#'
#' lay1 = make_layout(6)
#' lay1
#'
#' edit_layout(lay1)
#' lay1 %>%edit_layout()
#' lay2 = lay1 %>%edit_layout()
#' lay2
#' }
edit_layout = function(data){
  layout = edit(data)
  layout
}
