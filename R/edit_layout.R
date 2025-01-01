#' Edit an Existing Layout Matrix
#'
#' @param data A matrix or data frame to be edited interactively.
#'
#' @return A matrix or data frame representing the edited layout.
#' @export
#'
#' @examples
#' \dontrun{
#' layout <- matrix(1:9, ncol = 3)
#' edited_layout <- edit_layout(layout)
#' print(edited_layout)
#' }
edit_layout <- function(data) {
  layout <- edit(data)
  return(layout)
}
