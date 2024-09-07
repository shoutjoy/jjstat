
#' path create
#' @param from var1
#' @param to var2
#' @export
path <- function(from, to) {
  return(list(from = from,
              to = to))
}



#' plspm path create
#'
#' @param rownames rowname
#' @param colnames colname
#' @param relationship input path
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' edu_path <- plspm_path(
#'   rownames = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   colnames = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   relationship = list(
#'     list(from = "Value",
#'          to = c("Support", "Advising", "Tutoring")),
#'     list(from = "Satisfaction",
#'          to = c("Support", "Advising", "Tutoring","Value")),
#'     list(from = "Loyalty",
#'          to =c("Satisfaction"))
#'   )
#' )
#' edu_path
#'
#'
#' edu_path <- plspm_path(
#'   rownames = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   colnames = c("Support", "Advising", "Tutoring", "Value", "Satisfaction", "Loyalty"),
#'   relationship = list(
#'
#'     path(form = "Support", to ="Advising"),
#'     path(form = "Tutoring", to ="Advising"),
#'     path(from = "Value",
#'          to = c("Support", "Advising", "Tutoring")),
#'     path(from = "Satisfaction",
#'          to = c("Support", "Advising", "Tutoring","Value")),
#'     path (from = "Loyalty",
#'          to =c("Satisfaction"))
#'   )
#' )
#' edu_path
#' #'
#'
#' plspm_paths(
#'   relationship = list(
#'     list(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     list(from="EXPE", to=c("QUAL","VAL","SAT")),
#'     list(from="QUAL", to=c("VAL","SAT")),
#'     list(from="VAL", to="SAT"),
#'     list(from="SAT", to="LOY")
#'   )
#' )
#'
#' # 결과 출력
#'
#' sat_path1 = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY"))
#' )
#' sat_path1
#'
#' #'
#'
#' sat_path <- plspm_paths(relationship = list(
#'   path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'   path(from="EXPE", to=c("QUAL","VAL","SAT")),
#'   path(from="QUAL", to=c("VAL","SAT")),
#'   path(from="VAL", to="SAT"),
#'   path(from="SAT", to="LOY")
#' ) )
#'
#' sat_path <- plspm_paths(relationship = list(
#'   path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'   path(from="EXPE", to=c("QUAL","VAL","SAT")),
#'   path(from="QUAL", to=c("VAL","SAT")),
#'   path(from="VAL", to="SAT"),
#'   path(from="SAT", to="LOY")
#' ),
#' row_names =  c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#' )
#'
#' # 결과 출력
#' print(sat_path)
#'
#'
#' }
plspm_paths <- function(row_names = NULL,
                        col_names = NULL,
                        relationship = NULL) {

  # Check if relationship is provided
  if (is.null(relationship)) {
    stop("relationship must be provided")
  }

  # Extract unique names from relationship if row_names and col_names are NULL
  if (is.null(row_names) & is.null(col_names)) {
    from_names <- sapply(relationship, function(x) x$from)
    to_names <- unlist(sapply(relationship, function(x) x$to))
    unique_names <- unique(c(from_names, to_names))
    col_names <- unique_names
    row_names <- unique_names
  } else if (is.null(col_names)) {
    col_names <- row_names
  }

  num_rows <- length(row_names)
  num_cols <- length(col_names)

  path_matrix <- matrix(0,
                        nrow = num_rows,
                        ncol = num_cols,
                        dimnames = list(row_names, col_names))

  if (!is.null(relationship)) {
    for (path in relationship) {
      from <- path$from
      to <- unlist(path$to)
      path_matrix[from, to] <- 1
    }
  }

  return(t(path_matrix))
}
# plspm_paths <- function(row_names = NULL,
#                         col_names = NULL,
#                         relationship = NULL) {
#
#   if(is.null(col_names)){
#     col_names <- row_names
#     num_rows <- length(row_names)
#     num_cols <- length(col_names)
#
#   }else{
#     num_rows <- length(row_names)
#     num_cols <- length(col_names)
#   }
#
#   path_matrix <- matrix(0,
#                         nrow = num_rows,
#                         ncol = num_cols,
#                         dimnames = list(row_names, col_names))
#
#   if (!is.null(relationship)) {
#     for (path in relationship) {
#       from <- path$from
#       to <- path$to
#       path_matrix[from, to] <- 1
#     }
#   }
#
#   return(t(path_matrix))
# }


