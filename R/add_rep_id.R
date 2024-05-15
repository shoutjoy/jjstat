#' Create an ordered variable for overlapping groups
#'
#' @param data data.frame
#' @param grp select colname or number
#'
#' @return create data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data <- data.frame(
#'   weight = c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14,
#'              4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69,
#'              6.31, 5.12, 5.54, 5.50, 5.37, 5.29, 4.92, 6.15, 5.80, 5.26),
#'   group = c(rep("ctrl", 10), rep("trt1", 10), rep("trt2", 10))
#' )
#' data
#'
#' #generate id from Colname
#' add_rep_id(data, "group")
#'
#' #generate id from Col number
#' add_rep_id(data, 2)
#'
#' #data spread wide
#' data%>%add_rep_id("group")%>%as_trt("id") %>%wide_df("group", "weight")
#'
#'
#' data%>%
#'   add_rep_id("group")%>%
#'   as_trt("id") %>%
#'   wide_df("group", "weight")

#'
#' }
#'
#'
add_rep_id <- function(data, grp) {
  if (is.numeric(grp)) {
    grp <- colnames(data)[grp]
  }

  data$id <- ave(rep(1, nrow(data)), data[[grp]], FUN = seq_along)

  return(data)
}
