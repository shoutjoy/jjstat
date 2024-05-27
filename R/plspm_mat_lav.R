#' plspm_mat_lavaan syntax
#'
#' @param data matrix
#' @param free TRUE is free parameter
#'
#' @return syntax lavaan
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # Example data for testing
#' data_b <- matrix(c(
#'   0.000, 0.000, 0.000, 0.000, 0.000, 0,
#'   0.471, 0.000, 0.000, 0.000, 0.000, 0,
#'   0.000, 0.834, 0.000, 0.000, 0.000, 0,
#'   0.000, 0.046, 0.701, 0.000, 0.000, 0,
#'   0.245, -0.017, 0.222, 0.527, 0.000, 0,
#'   0.182, 0.000, 0.000, 0.000, 0.628, 0
#' ), nrow = 6, byrow = TRUE,
#' dimnames=list(
#'   c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY"),
#'   c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#' ))
#' plspm_mat_lav(data_b)%>%cat()
#' plspm_mat_lav(data_b)%>%
#'   diagram_model(whatLabels = "est", layout="tree2", nDigits=3,
#'                 edge.label.position=0.65, rotation=1)
#'
#'
#' data_r <- matrix(c(
#'   0.000, 0.000, 0.000, 0.000, 0.000, 0,
#'   0.471, 0.000, 0.000, 0.000, 0.000, 0,
#'   0.000, 0.834, 0.000, 0.000, 0.000, 0,
#'   0.000, 0.046, 0.701, 0.000, 0.000, 0,
#'   0.245, -0.017, 0.222, 0.527, 0.000, 0,
#'   0.182, 0.000, 0.000, 0.000, 0.628, 0
#' ), nrow = 6, byrow = TRUE,
#' dimnames=list(c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")))
#' data_r
#'
#' plspm_mat_lav(data_r)%>%cat()
#'
#' plspm_mat_lav(data_r)%>%
#'   diagram_model(whatLabels = "est", layout="tree2", nDigits=3,
#'                 edge.label.position=0.65, rotation=1)
#'
#' data_n <- matrix(c(
#'   0.000, 0.000, 0.000, 0.000, 0.000, 0,
#'   0.471, 0.000, 0.000, 0.000, 0.000, 0,
#'   0.000, 0.834, 0.000, 0.000, 0.000, 0,
#'   0.000, 0.046, 0.701, 0.000, 0.000, 0,
#'   0.245, -0.017, 0.222, 0.527, 0.000, 0,
#'   0.182, 0.000, 0.000, 0.000, 0.628, 0
#' ), nrow = 6, byrow = TRUE)
#' data_n
#' plspm_mat_lav(data_n)%>%cat()
#' plspm_mat_lav(data_n)%>%
#'   diagram_model(whatLabels = "est", layout="tree2", nDigits=3,
#'                 edge.label.position=0.65, rotation=1)
#'
#'
#'
#' data_null <- matrix(c(
#'   0.000, 0.000, 0.000, 0.000, 0.000, 0,
#'   "H1", 0.000, 0.000, 0.000, 0.000, 0,
#'   0.000, "H2", 0.000, 0.000, 0.000, 0,
#'   0.000, "H3", "H4", 0.000, 0.000, 0,
#'   "H5", "H6", "H7", "H8", 0.000, 0,
#'   "H9", 0.000, 0.000, 0.000, "H10", 0
#' ), nrow = 6, byrow = TRUE,
#' dimnames=list(
#'   c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY"),
#'   c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#' ))
#'
#' plspm_mat_lav(data_null)%>%cat()
#'
#' plspm_mat_lav(data_null)%>%
#'   diagram_model(whatLabels = "model", layout="tree2", nDigits=3,
#'                 edge.label.position=0.65, rotation=1)
#'
#' #'
#' lay_p = matrix(c('IMA', NA, NA, NA, NA, NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'EXP', NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'QUA',
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, 'VAL', NA, NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'SAT', NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'LOY',
#'                  NA, NA, NA, NA, NA, NA, NA, NA),
#'                nrow = 10, ncol = 10, byrow = FALSE)
#' lay_p
#'
#' # Using digaram_model
#'
#' satpls$path_coefs %>% plspm_mat_lav() %>%
#'   diagram_model(layout="tree2", whatLabels = "est",
#'                 rotation =1, edge.label.position=0.6)
#'
#' satpls$path_coefs %>% plspm_mat_lav() %>%
#'   diagram_model(layout="tree2", whatLabels = "est",
#'                 rotation =1, edge.label.position=0.6,
#'                 edgeLabels=satpls_fit$inner_model %>%plspm_edge_values())
#' #'
#' #'
#'
#'
#' }
#'
plspm_mat_lav <- function(data, free=FALSE) {
  # 행렬의 행 이름과 열 이름 가져오기
  if(is.null(rownames(data)) & is.null(colnames(data)) ){
    rownames(data) = paste0("eta", 1:nrow(data))
    colnames(data) = paste0("eta", 1:ncol(data))

  }else if(is.null(rownames(data)) & !is.null(colnames(data))){
    # rownames(data) = paste0("eta", 1:ncol(data))
    rownames(data) = colnames(data)

  }else if(!is.null(rownames(data)) & is.null(colnames(data))){
    colnames(data) = rownames(data)
  }


  data <- t(data)
  node_names <- rownames(data)
  col_names <- colnames(data)

  # generate model
  if(free){

    model <- ""
    for (i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
        if (data[i, j] != 0) {
          model <- paste0(model, col_names[j], " ~ ",
                           node_names[i], "\n")
        }
      }
    }

  }else{
  model <- ""
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (data[i, j] != 0) {
        model <- paste0(model, col_names[j], " ~ ",
                        data[i, j], "*", node_names[i], "\n")
      }
    }
  }

  }###

  return(model)

}
