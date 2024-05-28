#' plspm_mat_lavaan syntax
#'
#' @param data matrix
#' @param free TRUE is free parameter
#' @param hypo hypothesis paste
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
#' #' #'
#' data <- matrix(c(0, 1, 0, 1, 0, 1, 1, 0, 0), nrow=3, byrow=TRUE)
#' rownames(data) <- c("자기효능감", "진로동기", "진로태도")
#' colnames(data) <- c("진로동기", "진로태도", "진로준비")
#' data
#' # hypo=TRUE인 경우
#' cat(plspm_mat_lav(data, hypo=TRUE))
#' # 진로동기 ~ H1*자기효능감
#' # 진로태도 ~ H2*자기효능감
#' # 진로준비 ~ H3*자기효능감
#' # 진로준비 ~ H4*진로동기
#' # 진로준비 ~ H5*진로태도
#'
#' # free=TRUE, hypo=FALSE인 경우
#' cat(plspm_mat_lav(data, free=TRUE, hypo=FALSE))
#' # 진로동기 ~ 자기효능감
#' # 진로태도 ~ 자기효능감
#' # 진로준비 ~ 자기효능감
#' # 진로준비 ~ 진로동기
#' # 진로준비 ~ 진로태도
#'
#' # free=FALSE인 경우
#' cat(plspm_mat_lav(data, free=FALSE, hypo=FALSE))
#' # 진로동기 ~ 1*자기효능감
#' # 진로태도 ~ 1*자기효능감
#' # 진로준비 ~ 1*자기효능감
#' # 진로준비 ~ 1*진로동기
#' # 진로준비 ~ 1*진로태도
#'
#' }
#'
plspm_mat_lav <- function(data, free=FALSE, hypo=FALSE) {
  # hypo가 TRUE인 경우 free도 TRUE로 설정
  if (hypo) {
    free <- TRUE
  }

  # 행렬의 행 이름과 열 이름 가져오기
  if (is.null(rownames(data)) & is.null(colnames(data))) {
    rownames(data) <- paste0("eta", 1:nrow(data))
    colnames(data) <- paste0("eta", 1:ncol(data))
  } else if (is.null(rownames(data)) & !is.null(colnames(data))) {
    rownames(data) <- colnames(data)
  } else if (!is.null(rownames(data)) & is.null(colnames(data))) {
    colnames(data) <- rownames(data)
  }

  data <- t(data)
  node_names <- rownames(data)
  col_names <- colnames(data)

  # generate model
  model <- ""
  hypo_count <- 1
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (data[i, j] != 0) {
        if (free) {
          if (hypo) {
            model <- paste0(model, col_names[j], " ~ H", hypo_count, "*", node_names[i], "\n")
            hypo_count <- hypo_count + 1
          } else {
            model <- paste0(model, col_names[j], " ~ ", node_names[i], "\n")
          }
        } else {
          model <- paste0(model, col_names[j], " ~ ", data[i, j], "*", node_names[i], "\n")
        }
      }
    }
  }

  return(model)
}
