#' PLS-PMinteraction effect  Partial Least Squares Path Modeling
#'
#' @param Data matrix or data frame containing the manifest variables.
#' @param path_matrix A square (lower triangular) boolean matrix
#' representing the inner model (i.e. the path relationships between latent variables).
#' @param blocks 	list of vectors with column indices
#' or column names from Data indicating the sets of
#' manifest variables forming each block (i.e.
#' which manifest variables correspond to each block).
#' @param modes optional argument for runing the non-metric approach;
#' it is a list of string vectors indicating the type of measurement
#' scale for each manifest variable specified in blocks. scaling must be
#' specified when working with non-metric variables. Possible values:
#' "num" (linear transformation, suitable for numerical variables),
#' "raw" (no transformation), "nom" (non-monotonic transformation,
#' suitable for nominal variables), and "ord" (monotonic transformation,
#' suitable for ordinal variables).
#' @param scaling character vector indicating the type of measurement f
#' or each block. Possible values are: "A", "B", "newA", "PLScore",
#' "PLScow". The length of modes must be equal to the length of blocks.
#' @param scheme 	string indicating the type of inner weighting scheme.
#'  Possible values are "centroid", "factorial", or "path".
#' @param scaled standardized. Only used when scaling = NULL.
#' When (TRUE, data is scaled to standardized values (mean=0 and variance=1).
#'  The variance is calculated dividing by N instead of N-1).
#' @param tol decimal value indicating the tolerance criterion
#' for the iterations (tol=0.000001). Can be specified between 0 and 0.001.
#' @param maxiter integer indicating the maximum number of iterations
#'  (maxiter=100 by default). The minimum value of maxiter is 100.
#' @param plscomp optional vector indicating the number of PLS components
#' (for each block) to be used when handling non-metric data
#' (only used if scaling is provided)
#' @param boot.val 	whether bootstrap validation should be performed.
#'  (FALSE by default).
#' @param br number bootstrap resamples. Used only when boot.val=TRUE. When boot.val=TRUE, the default number of re-samples is 100.
#' @param dataset whether the data matrix used in the computations should be retrieved (TRUE by default).
#'
#' @return resutl
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#'
#' data(satisfaction)
#' sat_path3 = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY"))
#' )
#'
#' sat_path2
#'
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#'
#' sat_blocks1
#'
#' # vector of modes (reflective indicators):auto
#' sat_mod = rep("A", 6)
#'
#' resinter = plspm_interaction(Data=satisfaction,
#'                              sat_path2, blocks = sat_blocks1, modes= sat_mod,
#'                              interactionTerm = list(list(from = "IMAG*EXPE", to = "LOY"),
#'                                                     list(from = "IMAG*QUAL", to = "LOY")))
#' resinter%>%summary()
#' x11()
#' plot(resinter)
#'
#' # resinter %>%plspm_boot_effect()%>%cut_print(rm_row=TRUE)
#' resinter %>%plspm_effect()%>%cut_print(rm_row=TRUE)
#'
#' plspm_paths_sig(resinter, unite=TRUE)
#' plspm_paths_sig_plot(resinter)
#' plspm_path_coefs_plot(resinter)
#'
#' plspm_semPaths2(resinter, structural = T)

#'
#' }
#'
#'
plspm_interaction = function(Data, path_matrix, blocks,
                             modes = rep("A", ncol(path_matrix)),
                             interactionTerm = NULL, inter_modes=NULL,
                             scaling = NULL, scheme = "centroid", scaled = TRUE,
                             tol = 1e-06, maxiter = 100, plscomp = NULL,
                             boot.val = TRUE, br = 500, seed=NULL,
                             dataset = TRUE){


  # Tow stage interaction
  if(!is.null(interactionTerm)){
    intresult  <- plspm::plspm(Data = Data, path_matrix = path_matrix,
                               blocks = blocks, modes = modes,
                               scaling = scaling, scheme = scheme, scaled = scaled,
                               tol = tol, maxiter = maxiter, plscomp = plscomp,
                               boot.val = FALSE, br = br, dataset = dataset)


    # Paths
    int_Path_Matrix = plspm_add_inter_paths(intresult$model$IDM, interactionTerm)%>%
      as.matrix()

    int_Blocks = plspm_auto_blocks(int_Path_Matrix)

    if(is.null(inter_modes)){
      int_Modes = rep("A", ncol(int_Path_Matrix))
    }else{
      int_Modes = inter_modes
    }

    int_Data = plspm_auto_genData(intresult$scores ,interactionTerm )


    RES <- plspm::plspm(Data = int_Data, path_matrix = int_Path_Matrix,
                        blocks = int_Blocks, modes = int_Modes,
                        scaling = scaling, scheme = scheme, scaled = scaled,
                        tol = tol, maxiter = maxiter, plscomp = plscomp,
                        boot.val = TRUE, br = br, dataset = dataset)


  }
  RES
}


#' plspm_add_inter_paths
#'
#' @param data path matrix data
#' @param paths add path
#'
#' @return mat
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' data <- data.frame(
#'   IMAG = c(0, 1, 0, 0, 1, 1),
#'   EXPE = c(0, 0, 1, 1, 1, 0),
#'   QUAL = c(0, 0, 0, 1, 1, 0),
#'   VAL  = c(0, 0, 0, 0, 1, 0),
#'   SAT  = c(0, 0, 0, 0, 0, 1),
#'   LOY  = c(0, 0, 0, 0, 0, 0),
#'   row.names = c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#' )
#'
#' # 함수 호출 예시
#' plspm_add_inter_paths(data, list(
#'   int1 = list(from = "IMAG*EXPE", to = "LOY"),
#'   int2 = list(from = "IMAG*QUAL", to = "LOY")
#' ))
#'
#' plspm_add_inter_paths(data, list(
#'   list(from = "IMAG*EXPE", to = "LOY"),
#'   list(from = "IMAG*QUAL", to = "LOY")
#' ))
#'
#'
#' }
#'
#'
plspm_add_inter_paths <- function(data, paths) {
  # 데이터 행렬을 복사하여 변경
  new_data <- data

  # 경로 추가 작업
  for (i in seq_along(paths)) {
    path <- paths[[i]]

    if (is.null(names(paths))) {
      # 이름이 없는 경우 (경로 이름 생성)
      from_vars <- unlist(strsplit(path$from, "\\*"))
      int_name <- paste0("int_", substr(from_vars[1], 1, 2), substr(from_vars[2], 1, 2))
    } else {
      # 이름이 있는 경우
      int_name <- names(paths)[i]
    }

    # 새로운 경로가 있는 열과 행을 추가
    if (!int_name %in% colnames(new_data)) {
      new_col <- setNames(data.frame(matrix(0, nrow(new_data), 1)), int_name)
      new_row <- setNames(data.frame(matrix(0, 1, ncol(new_data) + 1)), c(int_name, colnames(new_data)))
      rownames(new_row) <- int_name
      new_data <- cbind(new_data, new_col)
      new_data <- rbind(new_data, new_row)
    }

    # 'to' 변수의 행에 경로 추가
    new_data[path$to, int_name] <- 1
  }

  # 열의 순서를 재정렬
  new_col_order <- c(setdiff(colnames(new_data), colnames(data)), colnames(data))
  new_data <- new_data[, new_col_order]

  # 행의 순서를 재정렬
  new_row_order <- c(setdiff(rownames(new_data), rownames(data)), rownames(data))
  new_data <- new_data[new_row_order, ]

  return(new_data)
}



#' plspm_auto_blocks
#'
#' @param mat path matrix
#' @param to_lower Change to lowercase
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' #'
#' mat <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0,
#'                 0, 0, 1, 1, 1, 0, 0, 0,
#'                 0, 0, 0, 1, 0, 0, 0, 0,
#'                 0, 0, 0, 1, 0, 0, 0, 0,
#'                 0, 0, 0, 0, 0, 1, 0, 0,
#'                 0, 0, 0, 0, 0, 0, 1, 1,
#'                 0, 0, 0, 0, 0, 0, 0, 0,
#'                 0, 0, 0, 0, 0, 0, 0, 0),
#'               nrow = 8, byrow = TRUE)
#'
#' rownames(mat) <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY", "int_IMEX", "int_IMQU")
#' colnames(mat) <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY", "int_IMEX", "int_IMQU")
#' mat
#' # 함수 호출
#' plspm_auto_blocks(mat)
#'
#' plspm_add_inter_paths(satpls_boot$model$IDM, list(
#'   list(from = "IMAG*EXPE", to = "LOY"),
#'   list(from = "IMAG*QUAL", to = "LOY")
#' ))%>%plspm_auto_blocks()

#'
#' }
#'
plspm_auto_blocks <- function(mat, to_lower=TRUE) {
  # 행렬의 행과 열 이름 추출
  row_names <- rownames(mat)
  col_names <- colnames(mat)

  # 리스트 생성
  result <- list()
  for (i in 1:length(row_names)) {
    if (to_lower) {
      result[[col_names[i]]] <- tolower(row_names[i])
    } else {
      result[[col_names[i]]] <- row_names[i]
    }
  }

  return(result)
}


#' plspm_auto_genData
#'
#' @param data data.frame
#' @param interactions interaction term list data
#' @param to_lower  Change to lowercase
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' data1 <- data.frame(
#'   IMAG = c(-0.06448267, 0.83858784, 0.47542512, 0.42690176, 1.04176442, 0.27894142),
#'   EXPE = c(0.3442433, 0.7974660, 0.5531210, 0.1281051, 0.4492096, 0.6914954),
#'   QUAL = c(-0.5254008, 0.3971273, 0.1642778, -0.6892055, 0.7255953, -0.2088824),
#'   VAL = c(-0.07103969, 0.44321149, 0.56088128, -0.81127296, 1.21587873, -0.29482532),
#'   SAT = c(-0.20583024, 0.36908558, 0.83019100, -0.05045141, 0.53174538, -0.20212349),
#'   LOY = c(0.1664431, 0.4924859, 0.8117161, -0.7969487, 0.7348195, 0.1714641)
#' )
#'
#' # 함수 실행 예시
#' plspm_auto_genData(data1, list(
#'   int1 = list(from = "IMAG*EXPE", to = "LOY"),
#'   int2 = list(from = "IMAG*QUAL", to = "LOY")
#' ))
#'
#' # 함수 실행 예시
#' plspm_auto_genData(satpls_boot$scores , list(
#'   int1 = list(from = "IMAG*EXPE", to = "LOY"),
#'   int2 = list(from = "IMAG*QUAL", to = "LOY")
#' )) %>%head()
#'
#' # 함수 실행 예시
#' plspm_auto_genData(satpls_boot$scores , list(
#'   list(from = "IMAG*EXPE", to = "LOY"),
#'   list(from = "IMAG*QUAL", to = "LOY")
#' ))%>%head()
#' #'
#' }
#'
#'
plspm_auto_genData <- function(data, interactions, to_lower=TRUE) {
  data <- data.frame(data)
  new_data <- data

  for (i in seq_along(interactions)) {
    interaction <- interactions[[i]]

    from_split <- strsplit(interaction$from, "\\*")[[1]]
    if (length(from_split) != 2) {
      stop("Interaction format must be 'VAR1*VAR2'")
    }

    var1 <- from_split[1]
    var2 <- from_split[2]

    # 생성된 변수 이름 설정
    if (!is.null(names(interactions)[i]) && names(interactions)[i] != "") {
      new_var_name <- names(interactions)[i]
    } else {
      new_var_name <- paste0("int_", substr(var1, 1, 2), "", substr(var2, 1, 2))
    }

    # 새로운 변수 생성
    new_col <- data[[var1]] * data[[var2]]
    new_data <- cbind(new_col, new_data)
    names(new_data)[1] <- new_var_name
  }

  # 열 이름을 소문자로 변환
  if (to_lower) {
    names(new_data) <- tolower(names(new_data))
  }

  return(new_data)
}
