#' layout_mat
#'
#' @param semplot semPaths2 data
#' @param text text True output text matrix
#' @param variable variable is rownames adapt
#' @param byrow byrow TRUE
#'
#' @return  data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' data(offense)
#' offense %>%str()
#'
#' nfl_path = matrix(
#'   c(0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 1, 1, 0, 0,
#'     1, 0, 0, 1, 0),
#'   nrow = 5, ncol = 5, byrow = TRUE )
#'
#' rownames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' colnames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' nfl_path
#'
#' nfl_blocks = list(7:8, 1:3, 4:6,1:6, 9:11)
#'
#' plspm_blocks_match(data=offense, blocks=nfl_blocks,
#'                    paths=nfl_path)
#'
#' nfl_mode =c("B","A","A","A","A")
#'
#' nfl_pls = plspm(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#' nfl_pls%>%summary()
#'
#' #########
#' nfl_pls_boot = plspm_sem(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#'
#' #Layouts extracted from mockups
#' plspm_semPaths2(nfl_pls_boot,sizeLat = 6)%>%
#'   plspm_factor_layout()
#'
#' #Make it input material
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%plspm_layout_mat(text=T)
#'
#' #Organizing materials for input
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%plspm_layout_mat()%>%
#'   mat_text_arrange()
#' }
#'
plspm_layout_mat <- function(semplot, text= TRUE, byrow =TRUE,
                       variable = TRUE) {

  data= semplot


  # 데이터에서 x, y 값 추출
  x_values <- data[, 1]
  y_values <- data[, 2]

  # x, y의 최소 및 최대값 설정
  x_min <- min(x_values)
  x_max <- max(x_values)
  y_min <- min(y_values)
  y_max <- max(y_values)

  # 행렬의 크기 설정
  x_range <- seq(x_min, x_max, length.out = 10)  # x 범위
  y_range <- seq(y_min, y_max, length.out = 10)  # y 범위

  # 빈 행렬 생성
  mat <- matrix(0, nrow = 10, ncol = 10)

  # 각 좌표를 행렬에 배치
  for (i in 1:nrow(data)) {
    x <- data[i, 1]
    y <- data[i, 2]

    # x, y 값을 행렬의 인덱스로 변환
    x_index <- which.min(abs(x_range - x))
    y_index <- which.min(abs(y_range - y))

    if (variable) {
      mat[11 - y_index, x_index] <- rownames(data)[i]  # y축을 뒤집어서 배치
    } else {
      mat[11 - y_index, x_index] <- 1  # y축을 뒤집어서 배치
    }
  }

  # 0을 NA로 변경
  if (variable) {
    mat[mat == 0] <- NA
  } else {
    mat[mat == 0] <- 0
  }

  if(text){
    return(mat%>% mat_text_arrange(byrow = byrow))
  }else{
    return(mat)
  }

}




#' layout_mat
#'
#' @param semplot semPaths2 data
#' @param text text True output text matrix
#' @param variable variable is rownames adapt
#' @param byrow byrow TRUE
#'
#' @return  data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' data(offense)
#' offense %>%str()
#'
#' nfl_path = matrix(
#'   c(0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 1, 1, 0, 0,
#'     1, 0, 0, 1, 0),
#'   nrow = 5, ncol = 5, byrow = TRUE )
#'
#' rownames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' colnames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' nfl_path
#'
#' nfl_blocks = list(7:8, 1:3, 4:6,1:6, 9:11)
#'
#' plspm_blocks_match(data=offense, blocks=nfl_blocks,
#'                    paths=nfl_path)
#'
#' nfl_mode =c("B","A","A","A","A")
#'
#' nfl_pls = plspm(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#' nfl_pls%>%summary()
#'
#' #########
#' nfl_pls_boot = plspm_sem(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#'
#' #Layouts extracted from mockups
#' plspm_semPaths2(nfl_pls_boot,sizeLat = 6)%>%
#'   plspm_factor_layout()
#'
#' #Make it input material
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat(text=T)
#'
#' #Organizing materials for input
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat()%>%
#'   mat_text_arrange()
#' }
#'
layout_mat <- function(semplot, text= TRUE, byrow =TRUE,
                       variable = TRUE) {

  data= semplot


  # 데이터에서 x, y 값 추출
  x_values <- data[, 1]
  y_values <- data[, 2]

  # x, y의 최소 및 최대값 설정
  x_min <- min(x_values)
  x_max <- max(x_values)
  y_min <- min(y_values)
  y_max <- max(y_values)

  # 행렬의 크기 설정
  x_range <- seq(x_min, x_max, length.out = 10)  # x 범위
  y_range <- seq(y_min, y_max, length.out = 10)  # y 범위

  # 빈 행렬 생성
  mat <- matrix(0, nrow = 10, ncol = 10)

  # 각 좌표를 행렬에 배치
  for (i in 1:nrow(data)) {
    x <- data[i, 1]
    y <- data[i, 2]

    # x, y 값을 행렬의 인덱스로 변환
    x_index <- which.min(abs(x_range - x))
    y_index <- which.min(abs(y_range - y))

    if (variable) {
      mat[11 - y_index, x_index] <- rownames(data)[i]  # y축을 뒤집어서 배치
    } else {
      mat[11 - y_index, x_index] <- 1  # y축을 뒤집어서 배치
    }
  }

  # 0을 NA로 변경
  if (variable) {
    mat[mat == 0] <- NA
  } else {
    mat[mat == 0] <- 0
  }

  if(text){
    return(mat%>% mat_text_arrange(byrow = byrow))
  }else{
    return(mat)
  }

}
