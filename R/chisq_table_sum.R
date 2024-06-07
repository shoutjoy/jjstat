
#' chisq table resize
#'
#' @param data mat
#' @param ... con 1,1,2,2
#' @param trans matr transpose
#' @param sep sep rowname combine  "-"
#'
#' @return mat
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Test data
#' data <- data.frame(T = c(30, 40, 60, 120),
#'                    S = c(50, 70, 60, 80))
#' rownames(data) <- LETTERS[1:4]
#' data <- as.matrix(data)
#'
#' # Example usage
#' print(chisq_table_sum(data, 1, 1, 2, 2))
#'
#' chisq_table_sum(data, 1, 1, 2, 2)
#' chisq_table_sum(data, con = c(1, 1, 2, 2))
#' chisq_table_sum(data, la = c(1, 1, 2, 2))
#'
#' }
#'
#'
#'
chisq_table_sum <- function(data, ..., trans=FALSE, sep="_") {

  # Combine additional arguments into a single vector
  con <- c(...)

  # If con is NULL or empty, return the data as is
  if (is.null(con) || length(con) == 0) {
    return(data)
  }

  if(is.table(data)){
    data = data %>% data.frame()%>% to_table()
    data = as.data.frame(data)
  }else if(is.matrix(data)){
    data = as.data.frame(data)
  }
  #transpose
  if(trans){
    data = t(data)
  }else{
    data = data
  }
  data = as.data.frame(data)
  # 행 이름 저장
  row_names <- rownames(data)

  # 데이터에 c 열 추가
  data$c <- con

  # con 값이 0인 행 제거
  filtered_data <- data[data$c != 0, , drop = FALSE]

  # 행 이름과 con 값을 합쳐 새로운 데이터 프레임 생성
  Names <- data.frame(row_names = row_names[data$c != 0], con = con[data$c != 0])

  # con 값에 따라 데이터 그룹화 및 합산
  result <- aggregate(. ~ c, filtered_data, sum)

  # Names 데이터 프레임을 이용하여 같은 종류끼리 합치기
  nameresult <- aggregate(row_names ~ con, Names, paste, collapse = sep)

  # c 열 제거
  result <- result[, -which(names(result) == "c")]

  # 결과 출력
  res <- cbind(nameresult, result) %>%
    arrange(row_names) %>%
    dplyr::select(-1) %>%
    tibble::column_to_rownames("row_names")

  return(res)
}
