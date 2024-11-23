#' 비율 척도로 변환하는 함수 정의
#'
#' @param data data
#' @param columns colum all
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#'  사용 예
#' data_frame: 데이터프레임 객체
#' 변환할 열 이름을 명시적으로 지정하거나 NULL로 모든 열을 변환
#' scaled_data <- min_max_scaler(data_frame, columns = c("V1_1", "V2_1", "V3_1"))
#' print(scaled_data)
#' }
convert_min_max_scaler <- function(data, columns = NULL) {
  # columns 인자: 스케일링할 열의 이름이나 인덱스를 지정. NULL일 경우 모든 열에 대해 수행.
  if (is.null(columns)) {
    columns <- names(data)
  }

  scaled_data <- data  # 원본 데이터 보존

  for (col in columns) {
    col_min <- min(data[[col]], na.rm = TRUE)  # 최소값 계산
    col_max <- max(data[[col]], na.rm = TRUE)  # 최대값 계산

    # 0에서 1 사이의 비율로 변환
    scaled_data[[col]] <- (data[[col]] - col_min) / (col_max - col_min)
  }

  return(scaled_data)
}



#' 비율 척도로 변환하는 함수 정의
#'
#' @param data data
#' @param columns colum all
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#'  사용 예
#' data_frame: 데이터프레임 객체
#' 변환할 열 이름을 명시적으로 지정하거나 NULL로 모든 열을 변환
#' scaled_data <- min_max_scaler(data_frame, columns = c("V1_1", "V2_1", "V3_1"))
#' print(scaled_data)
#' }
min_max_scaler <- function(data, columns = NULL) {
  # columns 인자: 스케일링할 열의 이름이나 인덱스를 지정. NULL일 경우 모든 열에 대해 수행.
  if (is.null(columns)) {
    columns <- names(data)
  }

  scaled_data <- data  # 원본 데이터 보존

  for (col in columns) {
    col_min <- min(data[[col]], na.rm = TRUE)  # 최소값 계산
    col_max <- max(data[[col]], na.rm = TRUE)  # 최대값 계산

    # 0에서 1 사이의 비율로 변환
    scaled_data[[col]] <- (data[[col]] - col_min) / (col_max - col_min)
  }

  return(scaled_data)
}
