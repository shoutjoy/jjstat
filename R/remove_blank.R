#' 데이터내에서 빈칸 제거
#'
#' @param data df
#' @param col col 숫자와 열이름으로 지정
#' @param data_type data.frame, vector
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 예제 데이터
#' vector_data <- c("익산 어양 초", "익산옥 야 초")
#' df_data <- data.frame(
#'   ID = c(259, 260),
#'   Name = c("익산 어양 초", "익산옥 야 초"),
#'   Value = c(1, 1),
#'   Score = c(0.1237624, 0.1237624)
#' )
#'
#' # 벡터에서 공백 제거
#' clean_vector <- remove_blank(vector_data, data_type = "vector")
#' print(clean_vector)
#'
#' # 데이터프레임에서 공백 제거 (열 이름으로)
#' clean_df <- remove_blank(df_data, col = "Name")
#' print(clean_df)
#'
#' # 데이터프레임에서 공백 제거 (열 번호로)
#' clean_df <- remove_blank(df_data, col = 2)
#' print(clean_df)
#' #'
#'
#' }
remove_blank <- function(data, col = NULL, data_type = c("data.frame", "vector", "character")) {
  # data_type 기본값 설정 (data.frame이 기본값)
  data_type <- match.arg(data_type)

  if (data_type == "vector") {
    # 벡터에서 공백 제거
    if (!is.vector(data)) {
      stop("data가 벡터가 아닙니다.")
    }
    # 모든 공백 및 양쪽 공백 제거
    return(trimws(gsub(" ", "", data)))

  } else if (data_type == "character") {
    # 단일 문자열에서 공백 제거
    if (!is.character(data)) {
      stop("data가 문자열이 아닙니다.")
    }
    # 모든 공백 및 양쪽 공백 제거
    return(trimws(gsub(" ", "", data)))

  } else if (data_type == "data.frame") {
    # 데이터프레임에서 공백 제거
    if (!is.data.frame(data)) {
      stop("data가 데이터프레임이 아닙니다.")
    }
    if (is.null(col)) {
      stop("data_type이 'data.frame'인 경우 col을 지정해야 합니다.")
    }

    # col이 숫자일 경우 열 이름으로 변환
    if (is.numeric(col)) {
      col <- colnames(data)[col]
    }
    if (!col %in% colnames(data)) {
      stop("col이 데이터프레임의 열 이름 또는 번호에 맞지 않습니다.")
    }

    # 해당 열에서 모든 공백 및 양쪽 공백 제거
    data[[col]] <- trimws(gsub(" ", "", data[[col]]))
    return(data)
  }
}
