#' pattern_detector: missing data detect
#'
#' @param df data
#' @param missing missing TRUE detecte missing T,F
#' @param data TRUE data print, FALSE rownames number
#' @param pattern pattern "" or  NA
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ed_tam %>% pattern_detector()
#' ed_tam %>% pattern_detector(FALSE,TRUE)%>% head()
#' # missing data chek
#' ed_tam %>% pattern_detector(missing=T, data=F)
#' #missing data output
#' ed_tam %>% pattern_detector(missing=T, data=T) %>%head(30)
#'
#' # not missing
#' ed_tam %>% pattern_detector(data=T,missing=F) %>%head()
#'
#' # ot missing , not data
#' ed_tam %>% pattern_detector(data=F,missing=F)
#'
#'
#' }
#'
#'
pattern_detector <- function(df, missing = TRUE, data = FALSE, pattern = "") {
  # 패턴에 맞는 행을 찾는지 여부를 결정하는 함수
  find_pattern <- function(row) {
    if (is.na(pattern)) {
      return(any(is.na(row)))
    } else {
      return(any(row == pattern, na.rm = TRUE))
    }
  }

  # 패턴에 맞는 행 또는 맞지 않는 행을 선택
  if (missing == TRUE) {
    # 패턴에 맞는 행을 필터링
    filtered_df <- df[apply(df, 1, function(row) find_pattern(row)), ]
    missing_rows = rownames(filtered_df)

    # output 인수에 따른 처리
    if (data == TRUE) {
      # 조건에 맞는 행을 반환
      res = return(filtered_df)

    } else {
      # missng data row names
      print( missing_rows)
      #missing data number
      res=(nrow(filtered_df))
      return(res)
    }
    # missing data

  } else {
    #pattern missing이 아닌 데이터
    filtered_df <- df[apply(df, 1, function(row) !find_pattern(row)), ]
    not_missing_rows = rownames(filtered_df)

    # output 인수에 따른 처리
    if (data == TRUE) {
      # 조건에 맞는 행을 반환
      return(filtered_df)

    } else {
      # 조건에 맞는 행의 개수를 반환
      print( not_missing_rows)
      return(nrow(filtered_df))
    }

  } }
