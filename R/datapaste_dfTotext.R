#' data.frame to input text
#'
#' @param data data.frame
#' @param rownames rownames=TRUE, FALSE -> nothing rownames
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # 데이터 프레임 생성
#' cho2013_cor <- data.frame(
#'   필요성 = c(1.000, 0.320, 0.266, 0.506, 0.429, 0.401),
#'   수업 = c(0.320, 1.000, 0.647, 0.463, 0.548, 0.554),
#'   교사 = c(0.266, 0.647, 1.000, 0.383, 0.491, 0.545),
#'   계획 = c(0.506, 0.463, 0.383, 1.000, 0.531, 0.491),
#'   영향 = c(0.429, 0.548, 0.491, 0.531, 1.000, 0.743),
#'   만족도 = c(0.401, 0.554, 0.545, 0.491, 0.743, 1.000)
#' )
#' cho2013_cor %>% make_df_text()
#'
#'
#' }
#'

make_df_text <- function(data, rownames = TRUE, text = FALSE) {
  # 데이터프레임으로 변환
  data <- as.data.frame(data)

  # 데이터프레임의 칼럼 이름 가져오기
  col_names <- names(data)

  # 데이터프레임에 데이터가 있는지 확인
  if (nrow(data) == 0) {
    cat("Data frame is empty.\n")
    return()
  }

  # 텍스트로 변환
  df_text <- "New = data.frame("

  # rownames 옵션에 따라 처리
  if (rownames) {
    if (!is.null(rownames(data))) {
      df_text <- paste(df_text, "row.names = c(",
                       paste0('"', rownames(data), '"', collapse = ", "),
                       "), \n", sep = "")
    }
  }

  if(text){
    for (col in col_names) {
      df_text <- paste(df_text, col, " = c(", sep = "")
      for (i in 1:nrow(data)) {
        if (is.na(data[i, col])) {
          df_text <- paste(df_text, "NA, ", sep = "")
        } else {
          df_text <- paste(df_text, "'", data[i, col], "', ", sep = "")
        }
      }
      df_text <- sub(", $", "", df_text)  # 마지막 쉼표와 공백 제거
      df_text <- paste(df_text, "),\n ", sep = "")
    }

  }else{
    for (col in col_names) {
      df_text <- paste(df_text, col, " = c(", sep = "")
      for (i in 1:nrow(data)) {
        if (is.na(data[i, col])) {
          df_text <- paste(df_text, "NA, ", sep = "")
        } else {
          df_text <- paste(df_text, data[i, col], ", ", sep = "")
        }
      }
      df_text <- sub(", $", "", df_text)  # 마지막 쉼표와 공백 제거
      df_text <- paste(df_text, "),\n ", sep = "")
    }

  }


  df_text <- sub(",\n $", "\n", df_text)  # 마지막 쉼표와 줄바꿈 제거
  df_text <- paste(df_text, ")\n", sep = "")

  # 결과 출력
  cat("\n\n", df_text,"\n\n")
}

#' data.frame to input text
#'
#' @param data data.frame
#' @param rownames rownames=TRUE, FALSE -> nothing rownames
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # 데이터 프레임 생성
#' cho2013_cor <- data.frame(
#'   필요성 = c(1.000, 0.320, 0.266, 0.506, 0.429, 0.401),
#'   수업 = c(0.320, 1.000, 0.647, 0.463, 0.548, 0.554),
#'   교사 = c(0.266, 0.647, 1.000, 0.383, 0.491, 0.545),
#'   계획 = c(0.506, 0.463, 0.383, 1.000, 0.531, 0.491),
#'   영향 = c(0.429, 0.548, 0.491, 0.531, 1.000, 0.743),
#'   만족도 = c(0.401, 0.554, 0.545, 0.491, 0.743, 1.000)
#' )
#' cho2013_cor %>% datapaste_text()
#' #'#정성령(2014)
#' Jungsy2014 = "
#' 수강료 1
#' 강사 .165** 1
#' 수업 .303** .144** 1
#' 개인관리 .494** .069 .284** 1
#' 인지도 .425** .176** .407** .424** 1
#' 이상적영향력 .210** -.056 .212** .286** .222** 1
#' 영감적동기화 .190** -.133** .211** .290** .230** .659** 1
#' 지적자극 .300** -.136** .159** .402** .238** .533** .517** 1
#' 개별적 배려 .202** -.153** .150** .250** .291** .684** .618** .530** 1
#' "
#' lav_matrix(Jungsy2014)
#'
#' Jungsy2014_cor = lav_matrix(Jungsy2014)
#'
#' Jungsy2014_cor %>% make_df_text()
#' }
#'
datapaste_text <-  function(data, rownames = TRUE) {
  # 데이터프레임으로 변환
  data <- as.data.frame(data)

  # 데이터프레임의 칼럼 이름 가져오기
  col_names <- names(data)

  # 데이터프레임에 데이터가 있는지 확인
  if (nrow(data) == 0) {
    cat("Data frame is empty.\n")
    return()
  }

  # 텍스트로 변환
  df_text <- "New = data.frame("

  # rownames 옵션에 따라 처리
  if (rownames) {
    if (!is.null(rownames(data))) {
      df_text <- paste(df_text,
                       "row.names = c(", paste0('"',
                        rownames(data), '"',
                        collapse = ", "), "), \n", sep = "")
    }
  }

  for (col in col_names) {
    df_text <- paste(df_text, col, " = c(", sep = "")
    for (i in 1:nrow(data)) {
      if (is.na(data[i, col])) {
        df_text <- paste(df_text, "NA, ", sep = "")
      } else {
        df_text <- paste(df_text, "'", data[i, col], "', ", sep = "")
      }
    }
    df_text <- sub(", $", "", df_text)  # 마지막 쉼표와 공백 제거
    df_text <- paste(df_text, "), \n", sep = "")
  }
  df_text <- sub(", \n$", "", df_text)  # 마지막 쉼표와 공백 제거
  df_text <- paste(df_text, ")\n", sep = "")

  # 결과 출력
  cat("\n\n", df_text,"\n\n")
}
