
#' Functions that let you input data in matrix form
#'
#' @param data input dat a
#'
#' @return make test
#' @export
#'
#' @examples
#'
#' \dontrun{
#' lay
#' V1      V2 V3        V4        V5       V6        V7        V8 V9
#' [1,] "ceo_8" NA "satis_5" "satis_4" ""       "satis_3" "satis_1" NA ""
#' [2,] ""      NA ""        ""        ""       ""        ""        NA ""
#' [3,] "ceo_7" NA ""        ""        ""       ""        ""        NA ""
#' [4,] ""      NA ""        ""        "SATISF" ""        ""        NA ""
#' [5,] "ceo_6" NA ""        ""        ""       ""        ""        NA "stay_2"
#' [6,] ""      NA ""        ""        ""       ""        ""        NA ""
#' [7,] "ceo_4" NA "CEOCOM"  ""        ""       ""        "STAYON"  NA ""
#' [8,] ""      NA ""        ""        ""       ""        ""        NA ""
#' [9,] "ceo_3" NA ""        ""        ""       ""        ""        NA "stay_1"
#' [10,] ""      NA ""        ""        ""       ""        ""        NA ""
#' [11,] "ceo_2" NA ""        ""        ""       ""        ""        NA ""
#' [12,] ""      NA ""        ""        ""       ""        ""        NA ""
#' [13,] "ceo_1" NA ""        ""        ""       ""        ""        NA ""
#'
#' lay %>% make_mat_text()
#'
#'
#' NewMat = matrix(c('ceo_8', NA, 'ceo_7', NA, 'ceo_6', NA, 'ceo_4', NA, 'ceo_3', NA, 'ceo_2', NA, 'ceo_1', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'satis_5', NA, NA, NA, NA, NA, 'CEOCOM', NA, NA, NA, NA, NA, NA, 'satis_4', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'SATISF', NA, NA, NA, NA, NA, NA, NA, NA, NA, 'satis_3', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'satis_1', NA, NA, NA, NA, NA, 'STAYON', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'stay_2', NA, NA, NA, 'stay_1', NA, NA, NA, NA),
#'                 nrow = 13, ncol = 9)
#'
#'
#' colnames(NewMat) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
#'
#' #as.numeric
#'# NewMat <- apply(NewMat, 2, function(x) as.numeric(gsub("'", "", x)))
#'
#' V1      V2 V3        V4        V5       V6        V7        V8 V9
#' [1,] "ceo_8" NA "satis_5" "satis_4" NA       "satis_3" "satis_1" NA NA
#' [2,] NA      NA NA        NA        NA       NA        NA        NA NA
#' [3,] "ceo_7" NA NA        NA        NA       NA        NA        NA NA
#' [4,] NA      NA NA        NA        "SATISF" NA        NA        NA NA
#' [5,] "ceo_6" NA NA        NA        NA       NA        NA        NA "stay_2"
#' [6,] NA      NA NA        NA        NA       NA        NA        NA NA
#' [7,] "ceo_4" NA "CEOCOM"  NA        NA       NA        "STAYON"  NA NA
#' [8,] NA      NA NA        NA        NA       NA        NA        NA NA
#' [9,] "ceo_3" NA NA        NA        NA       NA        NA        NA "stay_1"
#' [10,] NA      NA NA        NA        NA       NA        NA        NA NA
#' [11,] "ceo_2" NA NA        NA        NA       NA        NA        NA NA
#' [12,] NA      NA NA        NA        NA       NA        NA        NA NA
#' [13,] "ceo_1" NA NA        NA        NA       NA        NA        NA NA
#' #'
#' }
#'
#'
make_mat_text <- function(data, text = FALSE) {
  if (!is.matrix(data)) {
    cat("Input data is not a matrix.\n")
    return()
  }

  # 행렬의 행, 열 개수 확인
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  # Colsnaems <- colsnames(data)
  # Rowsnaems <- rowsnames(data)

  # 빈칸을 모두 NA로 처리
  data[data == ""] <- NA
  data[data == 'NA'] <- NA

  # 행렬 데이터를 입력 형태로 변환
  mat_text <- "NewMat = matrix(c("

  if(text){
    for (j in 1:n_cols) {
      for (i in 1:n_rows) {
        mat_text <- paste(mat_text, "'", data[i, j], "'", ", ", sep = "")
      }
    }

  }else{
    for (j in 1:n_cols) {
      for (i in 1:n_rows) {
        mat_text <- paste(mat_text, data[i, j], ", ", sep = "")
      }
    }

  }

  mat_text <- sub(", $", "", mat_text)  # 마지막 쉼표 제거
  mat_text <- paste(mat_text, "),\n", sep = "")
  mat_text <- paste(mat_text, "nrow = ", n_rows, ", ", sep = "")
  mat_text <- paste(mat_text, "ncol = ", n_cols, ")\n\n", sep = "")

  # 행렬의 행 이름과 열 이름이 있는 경우 출력에 포함
  if (!is.null(rownames(data))) {
    mat_text <- paste(mat_text, "rownames(NewMat) <- c(",
                      paste0('"', rownames(data), '"', collapse = ", "), ")\n", sep = "")
  }
  if (!is.null(colnames(data))) {
    mat_text <- paste(mat_text, "colnames(NewMat) <- c(",
                      paste0('"', colnames(data), '"', collapse = ", "), ")\n", sep = "")
  }

  # 패턴이 'NA'인 문자열을 NA로 대체
  mat_text <- gsub("'NA'", "NA", mat_text)



  # text로 출력
  cat("\n\n", mat_text, "\n\n" )

  # console에서 데이터로 출력
  eval(parse(text = mat_text))

  # replace_df 출력
  cat("\n\n")
  replace_df(pattern='NA',imp=NA, NewMat)

}
