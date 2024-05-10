
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
make_mat_text <- function(data, text = TRUE) {
  if (!is.matrix(data)) {
    cat("Input data is not a matrix.\n")
    return()
  }

  # Check the number of rows and columns in a matrix
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  # Colsnaems <- colsnames(data)
  # Rowsnaems <- rowsnames(data)

  # 빈칸을 모두 NA로 처리
  data[data == ""] <- NA
  data[data == 'NA'] <- NA

  # Convert matrix data to input form
  mat_text <- "NewMat = matrix(c("

  #Set all data to be wrapped in ''
  if(text){
    for (j in 1:n_cols) {
      for (i in 1:n_rows) {
        mat_text <- paste(mat_text, "'", data[i, j], "'", ", ", sep = "")
      }
    }

  }else{
    # Disable wrapping with '' in all data
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


#' 첫번째 열이 존재하는 경우에 사용하는 make_mat_text2
#'
#' @param data data.frame
#' @param text text TRUE
#' @param firstcol ncol setting
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data_text = "Tobacco_Dependence (0,5] (5,10] (10,15] (15,20] (20,98]
#' No_Nicotine_Dependence 130 210 43 114 20
#' Nicotine_Dependence 119 267 91 254 67"
#' nt=data_text%>% text2df(header=T)%>%tibble::column_to_rownames("Tobacco_Dependence")
#' nt %>%tibble()
#' nt=Char2num(nt,1:5)
#' nt
#' }
#'
#'
make_mat_text2 <- function(data, text = TRUE, firstcol=1) {
  if (!is.matrix(data)) {
    cat("Input data is not a matrix.\n")
    return()
  }  # data= data[,-firstcol]
  # Check the number of rows and columns in a matrix
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  # 빈칸을 모두 NA로 처리
  data[data == ""] <- NA
  data[data == 'NA'] <- NA

  # Convert matrix data to input form
  mat_text <- "NewMat = matrix(c("

  #Set all data to be wrapped in ''
  if (text) {
    for (j in 1:n_cols) {
      for (i in 1:n_rows) {
        # 데이터가 NA이면 그대로 사용하고, 아니면 문자열로 변환 후 따옴표 추가
        if (is.na(data[i, j])) {
          mat_text <- paste(mat_text, "NA", ", ", sep = "")
        } else {
          if (is.character(data[i, j])) {
            mat_text <- paste(mat_text, "'", data[i, j], "'", ", ", sep = "")
          } else {
            mat_text <- paste(mat_text, data[i, j], ", ", sep = "")
          }
        }
      }
    }

  } else {
    # Disable wrapping with '' in all data
    for (j in 1:n_cols) {
      for (i in 1:n_rows) {
        # 데이터가 NA이면 그대로 사용하고, 아니면 작은 따옴표 없이 그대로 사용
        if (is.na(data[i, j])) {
          mat_text <- paste(mat_text, "NA", ", ", sep = "")
        } else {
          mat_text <- paste(mat_text, data[i, j], ", ", sep = "")
        }
      }
    }

  }

  mat_text <- sub(", $", "", mat_text)  # 마지막 쉼표 제거

  # 정규표현식을 사용하여 부분 추출
  pattern <- "^NewMat = matrix\\("
  extracted <- gsub(pattern, "", mat_text)
  extracted <- gsub("^c\\(", "", extracted)


  # 정규표현식을 사용하여 문자열과 숫자를 추출
  matches <- gregexpr("([0-9]+|[^,]+)", extracted, perl = TRUE)
  values <- regmatches(extracted, matches)[[1]]  #매칭데이터 정리

  numbers <- gsub("^\\s+|\\s+$", "", values)  #빈칸 제거
  # 숫자와 문자열을 구분하여 추출
  numeric_values <- numbers[sapply(numbers, function(x) grepl("^\\d+$", x))]
  numeric_values <- as.numeric(numeric_values)

  character_values <- numbers[sapply(numbers, function(x) !grepl("^\\d+$", x))]
  # numbers2= c(character_values, numeric_values )



  # 행렬의 행 이름과 열 이름이 있는 경우 출력에 포함
  # 숫자들을 문자열로 변환하여 결합
  numeric_values_combined <- paste(numeric_values, collapse = ", ")

  mat_text <- paste0("NewMat = matrix(c(", numeric_values_combined, ")"
  )

  mat_text <- paste(mat_text, ",\n", sep = "") #enter처리
  mat_text <- paste(mat_text, "      nrow = ", n_rows, ", ", sep = "") #nrow
  mat_text <- paste(mat_text, "ncol = ", n_cols-firstcol, ")\n\n", sep = "") #ncol


  mat_text <- paste(mat_text, "rownames(NewMat) <- c(",
                    paste0('"', character_values, '"', collapse = ", "), ")\n", sep = "")


  mat_text <- paste(mat_text, "colnames(NewMat) <- c(",
                    paste0('"', colnames(data)[-1], '"', collapse = ", "), ")\n", sep = "")



  # # 패턴이 'NA'인 문자열을 NA로 대체
  mat_text <- gsub("'NA'", "NA", mat_text)

  # # text로 출력
  cat("\n\n", mat_text, "\n" )

  check= list(mat_text,extracted, character_values,numeric_values )
  # # console에서 데이터로 출력
  eval(parse(text = mat_text))

  # replace_df 출력
  cat("\n")
  replace_df(pattern='NA',imp=NA, NewMat)

}
