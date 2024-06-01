
#' mat_text_arrange
#'
#' @param mat matrix data
#' @param text TRUE '' FALSE number
#' @param byrow FALSE, byrow =TRUE fill in row
#'
#' @return  text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Example usage
#' mmm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30), nrow = 5, ncol = 6)
#'
#' make_mat_text(mmm, text = TRUE, byrow = TRUE)
#'
#' make_mat_text(mmm, text = TRUE, byrow = FALSE)
#'
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
#'   make_mat_text2()
#' }
#'

make_mat_text <- function(mat, text = TRUE, byrow = FALSE) {
  # Convert the matrix to a vector based on byrow parameter
  if (byrow) {
    mat_vector <- as.vector(t(mat))
  } else {
    mat_vector <- as.vector(mat)
  }

  # Create a formatted string
  if (text) {
    formatted_string <- paste0("  ",
        paste(sapply(mat_vector,function(x) if (is.na(x)) "NA" else paste0("'", x, "'")),
                                     collapse = ", "))
  } else {
    formatted_string <- paste0("  ",
                               paste(sapply(mat_vector,
                                            function(x) if (is.na(x)) 0 else x),
                                     collapse = ", "))
  }

  # Add line breaks after every ncol elements
  ncol <- ncol(mat)
  lines <- strsplit(formatted_string, ", ")[[1]]

  formatted_lines <- sapply(seq(1, length(lines), by = ncol),
                            function(i) paste(lines[i:(i + ncol - 1)],
                                              collapse = ", "))

  # Combine the lines into a final string
  final_string <- paste0("layout_New = matrix(\n  c(",
                         paste(formatted_lines, collapse = ",\n    "),
                         "),\n    nrow = ", nrow(mat),
                         ", ncol = ", ncol(mat),
                         ", byrow = ", ifelse(byrow, "TRUE", "FALSE"), ")\n")

  return(cat(final_string))
}



#' Functions that let you input data in matrix form
#'
#' @param data input dat a
#' @param text text = TRUE is text FALSE numeric
#' @param byrow byrow=FALSE  or TRUE
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
#' lay %>% make_mat_text2()
#'
#'
#' NewMat = matrix(c('ceo_8', NA, 'ceo_7', NA, 'ceo_6',
#' NA, 'ceo_4', NA, 'ceo_3', NA, 'ceo_2', NA, 'ceo_1',
#' NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#' 'satis_5', NA, NA, NA, NA, NA, 'CEOCOM', NA, NA, NA,
#' NA, NA, NA, 'satis_4', NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, NA, 'SATISF', NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, 'satis_3', NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'satis_1',
#'   NA, NA, NA, NA, NA, 'STAYON', NA, NA, NA, NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'    NA, NA, NA, NA, NA, NA, 'stay_2', NA, NA, NA,
#'    'stay_1', NA, NA, NA, NA), nrow = 13, ncol = 9)
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
#' #' #'
#'  lay_str1 %>%make_mat_text3(byrow=T)
#'
#'
#'  NewMat = matrix(c(NA, NA, 'PEOU', NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, 'CUX', NA, NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, 'AT', NA, 'USE',
#'   'SUX', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'   NA, NA, NA, NA, NA, 'PU', NA, NA, NA, NA),
#'   nrow = 7, ncol = 7, byrow = TRUE )
#'
#'  rownames(NewMat) <- c("row1", "row2", "row3", "row4", "row5", "row6", "row7")
#' colnames(NewMat) <- c("col1", "col2", "col3", "col4", "col5", "col6", "col7")
#'
#' NewMat %>%make_mat_text2(byrow=FALSE)
#' NewMat %>%make_mat_text2(byrow=TRUE)
#' #'
#' #'
#' }
#'
#'
make_mat_text2 <- function(data, text = TRUE, byrow = FALSE) {  # byrow 옵션 추가
  if (!is.matrix(data)) {
    cat("Input data is not a matrix.\n")
    return()
  }

  # Check the number of rows and columns in a matrix
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  # 빈칸을 모두 NA로 처리
  data[data == ""] <- NA
  data[data == 'NA'] <- NA

  # Convert matrix data to input form
  mat_text <- "NewMat = matrix(c("

  # byrow 옵션에 따라 데이터를 가져오는 순서 변경
  if (byrow) {  # byrow가 TRUE일 경우
    for (i in 1:n_rows) {
      for (j in 1:n_cols) {
        if (text) {
          mat_text <- paste(mat_text, "'", data[i, j], "'", ", ", sep = "")
        } else {
          mat_text <- paste(mat_text, data[i, j], ", ", sep = "")
        }
      }
    }
  } else {  # byrow가 FALSE일 경우 (기존과 동일)
    for (j in 1:n_cols) {
      for (i in 1:n_rows) {
        if (text) {
          mat_text <- paste(mat_text, "'", data[i, j], "'", ", ", sep = "")
        } else {
          mat_text <- paste(mat_text, data[i, j], ", ", sep = "")
        }
      }
    }
  }


  mat_text <- sub(", $", "", mat_text)  # 마지막 쉼표 제거
  mat_text <- paste(mat_text, "),\n", sep = "")
  mat_text <- paste(mat_text, "nrow = ", n_rows, ", ", sep = "")
  mat_text <- paste(mat_text, "ncol = ", n_cols, ",", sep = "")

  if (byrow) {  # byrow가 TRUE일 때만 byrow=TRUE 추가
    mat_text <- paste(mat_text, "byrow = TRUE", ")\n\n", sep = " ")
  } else {
    mat_text <- paste(mat_text, " byrow = FALSE", ")\n\n", sep = "")  # 기존 코드 유지
  }

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
make_mat_text_col <- function(data, text = TRUE, firstcol=1) {
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



