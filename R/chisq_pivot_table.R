#' pivot
#'
#' @param data data
#' @param formula formul
#' @param names_from  column
#' @param unite 1,2 colum unite
#' @param col 합쳐질 컬럼
#' @param remove 기존것 지우기
#' @param change_col 열의 위치 바꾸기
#'
#' @return 데이터
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #' kge_bind3a %>%
#' filter(type == "고유어1")%>%
#'   pivot_table(~ area + age + 성조, unite=TRUE)
#' #'
#' kge_bind3a %>%
#'   filter(type == "고유어1")%>%
#'   pivot_table(~  age + area+ 성조, unite=FALSE)
#'
#' #고유어1 공명음  지역별 연령
#' kge_bind3a %>%
#'   filter(type == "고유어1" & a1=="공명음")%>%
#'   pivot_table(~ age + area + 성조, unite=T)  #%>% md()
#'
#'
#'
#' kge_bind3a %>%
#'   filter(type == "고유어1")%>%
#'   pivot_table(~  age +  성조, unite=FALSE)
#'
#' #고유어1 공명음  지역별 연령
#' kge_bind3a %>%
#'   filter(type == "고유어1" & a1=="공명음")%>%
#'   pivot_table(~  area + 성조, unite=F)  #%>% md()

#'
#'
#'   }
#' #'
#'

pivot_table <- function(data, formula, names_from = NULL, unite = FALSE,
                         col = 1:2, remove = TRUE, change_col = TRUE) {
  library(dplyr)
  library(tidyr)
  library(rlang)

  # formula에서 마지막 변수를 추출
  if (is.null(names_from)) {
    terms <- all.vars(formula) # formula에서 모든 변수 이름 추출
    names_from <- tail(terms, 1) # 마지막 변수 선택
  } else {
    # names_from이 변수명인지 확인하여 처리
    names_from <- enquo(names_from) # 변수명을 캡처
    names_from <- eval_tidy(names_from) # 캡처한 변수명을 평가
  }

  # xtabs로 교차표 생성
  table_data <- data %>%
    xtabs(data = ., formula) %>%
    as.data.frame()

  # change_col 옵션에 따라 1열과 2열 위치 변경
  if (change_col) {
    col_order <- c(2, 1, 3:ncol(table_data)) # 2열과 1열의 순서를 바꾼 후 나머지 유지
    table_data <- table_data[, col_order]   # 열 순서 변경
  }

  # unite 옵션이 TRUE일 경우
  if (unite) {
    # col이 숫자형 벡터인지 확인하고 유효한 열 인덱스로 변환
    if (is.null(col) || length(col) < 2) {
      stop("`col` must specify at least two columns to unite when `unite = TRUE`.")
    }

    # `col`을 숫자로 변환 (1:2, c(1:2) 모두 처리)
    col <- as.numeric(unlist(col))
    col_names <- names(table_data)[col] # col로 지정된 변수 이름

    table_data <- table_data %>%
      unite("area_age", all_of(col_names), sep = "_", remove = remove) # 열 결합
  }

  # pivot_wider로 변환
  table_data <- table_data %>%
    pivot_wider(
      names_from = !!sym(names_from), # 동적으로 names_from 평가
      values_from = Freq
    )

  return(table_data)
}


#' pivot
#'
#' @param data data
#' @param formula formul
#' @param names_from  column
#' @param unite 1,2 colum unite
#' @param col 합쳐질 컬럼
#' @param remove 기존것 지우기
#' @param change_col 열의 위치 바꾸기
#'
#' @return 데이터
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #' kge_bind3a %>%
#' filter(type == "고유어1")%>%
#'   pivot_table(~ area + age + 성조, unite=TRUE)
#' #'
#' kge_bind3a %>%
#'   filter(type == "고유어1")%>%
#'   pivot_table(~  age + area+ 성조, unite=FALSE)
#'
#' #고유어1 공명음  지역별 연령
#' kge_bind3a %>%
#'   filter(type == "고유어1" & a1=="공명음")%>%
#'   pivot_table(~ age + area + 성조, unite=T)  #%>% md()
#'
#'
#'
#' kge_bind3a %>%
#'   filter(type == "고유어1")%>%
#'   pivot_table(~  age +  성조, unite=FALSE)
#'
#' #고유어1 공명음  지역별 연령
#' kge_bind3a %>%
#'   filter(type == "고유어1" & a1=="공명음")%>%
#'   pivot_table(~  area + 성조, unite=F)  #%>% md()

#'
#'
#'   }
#' #'
#'

chisq_pivot_table <- function(data, formula, names_from = NULL, unite = FALSE,
                        col = 1:2, remove = TRUE, change_col = TRUE) {
  library(dplyr)
  library(tidyr)
  library(rlang)

  # formula에서 마지막 변수를 추출
  if (is.null(names_from)) {
    terms <- all.vars(formula) # formula에서 모든 변수 이름 추출
    names_from <- tail(terms, 1) # 마지막 변수 선택
  } else {
    # names_from이 변수명인지 확인하여 처리
    names_from <- enquo(names_from) # 변수명을 캡처
    names_from <- eval_tidy(names_from) # 캡처한 변수명을 평가
  }

  # xtabs로 교차표 생성
  table_data <- data %>%
    xtabs(data = ., formula) %>%
    as.data.frame()

  # change_col 옵션에 따라 1열과 2열 위치 변경
  if (change_col) {
    col_order <- c(2, 1, 3:ncol(table_data)) # 2열과 1열의 순서를 바꾼 후 나머지 유지
    table_data <- table_data[, col_order]   # 열 순서 변경
  }

  # unite 옵션이 TRUE일 경우
  if (unite) {
    # col이 숫자형 벡터인지 확인하고 유효한 열 인덱스로 변환
    if (is.null(col) || length(col) < 2) {
      stop("`col` must specify at least two columns to unite when `unite = TRUE`.")
    }

    # `col`을 숫자로 변환 (1:2, c(1:2) 모두 처리)
    col <- as.numeric(unlist(col))
    col_names <- names(table_data)[col] # col로 지정된 변수 이름

    table_data <- table_data %>%
      unite("area_age", all_of(col_names), sep = "_", remove = remove) # 열 결합
  }

  # pivot_wider로 변환
  table_data <- table_data %>%
    pivot_wider(
      names_from = !!sym(names_from), # 동적으로 names_from 평가
      values_from = Freq
    )

  return(table_data)
}
