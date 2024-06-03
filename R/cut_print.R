#' Functions that cutoff a given value
#'
#' @param data data.frame
#' @param cut cutoff  default 0
#' @param digits round 3
#' @param rm_row remove row
#' @param rm_cut remove cutoff rows
#' @param star over cut marked star pasted
#' @param each  each col using cut
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mtcars%>% cut_print(cut = 280)
#' mtcars%>% cut_print(cut = 280, rm_row = T)
#' mtcars%>% cut_print(cut = 280, rm_row = T,rm_cut=T)
#'
#' #'
#' # 예제 데이터
#' data <- data.frame(
#'   name = c("C_S1", "C_S2", "C_S3", "C_S4", "C_S5", "A_M1", "A_M2",
#'            "A_M3", "B_A2", "B_A3", "D_P1", "D_P2", "D_P3"),
#'   block = c("자기효능감", "자기효능감", "자기효능감", "자기효능감",
#'             "자기효능감", "진로동기", "진로동기", "진로동기",
#'             "진로태도", "진로태도", "진로준비", "진로준비", "진로준비"),
#'   자기효능감 = c(0.88334202, 0.82571836, 0.79357537, 0.78069220,
#'             0.63356092, 0.79936329, 0.75998517, 0.66930167, 0.08384875,
#'             0.67950207, 0.75093254, 0.70621491, 0.33677926),
#'   진로동기 = c(0.819634450, 0.756922172, 0.732994108, 0.617655739,
#'            0.507011139, 0.859130054, 0.851836536, 0.815780379, 0.006420011,
#'            0.699152545, 0.769048149, 0.688524060, 0.336711208),
#'   진로태도 = c(0.6497353, 0.6010873, 0.5346407, 0.5128086,
#'            0.3223576, 0.6769730, 0.6030949, 0.4615263, 0.1463340,
#'            0.9964532, 0.5030203, 0.6625537, 0.4041093),
#'   진로준비 = c(0.76921767, 0.67147592, 0.58862568, 0.63738845,
#'            0.50527519, 0.69871013, 0.68835534, 0.66810321, 0.03263257,
#'            0.69001534, 0.86708694, 0.84986139, 0.55547251)
#' )
#'
#' # 각 열이 numeric 변수인지 확인
#' numeric_cols <- sapply(data, is.numeric)
#' print(numeric_cols)
#'
#' # 함수 실행 예시
#' result <- cut_print(data, cut = 0.01, digits = 2, rm_row = FALSE,
#'                     rm_cut = TRUE, star = FALSE, each = c(0.78, 0.8, 0.8, 0.8))
#' print(result)

#'
#' #'
#' }
cut_print <- function(data, cut = 0, digits = 3,
                      rm_row = FALSE, rm_cut = FALSE,
                      star = FALSE, each = NULL) {
  # Step 1: 행의 합이 0인 경우 제거 (numeric 변수만 대상)
  if (rm_row) {
    numeric_cols <- sapply(data, is.numeric)
    data <- data[rowSums(data[, numeric_cols], na.rm = TRUE) != 0, ]
  }

  # Step 2: 각 열의 값을 소수점 반올림
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {
      data[[i]] <- round(data[[i]], digits)
    }
  }

  # Step 3: each 값에 대한 처리
  numeric_cols <- sapply(data, is.numeric)

  if (!is.null(each)) {
    extended_each <- ifelse(numeric_cols, each, NA)

    for (i in 1:ncol(data)) {
      if (is.numeric(data[[i]])) {
        data[[i]] <- ifelse(is.na(data[[i]]) | data[[i]] < extended_each[i], "", data[[i]])
      }
    }
  } else {
    # Step 4: cut에 대한 처리
    if (star) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- ifelse(is.na(data[[i]]) | abs(data[[i]]) <= cut,
                              data[[i]], paste0(data[[i]], "*"))
        }
      }
    } else {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- ifelse(is.na(data[[i]]) | abs(data[[i]]) <= cut, "", data[[i]])
        }
      }
    }
  }

  res <- data.frame(data)

  # Step 5: rm_cut 옵션에 따른 처리 (단, star가 TRUE일 때는 적용되지 않음)
  if (rm_cut && !star) {
    res <- res %>%
      filter(rowSums(res == "", na.rm = TRUE) != ncol(res))
  }

  return(res)
}

#' overall cut
#'
#' @param data data
#' @param cut cut =0.5
#' @param digits 3
#' @param imp "
#'
#' @return data
#' @export
#'
cut_print_all <- function(data, cut = 0, imp="", digits=3) {
  data = data.frame(data)
  data = round2(data, digits)
  # 각 열의 값을 cut 값과 비교하여 조건에 맞는 값들만 남기고 나머지는 ""로 대체합니다.
  res=  data%>%
    mutate(across(where(is.numeric), ~ ifelse(abs(.) <= cut, imp, .)))%>%
    data.frame()

  res
}
