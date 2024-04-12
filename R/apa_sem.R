
#' SEM direct effect interpretation
#'
#' @param data DE_effect obj
#' @param md markdown_table output
#'
#' @return report apa
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'  #library(lavaan) # only needed once per session
#'  models <- '
#'    # measurement model
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + y2 + y3 + y4
#'      dem65 =~ y5 + y6 + y7 + y8
#'  # regressions
#'      dem60 ~ ind60
#'      dem65 ~ ind60 + dem60
#'    # residual correlations
#'      y1 ~~ y5
#'      y2 ~~ y4 + y6
#'      y3 ~~ y7
#'      y4 ~~ y8
#'     y6 ~~ y8
#'  '
#'  fits <- sem(models, data=PoliticalDemocracy)
#'  summary(fits, standardized=TRUE)
#'
#'  ##Direct effect
#'  DE_effect(fits)

#'  DE_effect(fits)%>% interpretation_de()
#'
#' }
#'
interpretation_de <- function(data, md = FALSE) {
  # data <- data  # 이 줄은 필요 없는 것 같습니다.

  # Check if 'label' column exists
  if (ncol(data)==8) {
    rsp <- c()
    for (i in 1:nrow(data)) {
      rsp[i] <- ifelse(stringr::str_detect(data[i, 1], "->"),
                       str_split(data[i, 1], "->"),
                       ifelse(stringr::str_detect(data[i, 1], "<-"),
                              str_split(data[i, 1], "<-"),
                              ifelse(stringr::str_detect(data[i, 1], "~~"),
                                     str_split(data[i, 1], "~~"),
                                     str_split(data[i, 1], ":"))))
    }
    rsp <- lapply(rsp, function(x) str_trim(x))
    Rsp <- do.call(rbind, rsp)

    inter <- c()
    for (i in 1:nrow(data)) {
      inter[i] <- paste0("가설[", i, "]: ", data[i, 1], "(", data[i, 2], ")",
                         "에 관한 경로", "분석 결과, 통계적으로",
                         ifelse(data[i, 8] == "ns", " 유의미한 효과는 나타나지 않았다",
                                " 유의미한 효과가 나타났다"),
                         "(est = ", round(data[i, 3], 3), ", p ",
                         ifelse(data[i, 7] < 0.001, "< .001",
                                paste0("= ", round(data[i, 7], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 β = ",
                         round(data[i, 3], 3),
                         ifelse(data[i, 3] > 0,
                                paste0(" 만큼 ", ifelse(data[i, 8] == "ns", "", "유의미한 "),
                                       "증가를 나타낸다. "),
                                paste0("만큼 ", ifelse(data[i, 8] == "ns", "", "유의미한 "),
                                       "감소를 나타낸다. ")),
                         ifelse(data[i, 8] == "ns",
                                "그러나, 통계적으로 유의미한 효과라고 할 수 없다. ", ""),  "\n" )
    }
  } else if(ncol(data)==7){


    rsp <- c()
    for (i in 1:nrow(data)) {
      rsp[i] <- ifelse(stringr::str_detect(data[i, 1], "->"),
                       str_split(data[i, 1], "->"),
                       ifelse(stringr::str_detect(data[i, 1], "<-"),
                              str_split(data[i, 1], "<-"),
                              ifelse(stringr::str_detect(data[i, 1], "~~"),
                                     str_split(data[i, 1], "~~"),
                                     str_split(data[i, 1], ":"))))
    }
    rsp <- lapply(rsp, function(x) str_trim(x))
    Rsp <- do.call(rbind, rsp)

    inter <- c()
    for (i in 1:nrow(data)) {
      inter[i] <- paste0("가설[", i, "]: ", data[i, 1],
                         "에 관한 경로", "분석 결과, 통계적으로",
                         ifelse(data[i, 7] == "ns", " 유의미한 효과는 나타나지 않았다",
                                " 유의미한 효과가 나타났다"),
                         "(est = ", round(data[i, 3], 3), ", p ",
                         ifelse(data[i, 6] < 0.001, "< .001",
                                paste0("= ", round(data[i, 6], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 β = ",
                         round(data[i, 2], 3),
                         ifelse(data[i, 2] > 0,
                                paste0(" 만큼 ", ifelse(data[i, 7] == "ns",
                                                      "", "유의미한 ") ,"증가를 나타낸다. "),
                                paste0("만큼 ", ifelse(data[i, 7] == "ns", "",
                                                     "유의미한 "),"감소를 나타낸다. ")),
                         ifelse(data[i, 7] == "ns",
                                "그러나, 통계적으로 유의미한 효과라고 할 수 없다. ", ""),  "\n" )
    }

  }

  print(data)

  if (md) {
    print(data %>% jjstat::md(digits = 3))
  }

  cat("\n\n연구모형에 대한 구조모형 분석결과, 각 가설(직접효과)는 다음과 같이 나타났다. \n",
      inter, "\n\n")
}






#' SEM direct effect interpretation
#'
#' @param data DE_effect obj
#' @param md markdown_table output
#'
#' @return report apa
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'  #library(lavaan) # only needed once per session
#'  models <- '
#'    # measurement model
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + y2 + y3 + y4
#'      dem65 =~ y5 + y6 + y7 + y8
#'  # regressions
#'      dem60 ~ ind60
#'      dem65 ~ ind60 + dem60
#'    # residual correlations
#'      y1 ~~ y5
#'      y2 ~~ y4 + y6
#'      y3 ~~ y7
#'      y4 ~~ y8
#'     y6 ~~ y8
#'  '
#'  fits <- sem(models, data=PoliticalDemocracy)
#'  summary(fits, standardized=TRUE)
#'
#'  ##Direct effect
#'  DE_effect(fits)

#'  DE_effect(fits)%>% sem_apa()
#'
#' example(sem)
#' DE_effect(fit)
#' DE_effect(fit) %>% sem_apa()
#'
#' }
#'
sem_apa <- function(data, md = FALSE) {
  # data <- data  # 이 줄은 필요 없는 것 같습니다.

  # Check if 'label' column exists
  if (ncol(data)==8) {
    rsp <- c()
    for (i in 1:nrow(data)) {
      rsp[i] <- ifelse(stringr::str_detect(data[i, 1], "->"),
                       str_split(data[i, 1], "->"),
                       ifelse(stringr::str_detect(data[i, 1], "<-"),
                              str_split(data[i, 1], "<-"),
                              ifelse(stringr::str_detect(data[i, 1], "~~"),
                                     str_split(data[i, 1], "~~"),
                                     str_split(data[i, 1], ":"))))
    }
    rsp <- lapply(rsp, function(x) str_trim(x))
    Rsp <- do.call(rbind, rsp)

    inter <- c()
    for (i in 1:nrow(data)) {
      inter[i] <- paste0("가설[", i, "]: ", data[i, 1], "(", data[i, 2], ")",
                         "에 관한 경로", "분석 결과, 통계적으로",
                         ifelse(data[i, 8] == "ns", " 유의미한 효과는 나타나지 않았다",
                                " 유의미한 효과가 나타났다"),
                         "(est = ", round(data[i, 3], 3), ", p ",
                         ifelse(data[i, 7] < 0.001, "< .001",
                                paste0("= ", round(data[i, 7], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 β = ",
                         round(data[i, 3], 3),
                         ifelse(data[i, 3] > 0,
                                paste0(" 만큼 ", ifelse(data[i, 8] == "ns",
                                                      "", "유의미한 ") ,"증가를 나타낸다. "),
                                paste0("만큼 ", ifelse(data[i, 8] == "ns",
                                                     "", "유의미한 "),"감소를 나타낸다. ")),
                         ifelse(data[i, 8] == "ns",
                                "그러나, 통계적으로 유의미한 효과라고 할 수 없다. ", ""),  "\n" )
    }
  } else if(ncol(data)==7){


    rsp <- c()
    for (i in 1:nrow(data)) {
      rsp[i] <- ifelse(stringr::str_detect(data[i, 1], "->"),
                       str_split(data[i, 1], "->"),
                       ifelse(stringr::str_detect(data[i, 1], "<-"),
                              str_split(data[i, 1], "<-"),
                              ifelse(stringr::str_detect(data[i, 1], "~~"),
                                     str_split(data[i, 1], "~~"),
                                     str_split(data[i, 1], ":"))))
    }
    rsp <- lapply(rsp, function(x) str_trim(x))
    Rsp <- do.call(rbind, rsp)

    inter <- c()
    for (i in 1:nrow(data)) {
      inter[i] <- paste0("가설[", i, "]: ", data[i, 1],
                         "에 관한 경로", "분석 결과, 통계적으로",
                         ifelse(data[i, 7] == "ns", " 유의미한 효과는 나타나지 않았다",
                                " 유의미한 효과가 나타났다"),
                         "(est = ", round(data[i, 3], 3), ", p ",
                         ifelse(data[i, 6] < 0.001, "< .001",
                                paste0("= ", round(data[i, 6], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 β = ",
                         round(data[i, 2], 3),
                         ifelse(data[i, 2] > 0,
                                paste0(" 만큼 ",
                                       ifelse(data[i, 7] == "ns", "", "유의미한 "),
                                       "증가를 나타낸다. "),
                                paste0("만큼 ", ifelse(data[i, 7] == "ns",
                                                     "", "유의미한 "),"감소를 나타낸다. ")),
                         ifelse(data[i, 7] == "ns",
                                "그러나, 통계적으로 유의미한 효과라고 할 수 없다. ", ""),  "\n" )
    }

  }

  print(data)

  if (md) {
    print(data %>% jjstat::md(digits = 3))
  }

  cat("\n\n연구모형에 대한 구조모형 분석결과, 각 가설(직접효과)는 다음과 같이 나타났다. \n",
      inter, "\n\n")
}


