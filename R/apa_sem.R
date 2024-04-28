
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
                         "(est = ", round(data[i, 3], 3),data[i, 8],
                         ", std = ",round(data[i, 4], 3),
                         ", p ",
                         ifelse(data[i, 7] < 0.001, "< .001",
                                paste0("= ", round(data[i, 7], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 B = ",
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
                         "(est = ", round(data[i, 2], 3),data[i, 7],
                         ", std = ",round(data[i, 3], 3),
                         ", p ",
                         ifelse(data[i, 6] < 0.001, "< .001",
                                paste0("= ", round(data[i, 6], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 B = ",
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






#' SEM direct effect interpretation
#'
#' @param data DE_effect obj
#' @param md markdown_table output
#' @param caption caption= title
#' @param print print TRUE
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
sem_apa <- function(data, md = FALSE, caption= "Table: ", print=TRUE) {
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
          "(est = ", round(data[i, 3], 3),data[i, 8],
          ", std = ",round(data[i, 4], 3),
          ", p ",ifelse(data[i, 7] < 0.001, "< .001",
                                paste0("= ", round(data[i, 7], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 B = ",
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
                         "(est = ", round(data[i, 2], 3),data[i, 7],
                         ", std = ",round(data[i, 3], 3),
                         ", p ",
                         ifelse(data[i, 6] < 0.001, "< .001",
                                paste0("= ", round(data[i, 6], 3))),
                         "). ",
                         "이는 '", Rsp[i, 1], "'이 한 단위 증가할수록 '",
                         Rsp[i, 2],"'의 비표준화계수(est)가 B = ",
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

  if(print){
    print(data)
  }




  if (md) {
    print(data %>% jjstat::md(digits = 3, caption = caption))
  }

  cat("\n\n연구모형에 대한 구조모형 분석결과, 각 가설(직접효과)는 다음과 같이 나타났다. \n",
      inter, "\n\n")
}


#' sem_apa_ie_CI
#'
#' @param data lavaan object
#' @param caption "매개효과 분석 결과",
#' @param md markdown
#' @param est est sel 2
#' @param std std sel 3
#' @param sig sig sel 6
#' @param CI CI seledct 7
#' @param Z Z=FALSE
#' @param tot_filter tot_filter="총",
#' @param ind_filter ind_filter="IE"
#'
#' @return table
#' @export
#'

sem_apa_ie_CI <- function(data, caption="매개효과 분석 결과 ",
                          md = FALSE, est=2, std=3, sig = 6, CI=7 ,Z=FALSE,
                          tot_filter="총",
                          ind_filter="IE" ) {

  data<- data %>% separate(z, c("Z","sig"), sep=" ")
  data = data %>% replace_df(pattern="ns", imp="") %>%
    unite(Est, est, sig, sep= "", remove=FALSE)
  data$Est = format(data$Est, #justify="right",
                    digits = 3)
  # data$Path = format(data$Path, justify="right")

  inter <- c()
  for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
    inter[i] <- paste0( "가설[",i,"] : '",
                        data[i, 1],
                        "'에 관한 매개효과",
                        ifelse(str_detect( data[i, 1], ind_filter), "(간접효과) ",
                               ifelse(str_detect( data[i, 1], tot_filter) , "(전체효과) ","(간접효과)"  )),
                        "분석 결과, 통계적으로",
                        ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                               " 유의미한 효과가 나타났다"),
                        #  "(est = ", round(data[i, est], 3),
                        "(est = ", data[i, est],
                        ", std = ", data[i, std],
                        ", 95%CI", data[i, CI],
                        "). \n\n"  )
  }

  # data_1 = data %>% replace_df(pattern="ns", imp="") %>%
  #            unite(est, est, sig, sep= "")
  # data_1$est = format(data_1$est, justify="left")
  # data_1$Path = format(data_1$Path, justify="left")
  if(Z){
    data   =  data%>%select(-est, -sig, Z)%>%
      dplyr::rename("95% CI" = CI_95p)
  }else{
    data   =  data%>%select(-est, -sig, -Z)%>%
      dplyr::rename("95% CI" = CI_95p)
  }
  print( data)

  if(md){  print(data%>% jjstat::md(digits=3, caption=caption))}

  cat("\n\n 매개효과(간접효과)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
      inter,"\n\n")

}



#' sem_apa_ie_CI
#'
#' @param data lavaan object
#' @param caption "매개효과 분석 결과",
#' @param md markdown
#' @param est est sel 2
#' @param std std sel 3
#' @param sig sig sel 6
#' @param CI CI seledct 7
#' @param Z Z=FALSE
#' @param tot_filter tot_filter="총",
#' @param ind_filter ind_filter="IE"
#'
#' @return table
#' @export
#'

interpretation_ie_CI <- function(data, caption="매개효과 분석 결과 ",
                          md = FALSE, est=2, std=3, sig = 6, CI=7 ,Z=FALSE,
                          tot_filter="총",
                          ind_filter="IE" ) {

  data<- data %>% separate(z, c("Z","sig"), sep=" ")
  data = data %>% replace_df(pattern="ns", imp="") %>%
    unite(Est, est, sig, sep= "", remove=FALSE)
  data$Est = format(data$Est, #justify="right",
                    digits = 3)
  # data$Path = format(data$Path, justify="right")

  inter <- c()
  for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
    inter[i] <- paste0( "가설[",i,"] : '",
                        data[i, 1],
                        "'에 관한 매개효과",
                        ifelse(str_detect( data[i, 1], ind_filter), "(간접효과) ",
                               ifelse(str_detect( data[i, 1], tot_filter) , "(전체효과) ","(간접효과)"  )),
                        "분석 결과, 통계적으로",
                        ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                               " 유의미한 효과가 나타났다"),
                        #  "(est = ", round(data[i, est], 3),
                        "(est = ", data[i, est],
                        ", std = ", data[i, std],
                        ", 95%CI", data[i, CI],
                        "). \n\n"  )
  }

  # data_1 = data %>% replace_df(pattern="ns", imp="") %>%
  #            unite(est, est, sig, sep= "")
  # data_1$est = format(data_1$est, justify="left")
  # data_1$Path = format(data_1$Path, justify="left")
  if(Z){
    data   =  data%>%select(-est, -sig, Z)%>%
      dplyr::rename("95% CI" = CI_95p)
  }else{
    data   =  data%>%select(-est, -sig, -Z)%>%
      dplyr::rename("95% CI" = CI_95p)
  }
  print( data)

  if(md){  print(data%>% jjstat::md(digits=3, caption=caption))}

  cat("\n\n 매개효과(간접효과)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
      inter,"\n\n")

}


#' sem_apa_ie
#'
#' @param data data
#' @param msg msg="IE"
#' @param md md = FALSE
#' @param est  est=3
#' @param std  std=4
#' @param sig  sig = 7
#' @param p  p=6
#'
#' @return ie report
#' @export
#'
#' @examples
#'
#' \dontrun{
#' example(sem)
#' IE_effect(fit)
#' IE_effect(fit) %>% sem_apa_ie()
#' }
sem_apa_ie <- function(data, msg="IE", md = FALSE, est=3, std=4, sig = 7, p=6  ) {
  inter <- c()

  if(msg=="IE"){

    for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
      inter[i] <- paste0( "가설[",i,"] : '",
                          data[i, 1],
                          "'에 관한 매개효과",
                          ifelse(str_detect( data[i, 1], "IE"), "(간접효과) ",
                                 ifelse(str_detect( data[i, 1], "TE") , "(전체효과) ","(간접효과) "  )),
                          "분석 결과, 통계적으로",
                          ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                                 " 유의미한 효과가 나타났다"),
                          "(est = ", round(data[i, est], 3),
                          ", std = ", round(data[i, std], 3),
                          ", p ",
                          ifelse(data[i, p]< 0.001,"< .001", paste0("= ", round(data[i, p], 3))),
                          "). \n\n"
      )
    }

    print(data)
    if(md){  print(data%>% jjstat::md(digits=3))}

    cat("\n\n 매개효과(간접효과)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
        inter,"\n\n")


  }else if(msg =="TE"){
    for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
      inter[i] <- paste0( "가설[",i,"] : '",
                          data[i, 1],
                          "'에 관한 매개효과",
                          ifelse(str_detect( data[i, 1], "IE"), "(간접효과) ",
                                 ifelse(str_detect( data[i, 1], "TE") , "(전체효과) ","(직접효과) "  )),
                          "분석 결과, 통계적으로",
                          ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                                 " 유의미한 효과가 나타났다"),
                          "(est = ", round(data[i, est], 3),
                          ", std = ", round(data[i, std], 3),
                          ", p ",
                          ifelse(data[i, p]< 0.001,"< .001", paste0("= ", round(data[i, p], 3))),
                          "). \n\n"
      )
    }

    print(data)
    if(md){  print(data%>% jjstat::md(digits=3))}

    cat("\n\n 매개효과(전체효과)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
        inter,"\n\n")

  } else if(msg =="Diff"){
    for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
      inter[i] <- paste0( "가설[",i,"] : '",
                          data[i, 1],"(",data[i, 2], ")",
                          "'에 관한 매개효과",
                          ifelse(str_detect( data[i, 1], "IE"), "(간접효과) ",
                                 ifelse(str_detect( data[i, 1], "TE") , "(전체효과) ","(효과차이검정) "  )),
                          "분석 결과, 통계적으로",
                          ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                                 " 유의미한 효과가 나타났다"),
                          "(est = ", round(data[i, est], 3),
                          ", std = ", round(data[i, std], 3),
                          ", p ",
                          ifelse(data[i, p]< 0.001,"< .001", paste0("= ", round(data[i, p], 3))),
                          "). \n\n"
      )
    }

    print(data)
    if(md){  print(data%>% jjstat::md(digits=3))}

    cat("\n\n 매개효과(차이효과 검정)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
        inter,"\n\n")
  }
}



#' interpretation_ie
#'
#' @param data data
#' @param msg msg="IE"
#' @param md md = FALSE
#' @param est  est=3
#' @param std  std=4
#' @param sig  sig = 7
#' @param p  p=6
#'
#' @return ie report
#' @export
#'
#' @examples
#'
#' \dontrun{
#' example(sem)
#' IE_effect(fit)
#' IE_effect(fit) %>% interpretation_ie()
#' }
interpretation_ie <- function(data, msg="IE", md = FALSE, est=3, std=4, sig = 7, p=6  ) {
  inter <- c()

  if(msg=="IE"){

    for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
      inter[i] <- paste0( "가설[",i,"] : '",
                          data[i, 1],
                          "'에 관한 매개효과",
                          ifelse(str_detect( data[i, 1], "IE"), "(간접효과) ",
                                 ifelse(str_detect( data[i, 1], "TE") , "(전체효과) ","(간접효과) "  )),
                          "분석 결과, 통계적으로",
                          ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                                 " 유의미한 효과가 나타났다"),
                          "(est = ", round(data[i, est], 3),
                          ", std = ", round(data[i, std], 3),
                          ", p ",
                          ifelse(data[i, p]< 0.001,"< .001", paste0("= ", round(data[i, p], 3))),
                          "). \n\n"
      )
    }

    print(data)
    if(md){  print(data%>% jjstat::md(digits=3))}

    cat("\n\n 매개효과(간접효과)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
        inter,"\n\n")


  }else if(msg =="TE"){
    for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
      inter[i] <- paste0( "가설[",i,"] : '",
                          data[i, 1],
                          "'에 관한 매개효과",
                          ifelse(str_detect( data[i, 1], "IE"), "(간접효과) ",
                                 ifelse(str_detect( data[i, 1], "TE") , "(전체효과) ","(직접효과) "  )),
                          "분석 결과, 통계적으로",
                          ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                                 " 유의미한 효과가 나타났다"),
                          "(est = ", round(data[i, est], 3),
                          ", std = ", round(data[i, std], 3),
                          ", p ",
                          ifelse(data[i, p]< 0.001,"< .001", paste0("= ", round(data[i, p], 3))),
                          "). \n\n"
      )
    }

    print(data)
    if(md){  print(data%>% jjstat::md(digits=3))}

    cat("\n\n 매개효과(전체효과)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
        inter,"\n\n")

  } else if(msg =="Diff"){
    for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
      inter[i] <- paste0( "가설[",i,"] : '",
                          data[i, 1],"(",data[i, 2], ")",
                          "'에 관한 매개효과",
                          ifelse(str_detect( data[i, 1], "IE"), "(간접효과) ",
                                 ifelse(str_detect( data[i, 1], "TE") , "(전체효과) ","(효과차이검정) "  )),
                          "분석 결과, 통계적으로",
                          ifelse(data[i, sig] == "ns", " 유의미한 효과는 없었다",
                                 " 유의미한 효과가 나타났다"),
                          "(est = ", round(data[i, est], 3),
                          ", std = ", round(data[i, std], 3),
                          ", p ",
                          ifelse(data[i, p]< 0.001,"< .001", paste0("= ", round(data[i, p], 3))),
                          "). \n\n"
      )
    }

    print(data)
    if(md){  print(data%>% jjstat::md(digits=3))}

    cat("\n\n 매개효과(차이효과 검정)에 관한 가설검정 결과, 다음과 같이 나타났다. \n\n",
        inter,"\n\n")
  }
}
