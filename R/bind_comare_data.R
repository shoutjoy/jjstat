#' Table-building functions that pairwise compare two pieces of data
#'
#' @param dff1 data1
#' @param dff2 data2
#' @param digits rounding
#' @param pair pari option default TRUE
#'
#' @return table
#' @export
#'
#' @examples
#' \dontrun{
#'
## devtools::install_github("M-E-Rademaker/cSEM")
## devtools::install_github("M-E-Rademaker/cSEM.DGP")
# ##
# library(cSEM.DGP)
# library(cSEM)
#
#
# HS.model <- ' visual  =~ x1 + x2 + x3
#               textual =~ x4 + x5 + x6
#               speed   =~ x7 + x8 + x9
#               '
#
# fit <- cfa(HS.model, data = HolzingerSwineford1939)
# summary(fit)
#
# HS_model <- ' visual  =~ 0.77*x1 + 0.42*x2 + 0.58*x3
#               textual =~ 0.85*x4 + 0.86*x5 + 0.84*x6
#               speed   =~ 0.57*x7 + 0.72*x8 + 0.67*x9
#               visual ~~  0.46*textual
#               visual ~~  0.47*speed
#               textual ~~  0.28*speed
#           '
# HS_simdata = generateData(HS_model, .return_type = "data.frame",
#                           .empirical = TRUE, .N= 301)
#
# fit_sim <- cfa(HS.model, data = HS_simdata)
# summary(fit_sim)
#
#
# dff1 = sem_effect(fit,"=~","~~")%>%select(1,2,3,4)
# dff2 = sem_effect(fit_sim,"=~","~~")%>%select(1,2,3,4)
#
# #The very tiring way
# cbind.data.frame(dff1[,1:2], dff2[,2],
#                  dff1[,3], dff2[,3],
#                  dff1[,4], dff2[,4])%>%
#   `colnames<-`(c("Path","est1","est2",
#                  "std1","std2", "se1","se2"))%>%
#   tibble()%>%print(n=Inf)
#
# #It's cool to do it this way!
# bind_compare_data(dff1, dff2,2)
#'
#' }
#'
bind_compare_data<- function(dff1, dff2, digits=3, pair=TRUE) {


  if(pair){
    # 두 데이터프레임의 열 개수 확인
    n_col_dff1 <- ncol(dff1)
    n_col_dff2 <- ncol(dff2)

    # 열 개수가 다르면 경고 메시지 출력
    if (n_col_dff1 != n_col_dff2) {
      warning("두 데이터프레임의 열 개수가 다릅니다.")
      return(NULL)
    }

    # 열 이름 설정
    colnames_dff1 <- paste0(colnames(dff1), "_1")
    colnames_dff2 <- paste0(colnames(dff2), "_2")

    # 빈 데이터프레임 생성
    combined_data <- data.frame(matrix(nrow = nrow(dff1), ncol = n_col_dff1 * 2))

    # 열 이름 설정
    colnames(combined_data) <- c(colnames_dff1, colnames_dff2)

    combined_data = cbind.data.frame(dff1[, 1], dff2[, 1])
    # 열별로 데이터를 합칩니다.
    for (i in 2:n_col_dff1) {
      # 열 이름이 중복되지 않도록 조정
      colname_dff1 <- paste0(colnames(dff1)[i], "_1")
      colname_dff2 <- paste0(colnames(dff2)[i], "_2")

      combined_data = cbind(combined_data, dff1[, i], dff2[, i])

      # 열 이름 설정
      colnames(combined_data)[(2*i - 1):(2*i)] <- c(colname_dff1, colname_dff2)
    }

    return(combined_data%>% select(-2)%>%
             data.frame() %>%
             Round(digits=digits)  )

  }else{
    # 두 번째 데이터프레임에서 첫 번째 열을 제외한 나머지 열을 가져옵니다.
    dff2_subset <- dff2[-1]

    # 두 데이터프레임을 합칩니다.
    combined_data <- cbind.data.frame(dff1, dff2_subset)

    # 첫 번째 데이터프레임의 Path 열과 두 번째 데이터프레임의 나머지 열을 합칩니다.
    new_colnames <- c(colnames(dff1), colnames(dff2_subset))
    colnames(combined_data) <- new_colnames

    combined_data<- combined_data%>%data.frame() %>%tibble()
    return(combined_data%>%tibble())
  }
}
