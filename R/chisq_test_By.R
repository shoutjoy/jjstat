
#' one sample chisq test by group
#'
#' @param dataset data.frame
#' @param sel select column group
#' @param correct chisq correct
#' @param simulate.p.value simulate.p.value
#'
#' @return Chi-square results by group
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars%>%chisq_test_By1("cyl")
#'
#' mtcars%>%chisq_test_By1("am")
#' iris%>%chisq_test_By1("Species")
#' }
chisq_test_By1 <- function(dataset, sel = NULL,
                           correct = FALSE,
                           simulate.p.value=FALSE) {

  # 데이터 변환 및 요약
  data <- dataset %>%
    jjstat::as_trt(sel) %>%
    group_by_at(vars(all_of(sel))) %>%
    summarise(n = n()) %>%
    mutate("perc(%)" = n / sum(n)*100)

  # 카이제곱 검정 및 결과 저장
  results <- lapply(unique(data[[sel]]), function(cat) {
    subset_data <- subset(data, data[[sel]] == cat)
    chisq_result <- suppressWarnings(chisq.test(subset_data[, c("n", "perc(%)")],
                                                correct = correct,
                                                simulate.p.value=simulate.p.value) )%>% broom::tidy()
  })

  # # 결과 데이터프레임 생성 및 가공
  results_df <- bind_rows(results, .id = sel) %>%
    dplyr::select(-method) %>%
    dplyr::rename(df = parameter, chisq = statistic)

  # # 결과와 원본 데이터 병합
  res <- bind_cols(data, results_df[, -1]) %>%
    dplyr::select(1,2,3,4,6,5)

  return(res)
}



#' twosample chisq.test by group
#'
#' @param data data,frame
#' @param v1 first variable
#' @param v2 second variable
#' @param sel group
#' @param correct correct
#' @param simulate.p.value simulate.p.value
#' @param warning warning
#' @param type "all", "res"

#'
#' @return  chisq result
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mtcars%>%  chisq_test_By2("am", "vs", "cyl")
#'
#' mtcars%>% chisq_test_By2("am", "vs", sel= "cyl")
#'
#' Mtcars <- mtcars  # 데이터를 원본과 겹치지 않도록 복사
#' Mtcars$am = factor(Mtcars$am, levels=c(0,1), labels= c("automatic","manual" ))
#' Mtcars$vs = factor(Mtcars$vs, levels=c(0,1), labels= c("V-shaped","straight" ))
#' Mtcars$cyl = factor(Mtcars$cyl, levels=c(4,6,8),
#'              labels= c("cyl-4","cyl-6","cyl-8"))
#'
#' chisq_test_By3(data = Mtcars, v1 = "am", v2 = "vs", sel = "cyl")
#'
#' }
#'
#'
#'
#'
chisq_test_By2 <- function(data,
                           v1,
                           v2,
                           sel,
                           correct=FALSE,
                           simulate.p.value=FALSE,
                           warning = FALSE,
                           type= "all") {
  # 데이터 처리 및 chisq.test() 진행
  tbl_df <-data %>%
    select({{v1}},{{v2}}, {{sel}})%>%
    group_by({{sel}})%>%
    table()



  if(warning){
    results <- lapply(unique(data[[sel]]), function(group) {
      tbl <- table(data[data[[sel]] == group, c(v1, v2)])
      chisq <- chisq.test(tbl, correct = correct,
                          simulate.p.value =simulate.p.value )$statistic
      df <- chisq.test(tbl, correct = correct,
                       simulate.p.value =simulate.p.value )$parameter
      p <- chisq.test(tbl, correct = correct,
                      simulate.p.value =simulate.p.value )$p.value
      n <- sum(tbl)  # 각 그룹별 빈도수 계산
      data.frame(level = group, n = n, chisq, df, p)
    })
  }else{
    results <- lapply(unique(data[[sel]]), function(group) {
      tbl <- table(data[data[[sel]] == group, c(v1, v2)])
      chisq <- chisq.test(tbl, correct = correct,
                          simulate.p.value =simulate.p.value )$statistic
      df <- chisq.test(tbl, correct = correct,
                       simulate.p.value =simulate.p.value )$parameter
      p <- chisq.test(tbl, correct = correct,
                      simulate.p.value =simulate.p.value )$p.value
      n <- sum(tbl)  # 각 그룹별 빈도수 계산
      data.frame(level = group, n = n, chisq, df, p)
    })%>% suppressWarnings()
  }

  # 결과 출력
  result_df <- do.call(what = rbind, results)

  #nest
  tbl_nest <-data %>%
    select({{v1}},{{v2}}, {{sel}})%>%
    group_by_at({{sel}})%>%
    nest()



  # 그룹변수의 레벨과 빈도수(n)를 합치기
  result_df <- cbind(level = factor(result_df$level),
                     result_df[, -1])%>%
    p_mark_sig("p")

  all = list(table=tbl_df, nest=tbl_nest, chisq=result_df)

  switch(type, all = all, res= result_df)
}

# chisq_test_By2 = function(dataset, v1, v2, sel=NULL, type="res"){
#
#   res = dataset %>%
#     select(all_of(c(v1, v2, sel))) %>%
#     group_by_at(vars(all_of(sel)))  %>%
#     nest() %>%
#     mutate(
#       chisq = map_dbl(data, ~ suppressWarnings(chisq.test(.)$statistic) ),
#       df = map_dbl(data, ~suppressWarnings(chisq.test(.)$parameter)),
#       chisq_p = map_dbl(data, ~suppressWarnings(chisq.test(.)$p.value)))
#
#   long =  res%>% unnest(data)
#
#   switch(type, res=res, long = print(long, n=Inf) )
#
# }
#

