
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
#'
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
#' @param type  res, long
#'
#' @return  chisq result
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars%>%  chisq_test_By2("am", "vs", "cyl")
#' mtcars%>% chisq_test_By2("am", "vs", "cyl", type="long")
#'
#' }
#'
#'
chisq_test_By2 = function(dataset, v1, v2, sel=NULL, type="res"){

  res = dataset %>%
    select(all_of(c(v1, v2, sel))) %>%
    group_by_at(vars(all_of(sel)))  %>%
    nest() %>%
    mutate(
      chisq = map_dbl(data, ~ suppressWarnings(chisq.test(.)$statistic) ),
      df = map_dbl(data, ~suppressWarnings(chisq.test(.)$parameter)),
      chisq_p = map_dbl(data, ~suppressWarnings(chisq.test(.)$p.value)))

  long =  res%>% unnest(data)

  switch(type, res=res, long = print(long, n=Inf) )

}


