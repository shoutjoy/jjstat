#' chisq_gof_posthoc
#'
#' @param Xs data
#' @param type all, res, p, chisq
#' @param method p.adust
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' X <- c(49, 30, 63, 59)
#'
#' chisq_gof_posthoc(X, "res")
#' chisq_gof_posthoc(X,"all")$gof_test
#' chisq_gof_posthoc(X,"all")$p
#' chisq_gof_posthoc(X,"all")$chisq
#' chisq_gof_posthoc(X, typ="all")
#' chisq_gof_posthoc(X, typ="res")
#' chisq_gof_posthoc(X, type="p")
#' chisq_gof_posthoc(X, type="chisq")
#'
#' chisq_gof_posthoc(X, method="bonferroni")
#'
#' X1 <- c(4, 3, 6, 19)
#' chisq_gof_posthoc(X1)
#' chisq_gof_posthoc(X1, typ="all")
#' chisq_gof_posthoc(X1, type="p")
#' chisq_gof_posthoc(X1, type="chisq")
#' chisq_gof_posthoc(X1, method="bonferroni")
#'
#'
#' chisq_gof_posthoc(X, method="bonferroni")
#'
#' chisq_gof_posthoc(c(49, 30, 63, 59, 40, 60))
#'
#' lapply(dfat %>%table_sum(c(1,0,0)), unlist)
#' #'
#' }
#'
#'
#'
chisq_gof_posthoc <- function(Xs, type="all", method="fdr") {

  # 입력 데이터가 데이터 프레임인 경우 각 열을 벡터로 변환
  Xs = as.numeric(Xs)

  overall =  chisq.test(Xs) %>%tidy()
  #data combination
  Names <- combn(length(Xs), 2,
                 FUN = function(x) paste0(Xs[x[1]], "_", Xs[x[2]]))


  #repeat chisq.test
  results <- lapply(Names, function(pair) {
    chisq.test(as.numeric(unlist(strsplit(pair, "_"))))
  })%>% suppressWarnings()
  # arrange the results data
  combined_results <- bind_rows(lapply(results, tidy))
  combined_results$pairwise <- Names

  combined_results= combined_results %>%
    dplyr::select(pairwise, chisq=statistic,df=parameter, p.value)


  #p.adjust------------
  ##pairwise
  fun.p <- function(i,j) {
    xi <- Xs[i]
    xj <- Xs[j]
    suppressWarnings(chisq.test(c(xi, xj)))$p.value  }
  # calulate the p
  tab.p <- pairwise.table(fun.p, as.character(Xs),
                          p.adjust.method= method)
  # adjust the p value
  adj.p = tab.p%>%
    long_df("cell_1","cell_2","adj.p") %>%
    drop_na () %>%#arrange(cell_2) %>%
    mutate(cell_2 = substring(cell_2,2,3) ) %>%
    dplyr::select(cell_2, cell_1, adj.p)%>%
    tidyr::unite(pairwise, cell_2, cell_1)

  if(type=="res"){
    cat("\n p adjust method =",method,"\n")
  }


  res = full_join(combined_results,
                  adj.p, by="pairwise")%>%
    p_mark_sig("adj.p")

  Res= list(gof_test = overall,
            p = adj.p, chisq = combined_results,
            posthoc = res)

  # Res
  switch(type, all= Res,res=res, p=adj.p, chisq =combined_results )

}



#' chisq_gof_posthoc
#'
#' @param Xs data
#' @param type all, res, p, chisq
#' @param method p.adust
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #'
#' X <- c(49, 30, 63, 59)
#'
#' chisq_each(X, "res")
#' chisq_each(X,"all")$gof_test
#' chisq_each(X,"all")$p
#' chisq_each(X,"all")$chisq
#' chisq_each(X, typ="all")
#' chisq_each(X, typ="res")
#' chisq_each(X, type="p")
#' chisq_each(X, type="chisq")
#'
#' chisq_each(X, method="bonferroni")
#'
#' X1 <- c(4, 3, 6, 19)
#' chisq_each(X1)
#' chisq_each(X1, typ="all")
#' chisq_each(X1, type="p")
#' chisq_each(X1, type="chisq")
#' chisq_each(X1, method="bonferroni")
#'
#'
#' chisq_each(X, method="bonferroni")
#'
#' chisq_each(c(49, 30, 63, 59, 40, 60))
#'
#' #'
#' }
#'
#'
#'
chisq_each <- function(Xs, type="all", method="fdr") {

  # 입력 데이터가 데이터 프레임인 경우 각 열을 벡터로 변환
  Xs = as.numeric(Xs)

  overall =  chisq.test(Xs) %>%tidy()
  #data combination
  Names <- combn(length(Xs), 2,
                 FUN = function(x) paste0(Xs[x[1]], "_", Xs[x[2]]))


  #repeat chisq.test
  results <- lapply(Names, function(pair) {
    chisq.test(as.numeric(unlist(strsplit(pair, "_"))))
  })%>% suppressWarnings()
  # arrange the results data
  combined_results <- bind_rows(lapply(results, tidy))
  combined_results$pairwise <- Names

  combined_results= combined_results %>%
    dplyr::select(pairwise, chisq=statistic,df=parameter, p.value)


  #p.adjust------------
  ##pairwise
  fun.p <- function(i,j) {
    xi <- Xs[i]
    xj <- Xs[j]
    suppressWarnings(chisq.test(c(xi, xj)))$p.value  }
  # calulate the p
  tab.p <- pairwise.table(fun.p, as.character(Xs),
                          p.adjust.method= method)
  # adjust the p value
  adj.p = tab.p%>%
    long_df("cell_1","cell_2","adj.p") %>%
    drop_na () %>%#arrange(cell_2) %>%
    mutate(cell_2 = substring(cell_2,2,3) ) %>%
    dplyr::select(cell_2, cell_1, adj.p)%>%
    tidyr::unite(pairwise, cell_2, cell_1)

  if(type=="res"){
    cat("\n p adjust method =",method,"\n")
  }


  res = full_join(combined_results,
                  adj.p, by="pairwise")%>%
    p_mark_sig("adj.p")

  Res= list(gof_test = overall,
            p = adj.p, chisq = combined_results,
            posthoc = res)

  # Res
  switch(type, all= Res,res=res, p=adj.p, chisq =combined_results )

}
