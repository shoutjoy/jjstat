#' Chisq test Contrast test
#'
#' @param data data
#' @param ... contrast
#' @param test chisq.test, fisher.test
#' @param name rowname, when  pairwise
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' d <- matrix(c(584, 24, 1721, 56, 2400, 90, 8164, 289),
#'             nrow = 4, byrow = TRUE,
#'             dimnames = list(c("kangnam", "Moksdong", "nowon", "dongtan"), c("Yes", "No")))
#'
#' d%>%chisq.test()
#' chisq_contrast(d)
#' chisq_contrast(d, c(1,2,3,4))
#'
#'
#' chisq_contrast(d, c(1,1,2,3),c(2,1,2,2),c(2,2,1,2),c(2,2,2,1))
#' # fisher.test
#' chisq_contrast(d, c(1,2,2,2),c(2,1,2,2),c(2,2,1,2),c(2,2,2,1), test=fisher.test)
#' # fisher.exact test
#' chisq_contrast(d, c(1,2,2,3),c(2,1,2,2),c(2,2,1,2),c(2,2,2,1), test=fisher.test)
#'
#' chisq_contrast(d, c(1,2,2,2),c(2,1,1,2),c(2,2,1,2),c(2,2,2,1), test=fisher.test)
#' chisq_contrast(d, c(1,2,2,2),c(2,3,1,2),c(2,2,1,2),c(2,2,2,1), test=fisher.test)
#' chisq_contrast(d, c(1,2,2,2),c(2,3,1,2),c(2,2,1,3),c(2,2,2,1), test=fisher.test)
#' chisq_contrast(d, c(1,2,2,2),c(2,3,1,2),c(2,2,1,3),c(3,4,2,1), test=fisher.test)
#'
#'
#' #other data
#' data_text = "Tobacco_Dependence (0,5] (5,10] (10,15] (15,20] (20,98]
#' No_Nicotine_Dependence 130 210 43 114 20
#' Nicotine_Dependence 119 267 91 254 67"
#'
#' #method2
#' data_text%>% text2df(header=T) %>% make_df_text()
#' nt
#' nt = data_text%>% text2df(header=T)%>% jjstat::col2row()
#' nt %>%tibble()
#' nt=Char2num(nt,1:5)
#' nt%>%t() %>%as.data.frame()%>%
#'   chisq_contrast(c(1,2,2,2,2),
#'                  c(2,1,2,2,2),
#'                  c(2,2,1,2,2),
#'                  c(2,3,4,1,2))
#' }
#'
#'
chisq_contrast <- function(data, ..., test=chisq.test, name= FALSE) {
  data = as.data.frame(data)
  # 사용자로부터 대조군을 입력받음
  contrasts <- list(...)


  Rownames = rownames(data)

  contrast_strings <- sapply(list(...), function(contrast) {
    paste("c(", paste(contrast, collapse = ","), ")", sep = "")
  })
  # 결과를 저장할 리스트 초기화
  results <- list()
  chi_sq_tidy<-list()
  chi_sq<-list()

  # 대조군의 개수
  n_contrasts <- length(contrasts)
  # 대조군별로 table_sum 함수를 적용하고 카이제곱 검정 수행
  for (i in 1:n_contrasts) {
    contrast <- contrasts[[i]]
    result <- table_sum(data, contrast)
    chi_sq[[i]] <- test(result)  #test
    results[[i]] <- result
    chi_sq_tidy[[i]] <- broom::tidy(test(result))
  }

  if(identical(test, stats::chisq.test)){
    # case chisq.test
    chi_res <- do.call(rbind, chi_sq_tidy) %>%
      dplyr::mutate(
        contr = contrast_strings) %>%
      p_mark_sig() %>%
      dplyr::select( contr, statistic, p.value, sig, parameter, method)%>%
      dplyr::rename(chisq=statistic, df=parameter)

  }else {
    if (all(names(chi_sq_tidy[[1]]) == names(chi_sq_tidy[[2]]))) {
      # "alternative" 열이 없는 경우에만 조정
      chi_res <- do.call(rbind, chi_sq_tidy) %>%
        dplyr::mutate(contr = contrast_strings) %>%
        p_mark_sig() %>%
        dplyr::select(contr,  estimate, p.value,sig, conf.low, conf.high, method) %>%
        rename(odds_ratio = estimate)
    } else {
      # "alternative" 열이 있는 경우에는 그대로 사용
      chi_res <- lapply(chi_sq_tidy,
                        function(x) dplyr::select(x, p.value, method)) %>%
        do.call(what=rbind) %>%
        dplyr::mutate(contr = contrast_strings)%>%p_mark_sig("p.value")%>%
        dplyr::select(4,2,5,3)

    }
  }

  names(results) = contrasts
  if(name){
    chi_res = cbind.data.frame(selected = Rownames,  chi_res)
  }
  Res = list( results, chi_res)
  Res

}
