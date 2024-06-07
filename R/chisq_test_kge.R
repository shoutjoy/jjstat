
#' kge_chisq_table chisq table observed/Expected table-
#'
#' @param dataset data.frame
#' @param v1 col
#' @param v2 row
#' @param title title
#' @param type res2 default
#' @param digits 3
#' @param yadd up yadd
#' @param Ncol grid
#' @param trans transpose
#' @param simple * style
#' @param ko graph korean
#' @param simulate.p.value exact
#' @param correct exactt
#' @param size_bartext graph text
#' @param strip_size strip
#' @param axis_size axis_size
#' @param text_size text_size
#' @param xlab xlab
#' @param ylab ylab
#' @param cramer cramer cor
#'
#' @return multiple data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' kge_chisq_table(mtcars,"am","cyl")
#'
#'
#' #chisq test result
#' vote_chi <- Chisq_retest(c(183, 35, 824, 50), ncol=2,
#'                         colname = c("Did not vote", "Voted"),
#'                          rowname =c("All other ages","Youngest"))
#'
#' vote_chi
#'
#'
#' # generate matrix
#' voteMat = matrix(c(183, 35, 824, 50), ncol=2)
#' colnames(voteMat) = c("Did not vote", "Voted")
#' rownames(voteMat) = c("All other ages","Youngest")
#'
#' voteMat%>% data.frame() %>%
#'   long_df("voted", rowname="youth")%>% #long format
#'   add_rows_freq() %>%     # add rows
#'   kge_chisq_table("voted","youth")    #chisq test
#'
#'
#' #신진영(2013)
#' shin2013_mat =matrix(c(14,1,23,3,4,0,9,0,13,18,89,8,11,40,46,12,3,4,22,0), ncol= 5)
#' # colnames(shin2013_mat)=c("지능지수가높다","학업성취도가높다","창의적사고를한다","특정분야에서뛰어난능력을보인다","모든분야에서뛰어난능력을보인다")
#' colnames(shin2013_mat)=c("지능지수","학업성취도","창의적사고","특정분야","모든분야")
#' rownames(shin2013_mat)=c("고졸","초대졸","대졸","대학원이상")
#' shin2013_mat
#'
#' shin2013_mat %>% data.frame() %>%
#'   rownames_to_column("학력") %>%
#'   to_long(names_to = "영재성",cols=2:6) %>%
#'   unCount() %>%
#'   chisq_test_kge(v1="영재성", v2="학력")
#'
#'
#' shin2013_mat%>%  data.frame() %>%
#'   long_df("학력","영재성") %>%
#'   unCount() %>%
#'   chisq_test_kge("학력","영재성")

#'
#' }
chisq_test_kge = function(dataset,
                          v1="var1",
                          v2="var2",
                          title ="Table",
                          type = "result",
                          digits = 3,
                          yadd=0.4,
                          Ncol=NULL,
                          trans = FALSE,
                          simple= FALSE, #유의성 종류
                          ko = TRUE,
                          simulate.p.value=FALSE,
                          correct= FALSE,
                          size_bartext=5,
                          strip_size = 16,
                          axis_size = 16,
                          text_size = 13,
                          xlab = "관측기대비율",
                          ylab = "var2",
                          cramer="adjust"
                          # raw =TRUE
){

  # if(raw){
  #   # Using data.frame
  #   data =  dataset %>%
  #     dplyr::select(all_of(v1), all_of(v2)) %>%
  #     table()
  # }else{
  #   # table data/ Contigency table
  #   var1 = rownames(dataset)
  #   var2 = colnames(dataset)
  #
  #   data =  dataset%>% long_df(v1,v2) %>% unCount() %>% table()
  # }
  #
  if(is.matrix(dataset)|is.table(dataset)){
    # table data/ Contigency table
    var1 = rownames(dataset)
    var2 = colnames(dataset)

    data =  dataset%>% long_df(v1,v2) %>% unCount() %>% table()


  }else if(is.data.frame){
     # Using data.frame
    data =  dataset %>%
      dplyr::select(all_of(v1), all_of(v2)) %>%
      table()
  }else{
    data =  dataset %>%
      dplyr::select(all_of(v1), all_of(v2)) %>%
      table()

  }


  #  margn sum
  data_margin0 = data %>% addmargins()

  data_rowsum0 = data %>%  apply(., MARGIN = 2 , FUN = sum)
  data_rowsum_df = data %>%  rbind(SUM = apply(., MARGIN = 2 , FUN = sum) )
  data_colsum = data_rowsum_df %>% apply(., MARGIN = 1 , FUN = sum)

  #비율을 생성하여 결합(%까지 생성하여 결합#

  data_margin = data  %>%
    jjstat::accent_table( v1, v2, trans = trans, type = "ratio")

  #margin sum-------------------------------------------------
  # **column names ----
  r2colnames = paste0(v1,"/", v2)
  #패턴 그래프 ko적용--raq =TRUE, type="g"
  #데이터 프레임 생성
  data_graph = data  %>% jjstat::accent_table( v2, v1, type = "res")

  ##########
  data_margin = cbind(
    rbind(data_margin, SUM = data_rowsum0 ),
    SUM = data_colsum )

  # 교차표 contigency table
  contigency_table_md0 = data_margin %>%
    data.frame() %>%
    rownames_to_column(r2colnames)

  #열이름 재설정
  colnames(contigency_table_md0) <- c(r2colnames,
                                      colnames(data_graph),
                                      "SUM")


  contigency_table_md =  contigency_table_md0 %>%
    jjstat::markdown_table(caption = paste0(title," Contingency table"),
                           general = NULL)

  #Cramer boss----------------------
  cramer_cor = cramers_v(data, type = cramer) %>%suppressWarnings()
  cramer_cor_v = cramers_v(data, type = "cramer", digit = digits)%>%suppressWarnings()
  cramer_cor_v_table = cramers_v(data, type = "datvar1", digits = 2)%>%suppressWarnings()

  # data_margin = cbind(data_margin, data_margin0[, ncol(data_margin0)])

  #chisq.test------------------------------------------
  Onset_or_Coda_Accent_Contingency_table <- data
  #chi-squared dataprem variants
  res_df = suppressWarnings(chisq.test(data) )%>% broom::tidy()



  res = chisq.test(Onset_or_Coda_Accent_Contingency_table,
                   correct = correct,
                   simulate.p.value = simulate.p.value) %>%suppressWarnings()
  # Judgmental statistics
  chi2 = res$statistic
  p_vlaue_chi = res$p.value
  # df_chi = res$parameter

  msg_sig_chi = ifelse(p_vlaue_chi < 0.05,"significant", "not significant")


  chi_mag = paste0(" [chisq = ",round(res$statistic, 2),
                   ", df = ",res$parameter,
                   ", p = ", format_number(res$p.value, 2),"]" )

  #Create a chi-squared report
  v1_input = ifelse(v1=="var1", "Onset",
                    ifelse(grepl("^w", v1), "Weight",
                           ifelse(v1=="a3", "Coda", v1)))

  v2_input = ifelse(v2=="var2", "Accent", v1)


  res_report = paste0("The Pearson's Chi-squared test of independence between ",
                      v1_input," and ",
                      v2_input,
                      " suggests that the effect is statistically ",
                      msg_sig_chi,
                      chi_mag,
                      "; ",
                      cramer_cor_v, ".")%>%suppressWarnings()



  if(nrow(data) != 1){
    chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
      tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
      tibble::column_to_rownames(v1) %>%
      jjstat::Round(digits)
  }else{
    chi_table =
      rbind(
        observed = data %>%
          jjstat::accent_table( v1, v2, trans = trans),
        expected = res$expected,
        obs_expected_ratio = (res$observed / res$expected)
      ) %>% jjstat::Round(digits)

    # g= NULL
  }


  #observer/Expected 에 유의성 표시
  chi_table_sig = format(calculate_chi_sig(data), 3)%>%suppressWarnings()

  ###마크다운   유의성표시된 것으로 변경----
  chi_table_md0 = chi_table_sig %>%data.frame() %>%
    rownames_to_column(r2colnames)
  #열이름 재설정
  colnames(chi_table_md0) <- c(r2colnames, colnames(chi_table_sig))

  chi_table_md = chi_table_md0 %>%
    jjstat::markdown_table(caption = paste0(title, chi_mag,"; ",
                                            cramer_cor_v_table,"."),
                           digits = digits,
                           general = NULL)

  cat("\n\nNOTE: made by Park Joonghee PhD
      ***: p < .001, **: p < .01, *: p < .05\n\n ")


  if(ko){
    graph = patternGraph_obs_exp_ko(data_graph,
                                    raw = TRUE,
                                    size_bartext=size_bartext,
                                    strip_size = strip_size,
                                    axis_size = axis_size,
                                    text_size = text_size,
                                    yadd = yadd,
                                    Ncol = Ncol,
                                    xlab = xlab,
                                    ylab = ylab)
  }else{
    graph = patternGraph_obs_exp(data_graph,
                                 raw = TRUE,
                                 yadd = yadd,
                                 size_bartext=size_bartext,
                                 strip_size = strip_size,
                                 axis_size = axis_size,
                                 text_size = text_size,
                                 Ncol = Ncol,
                                 xlab = xlab,
                                 ylab = ylab)
  }

  result= list(
    contigency_table_md0,
    obs_exp_table = chi_table,
    obs_exp_table_sig = chi_table_md0,
    contigency_table_md,
    obs_exp_table_md = chi_table_md,
    cramers_v = cramer_cor_v,
    chisq_result = res_df,
    chisq_apa = res_report,
    graph)

  switch(type,
         ct = data,
         df = data.frame(data),
         data_graph = data_graph,
         margin = data_margin,
         chisq_test = res,
         chisq_df = res_df,
         chisq_report = res_report,
         chi_table = chi_table,

         contigency_table_margin = contigency_table_margin,
         contigency_table = contigency_table,
         chi_table_sig_each_variable=chi_table_sig_each_variable,
         chi_table_sig_each_variable2=chi_table_sig_each_variable2,
         chisq_test_overall=chisq_test_overall,
         chisq_report_overal=chisq_report_overal,
         CRAMER_V=CRAMER_V,
         CRAMER_V.adusted=CRAMER_V.adusted,
         contigency_table_md= contigency_table_md,
         chi_table_md=chi_table_md,
         g = graph,
         res = result,
         result = result
  )


}

