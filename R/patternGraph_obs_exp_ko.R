
#' patternGraph_obs_exp
#' @param data data,frame
#' @param raw raw data
#' @param Ncol grid
#' @param yadd up yadd
#' @param strip_size  strip
#' @param axis_size axis
#' @param text_size text
#' @param size_bartext bartext
#' @param xlab xlab
#' @param ylab ylab
#' @param Colsnames new select col pivot long
#' @param trans trans plot transpose
#' @param type res

#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#'   auto_pattern("고유어1") %>%
#'   kge_chisq_table("a1","성조", "고유어1 부산")
#'
#' #sAVE
#' data_long<- aaa$chi_table %>%
#'   rownames_to_column("syllabic") %>%
#'   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
#' data_long
#'
#' data_long%>% ggplot(aes(x = accent, y = ratio))+
#'   geom_bar(stat = "identity", aes( fill = accent),
#'            position = "dodge", show.legend = FALSE)+
#'   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#'   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#'   ylim(0,max(data1$ratio)+0.07)+
#'   coord_flip()+
#'   theme_bw()+
#'   theme(axis.text = element_text(size= 12),
#'         axis.title = element_text(size= 12),
#'         strip.text = element_text(size= 14)
#'   )+
#'   scale_fill_grey(start = 0, end = 0.7) +
#'   facet_wrap(~ syllabic )
#'
#' aaa%>% patternGraph_obs_exp_ko()
#'
#' #신진영(2013)
#' shin2013_mat =matrix(c(14,1,23,3,4,0,9,0,13,18,89,8,11,40,46,12,3,4,22,0), ncol= 5)
#' # colnames(shin2013_mat)=c("지능지수가높다","학업성취도가높다","창의적사고를한다","특정분야에서뛰어난능력을보인다","모든분야에서뛰어난능력을보인다")
#' colnames(shin2013_mat)=c("지능지수","학업성취도","창의적사고","특정분야","모든분야")
#' rownames(shin2013_mat)=c("고졸","초대졸","대졸","대학원이상")
#' shin2013_mat
#'
#' shin2013_mat%>% data.frame() %>%
#'   long_df("학력","영재성", cols=2:6) %>%
#'   add_rows_freq("real") %>%
#'   chisq_test_kge("학력","영재성", type="data_graph") %>%
#'   patternGraph_obs_exp_ko()
#'
#'
#'
#' }
#'
patternGraph_obs_exp_ko = function(data,
                                   raw = TRUE,
                                   Ncol = NULL,
                                   yadd = 0.5,
                                   strip_size = 16,
                                   axis_size = 16,
                                   text_size = 13,
                                   size_bartext = 5,
                                   xlab = "기준변수",
                                   ylab = "관측기대비율",
                                   Colsnames = NULL,
                                   trans=FALSE,
                                   type="g"
){



  data1 <- data

  if(is.null(Colsnames)){
    Cols = 1:ncol(data1)+1
  }else{
    Cols = Colsnames
  }


  data_long0 <- data1 %>% as.data.frame() %>%
    long_df(
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols )

  data_long_df <- data1 %>%
    long_df(

      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols )%>%
    jjstat::Round()

  data_long_oe <- data1 %>%
    obs_exp_table() %>%
    long_df(
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols) %>%
    jjstat::Round()

  data_long_sig =  data1%>%
    p_sig_cal()%>%
    long_df(
      names_to = "성조형",values_to = 'star',
      rowname = "syllabic",
      cols = Cols)

  data_long_p =  data1%>%
    p_value_cal()%>%
    long_df(
      names_to = "성조형",values_to = 'p.value',
      rowname = "syllabic",
      cols = Cols )


  ##################
  if(trans){


    if(raw){
      #For a contigency table
      data_long = bind_cols(data_long_oe, data_long_sig[, 3]) %>%
        tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

      g0 = data_long  %>% ggplot(aes(x = syllabic, y = 관측기대비율))

    }else{
      #관When used in kge_chisq_table when a side/expectation table comes in
      data_long = bind_cols(data_long_df, data_long_sig[, 3]) %>%
        tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

      g0 = data_long  %>% ggplot(aes(x = syllabic, y = 관측기대비율))
    }




    g = g0 +geom_bar(stat = "identity", aes( fill = syllabic),
                     position = "dodge", show.legend = FALSE)+
      geom_hline(yintercept = 1, linetype=2, color="gray80")+
      geom_text(aes(label =  Sig ),
                hjust = -0.1, size = size_bartext)+
      ylim(0,max(data_long_oe[, 3])+ yadd)+
      labs(x = xlab, y = ylab)+
      coord_flip()+
      theme_bw()+
      theme(axis.text = element_text(size= text_size),
            axis.title = element_text(size= axis_size),
            strip.text = element_text(size= strip_size)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      facet_wrap(~ 성조형 , ncol = Ncol)

  }else{
  #####################
    if(raw){
      #For a contigency table
      data_long = bind_cols(data_long_oe, data_long_sig[, 3]) %>%
        tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

      g0 = data_long  %>% ggplot(aes(x = 성조형, y = 관측기대비율))

    }else{
      #관When used in kge_chisq_table when a side/expectation table comes in
      data_long = bind_cols(data_long_df, data_long_sig[, 3]) %>%
        tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

      g0 = data_long  %>% ggplot(aes(x = 성조형, y = 관측기대비율))
    }




    g = g0 +geom_bar(stat = "identity", aes( fill = 성조형),
                     position = "dodge", show.legend = FALSE)+
      geom_hline(yintercept = 1, linetype=2, color="gray80")+
      geom_text(aes(label =  Sig ),
                hjust = -0.1, size = size_bartext)+
      ylim(0,max(data_long_oe[, 3])+ yadd)+
      labs(x = xlab, y = ylab)+
      coord_flip()+
      theme_bw()+
      theme(axis.text = element_text(size= text_size),
            axis.title = element_text(size= axis_size),
            strip.text = element_text(size= strip_size)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      facet_wrap(~ syllabic , ncol = Ncol)

    }




  res =  list(g, data_long,data_long_oe, data_long_sig, data_long_p)
  g = g

  switch(type, res = res, g=g)

}






#' patternGraph_obs_exp
#' @param data data,frame
#' @param raw raw data
#' @param Ncol grid
#' @param yadd up yadd
#' @param strip_size  strip
#' @param axis_size axis
#' @param text_size text
#' @param size_bartext bartext
#' @param xlab xlab
#' @param ylab ylab
#' @param Colsnames new select col pivot long
#' @param type res
#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#'   auto_pattern("고유어1") %>%
#'   kge_chisq_table("a1","성조", "고유어1 부산")
#'
#' #sAVE
#' data_long<- aaa$chi_table %>%
#'   rownames_to_column("syllabic") %>%
#'   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
#' data_long
#'
#' data_long%>% ggplot(aes(x = accent, y = ratio))+
#'   geom_bar(stat = "identity", aes( fill = accent),
#'            position = "dodge", show.legend = FALSE)+
#'   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#'   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#'   ylim(0,max(data1$ratio)+0.07)+
#'   coord_flip()+
#'   theme_bw()+
#'   theme(axis.text = element_text(size= 12),
#'         axis.title = element_text(size= 12),
#'         strip.text = element_text(size= 14)
#'   )+
#'   scale_fill_grey(start = 0, end = 0.7) +
#'   facet_wrap(~ syllabic )
#'
#' aaa%>% patternGraph_obs_exp()
#'
#' #신진영(2013)
#' shin2013_mat =matrix(c(14,1,23,3,4,0,9,0,13,18,89,8,11,40,46,12,3,4,22,0), ncol= 5)
#' # colnames(shin2013_mat)=c("지능지수가높다","학업성취도가높다","창의적사고를한다","특정분야에서뛰어난능력을보인다","모든분야에서뛰어난능력을보인다")
#' colnames(shin2013_mat)=c("지능지수","학업성취도","창의적사고","특정분야","모든분야")
#' rownames(shin2013_mat)=c("고졸","초대졸","대졸","대학원이상")
#' shin2013_mat
#'
#' shin2013_mat%>% data.frame() %>%
#'   long_df("학력","영재성", cols=2:6) %>%
#'   add_rows_freq("real") %>%
#'   chisq_test_kge("학력","영재성", type="data_graph") %>%
#'   patternGraph_obs_exp_ko()
#'
#' }
patternGraph_obs_exp = function(data,
                                raw = TRUE,
                                Ncol = NULL,
                                yadd = 0.3,
                                # values_to = NULL,
                                # names_to =NULL,
                                strip_size = 16,
                                axis_size = 15,
                                text_size = 13,
                                size_bartext=4,
                                xlab = "성조형",
                                ylab = "관측기대비율",
                                Colsnames = NULL,
                                type="g"
){
library(tidyverse)

  data1 <- data

  if(is.null(Colsnames)){
    Cols = 1:ncol(data1)+1
  }else{
    Cols = Colsnames
  }


  data_long0 <- data1 %>% as.data.frame() %>%
    long_df(
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols )

  data_long_df <- data1 %>%
    long_df(

      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols )%>%
    jjstat::Round()

  data_long_oe <- data1 %>%
    obs_exp_table() %>%
    long_df(
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols) %>%
    jjstat::Round()

  data_long_sig =  data1%>%
    p_sig_cal()%>%
    long_df(
      names_to = "성조형",values_to = 'star',
      rowname = "syllabic",
      cols = Cols)

  data_long_p =  data1%>%
    p_value_cal()%>%
    long_df(
      names_to = "성조형",values_to = 'p.value',
      rowname = "syllabic",
      cols = Cols )

  if(raw){
    #contigency table인 경우
    data_long = dplyr::bind_cols(data_long_oe, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = 성조형, y = 관측기대비율))

  }else{
    #관측/기대 표가 들어 온경우  kge_chisq_table에서 사용시
    data_long = dplyr::bind_cols(data_long_df, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = 성조형, y = 관측기대비율))
  }



  g = g0 + geom_bar(stat = "identity", aes( fill = accent),
                    position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    # geom_text(aes(label =  paste(round(ratio,2), star ) ),
    geom_text(aes(label =  Sig ),
              hjust = -0.1, size = size_bartext)+
    ylim(0,max(data_long_oe[, 3])+ yadd)+
    labs(x = xlab, y = ylab)+
    coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(size= text_size),
          axis.title = element_text(size= axis_size),
          strip.text = element_text(size= strip_size)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ syllabic , ncol = Ncol)

  res =  list(g, data_long,data_long_oe, data_long_sig, data_long_p)
  g= g

  switch(type, res = res, g=g)
}





#' patternGraph2
#' @param data data,frame
#' @param raw raw data
#' @param Ncol grid
#' @param yadd up yadd
#' @param strip_size  strip
#' @param axis_size axis
#' @param text_size text
#' @param size_bartext bartext
#' @param type g
#' @param xlab xlab
#' @param ylab ylab
#' @param Colsnames new select col pivot long
#'
#' @return result fill data
#' @export
#'
#' @examples
#' \dontrun{
#' aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#'   auto_pattern("고유어1") %>%
#'   kge_chisq_table("a1","성조", "고유어1 부산")
#'
#' #sAVE
#' data_long<- aaa$chi_table %>%
#'   rownames_to_column("syllabic") %>%
#'   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
#' data_long
#'
#' data_long%>% ggplot(aes(x = accent, y = ratio))+
#'   geom_bar(stat = "identity", aes( fill = accent),
#'            position = "dodge", show.legend = FALSE)+
#'   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#'   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#'   ylim(0,max(data1$ratio)+0.07)+
#'   coord_flip()+
#'   theme_bw()+
#'   theme(axis.text = element_text(size= 12),
#'         axis.title = element_text(size= 12),
#'         strip.text = element_text(size= 14)
#'   )+
#'   scale_fill_grey(start = 0, end = 0.7) +
#'   facet_wrap(~ syllabic )
#'
#' aaa%>% patternGraph_obs_exp()
#'
#' }
patternGraph2 = function(data, type="g",
                         raw = TRUE,
                         Ncol = NULL,
                         yadd = 0.45,
                         # values_to = NULL,
                         # names_to =NULL,
                         xlab = "음절",
                         ylab = "관측기대비율",
                         strip_size = 16,
                         axis_size = 16,
                         text_size = 14,Colsnames = NULL,
                         size_bartext=5

){

  data1 <- data

  if(is.null(Colsnames)){
    Cols = 1:ncol(data1)+1
  }else{
    Cols = Colsnames
  }


  data_long0 <- data1 %>% as.data.frame() %>%
    long_df(
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols )

  data_long_df <- data1 %>%
    long_df(

      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols )%>%
    jjstat::Round()

  data_long_oe <- data1 %>%
    obs_exp_table() %>%
    long_df(
      names_to = "성조형",values_to = '관측기대비율',
      rowname = "syllabic",
      cols = Cols) %>%
    jjstat::Round()

  data_long_sig =  data1%>%
    p_sig_cal()%>%
    long_df(
      names_to = "성조형",values_to = 'star',
      rowname = "syllabic",
      cols = Cols)

  data_long_p =  data1%>%
    p_value_cal()%>%
    long_df(
      names_to = "성조형",values_to = 'p.value',
      rowname = "syllabic",
      cols = Cols )





  if(raw){
    #contigency table인 경우
    data_long = bind_cols(data_long_oe, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = syllabic, y = 관측기대비율))

  }else{
    #관측/기대 표가 들어 온경우  kge_chisq_table에서 사용시
    data_long = bind_cols(data_long_df, data_long_sig[, 3]) %>%
      tidyr::unite(Sig, 관측기대비율, star, remove = FALSE, sep = "")

    g0 = data_long  %>% ggplot(aes(x = syllabic, y = 관측기대비율))
  }




  g = g0 +geom_bar(stat = "identity", aes( fill = syllabic),
                   position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    geom_text(aes(label =  Sig ),
              hjust = -0.1, size = size_bartext)+
    ylim(0,max(data_long_oe[, 3])+ yadd)+
    labs(x = xlab, y = ylab)+
    coord_flip()+
    theme_bw()+
    theme(
      axis.text.y =  element_text(size= text_size + 2),
      axis.text.x =  element_text(size= text_size - 1.5),
      axis.title = element_text(size= axis_size),
      strip.text = element_text(size= strip_size)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ 성조형 , ncol = Ncol)

  res =  list(g, data_long,data_long_oe, data_long_sig, data_long_p)
  g= g

  switch(type,
         res = res,
         all = res,
         g=g)
}

