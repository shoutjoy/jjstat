#' Frequency analysis table
#'
#' @param data data.frame
#' @param ... vars If you input varabale names "var1", "var2" ,...
#' @param prop proportion value, default TRUE
#' @param angle  axis text angle
#' @param reorder TRUE is plot x-variable reoder
#' @param size_text default 3, bargraph text
#' @param size_axis default 12, axis test
#' @param legend.position legend.position="" none. top, bottom, left, right
#'
#' @return Freqency table
#' @export
#'
#' @examples
#' \dontrun{
#' Mtcars = mtcars
#' Mtcars$am = factor(Mtcars$am, levels=c(0,1), labels= c("automatic","manual" ))
#' Mtcars$vs  = factor(Mtcars$vs, levels=c(0,1), labels= c("V-shaped","straight" ))
#' Mtcars %>% table_freq("am")
#' Mtcars %>% table_freq("am", prop=F)
#' Mtcars %>% table_freq("vs", plot=FALSE)
#'
#' Mtcars %>% table_freq("vs")
#' Mtcars %>% table_freq("vs","am")
#'
#' Mtcars %>% table_freq("vs","am","cyl")
#'
#' Mtcars %>% table_freq("vs","am","cyl", angle=90)
#' Mtcars %>% table_freq("vs","am","cyl", angle=90, reorder = TRUE)
#'
#' Mtcars %>% table_freq("vs","am","cyl", angle=90)+ylim(0,14)
#' Mtcars %>% table_freq("vs","am","cyl", angle=90, reorder = TRUE)+ylim(0,14)
#'
#' Mtcars %>% table_freq("vs","am") %>% arrange(am)
#' Mtcars %>% table_freq("vs","am") %>% arrange(vs)
#' Mtcars %>% table_freq("vs","am") %>% arrange(Freq)
#'
#' }
#'
#'
#'
table_freq = function(data, ...,
                      prop = TRUE,
                      plot = TRUE,
                      angle = 0,
                      size_text = 5,
                      size_axis = 12,
                      legend.position = "top",
                      reorder = FALSE){
  select_vars = c(...)

  res = data[, c(...)]%>%
    table() %>%
    data.frame() %>%
    mutate("prop(%)" = Freq/sum(Freq)*100)


  if(length(select_vars)==1){
    colnames(res) = c(select_vars, "Freq","prop(%)" )
  }
  if(!prop){
    res = res %>%dplyr::select(-`prop(%)`)
    LABEL = paste("n=", res$Freq)
  }else{
    LABEL = paste0(res$Freq,"(",round(res$`prop(%)`,2),"%)")
  }



  print(res %>%tibble::tibble())
  ##plot
  if(plot){
    # Select  fct, chr variable  then Unite
    Res = res %>%
      unite(x_var, where(is.factor), where(is.character))

    # plot x-variable reorder
    if(reorder){
      Res <- Res %>% mutate(x_var = fct_reorder(x_var, desc(Freq)))
    }

    #make graph
    g = Res%>%
      ggplot(aes(x = x_var, y = Freq))+
      geom_bar(stat="identity", aes(fill = x_var))+
      theme_bw()+
      geom_text(aes(label = LABEL), vjust = -.5, size = size_text)+
      theme(axis.text.x = element_text(angle = angle,
                                       size = size_axis, face = "bold"),
            legend.position = legend.position)
    #output graph

    print(g)
  }
}


#' Frequency analysis table
#'
#' @param data data.frame
#' @param ... vars If you input varabale names "var1", "var2" ,...
#' @param prop proportion value, default TRUE
#' @param size_text default 3, bargraph text
#' @param size_axis default 12, axis test
#' @param legend.position legend.position="" none. top, bottom, left, right
#' @return Freqency table
#' @export
#'
#' @examples
#' \dontrun{
#' Mtcars = mtcars
#' Mtcars$am = factor(Mtcars$am, levels=c(0,1), labels= c("automatic","manual" ))
#' Mtcars$vs  = factor(Mtcars$vs, levels=c(0,1), labels= c("V-shaped","straight" ))
#' Mtcars %>% Freq_table("am")
#' Mtcars %>% Freq_table("am", prop=F)
#' Mtcars %>% Freq_table("vs", plot=FALSE)
#'
#' Mtcars %>% Freq_table("vs")
#' Mtcars %>% Freq_table("vs","am")
#'
#' Mtcars %>% Freq_table("vs","am","cyl")
#'
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90)
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90, reorder = TRUE)
#'
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90)+ylim(0,14)
#' Mtcars %>% Freq_table("vs","am","cyl", angle=90, reorder = TRUE)+ylim(0,14)
#'
#' Mtcars %>% Freq_table("vs","am") %>% arrange(am)
#' Mtcars %>% Freq_table("vs","am") %>% arrange(vs)
#' Mtcars %>% Freq_table("vs","am") %>% arrange(Freq)
#'
#' }
#'
#'
#'
Freq_table = function(data, ...,
                      prop = FALSE,
                      plot = FALSE,
                      angle = 0,
                      size_text = 5,
                      size_axis = 12,
                      legend.position ="top",
                      reorder = FALSE){
  select_vars = c(...)

  res = data[, c(...)]%>%
    table() %>%
    data.frame() %>%
    mutate("prop(%)" = Freq/sum(Freq)*100)


  if(length(select_vars)==1){
    colnames(res) = c(select_vars, "Freq","prop(%)" )
  }
  if(!prop){
    res = res %>%dplyr::select(-`prop(%)`)
    LABEL = paste("n=", res$Freq)
  }else{
    LABEL = paste0(res$Freq,"(",round(res$`prop(%)`,2),"%)")
  }



  # print(res %>%tibble::tibble())
  print(res )
  ##plot
  if(plot){
    # Select  fct, chr variable  then Unite
    Res = res %>%
      unite(x_var, where(is.factor), where(is.character))

    # plot x-variable reorder
    if(reorder){
      Res <- Res %>% mutate(x_var = fct_reorder(x_var, desc(Freq)))
    }

    #make graph
    g = Res%>%
      ggplot(aes(x = x_var, y = Freq))+
      geom_bar(stat="identity", aes(fill = x_var))+
      theme_bw()+
      geom_text(aes(label = LABEL), vjust = -.5, size = size_text)+
      theme(axis.text.x = element_text(angle = angle,
                                       size = size_axis, face = "bold"),
            legend.position = legend.position)
    #output graph

    print(g)
  }
}


