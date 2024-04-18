
#' patternGraph
#'
#' @param data data.frame
#' @param size bar text size
#' @param strip strip size
#' @param text axis text test size
#' @param axis aixs size
#' @param hjust hjust
#' @param yadd yadd
#' @param type type "g", "data", "all"
#' @param xlab xlab
#' @param ylab ylab
#' @param show show.legend
#' @param tolong long format transpose
#'
#' @return graph
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
#' aaa%>% patternGraph()
#'
#' }
patternGraph = function(data,
                        size=4,
                        strip = 14,
                        text = 12,
                        axis = 16,
                        hjust = -0.4,
                        yadd = 10,
                        type = "g",
                        xlab = "성조형",
                        ylab="빈도",
                        show = TRUE,
                        tolong = TRUE
){


  if(tolong){
    data1 = data %>% data.frame() %>%
      rownames_to_column("accent")

    data1 =  data1%>% pivot_longer(names_to = "speaker",
                                   values_to = "freq",
                                   cols=2:ncol(data1))
  }else{
    data1 = data
  }


  if(show){
    g =  data1%>%
      ggplot(aes(x = accent, y = freq))+
      geom_bar(stat = "identity", aes( fill = accent),
               position = "dodge", show.legend = FALSE)+
      geom_text(aes(label = data1$freq), hjust =  hjust, size = size)+
      coord_flip()+
      ylim(0, max(data1$freq)+ yadd)+
      labs(x = xlab, y= ylab)+
      theme_bw()+
      theme(axis.text = element_text(size= text),
            axis.title = element_text(size= axis),
            strip.text = element_text(size= strip)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      # facet_wrap(~ accent)
      facet_wrap(~ speaker)

  }else{
    g =  data1%>%
      ggplot(aes(x = accent, y = freq))+
      geom_bar(stat = "identity", aes( fill = accent),
               position = "dodge", show.legend = FALSE)+
      # geom_text(aes(label = freq), hjust =  hjust, size = size)+
      coord_flip()+
      ylim(0, max(data1$freq)+ yadd)+
      labs(x = xlab, y= ylab)+
      theme_bw()+
      theme(axis.text = element_text(size= text),
            axis.title = element_text(size= axis),
            strip.text = element_text(size= strip)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      # facet_wrap(~ accent)
      facet_wrap(~ speaker)
  }
  res= list(g, data1)

  switch(type,
         g = g,
         data = data1,
         all=res
  )
}
