
#' lca_class_plot,잠재클래스 패턴 그래프
#'
#' @param lcadata_graph  lcaGraphData 데이터처리된 것
#' @param type g, full, line
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' ## using
#' jutLca3 %>%lcaGraphData() %>% lca_class_plot()
#' jutLca3 %>%lcaGraphData() %>% lca_class_plot("line")
#'
#' jutLca3 %>%lcaGraphData( "학업성적"="L05",
#'                          "부모지지"="L04",
#'                          "진로결정"="L03",
#'                          "준비미흡"="L02",
#'                          "목표확실"="L01") %>%
#'   lca_class_plot()
#' #full graph
#' jutLca3 %>%lcaGraphData( "학업성적"="L05",
#'                          "부모지지"="L04",
#'                          "진로결정"="L03",
#'                          "준비미흡"="L02",
#'                          "목표확실"="L01") %>%
#'   replace_df_rep(
#'     "L01", "L01:목표확실",
#'     "L02", "L02:준비미흡",
#'     "L03", "L03:진로결정",
#'     "L04", "L04:부모지지",
#'     "L05", "L05:학업성적")%>%
#'   replace_df_rep(
#'     "Class1", "Class1:적극적 진로준비형",
#'     "Class2", "Class2:불명확 진로인식형",
#'     "Class3", "Class3:조건부 진로지향형"
#'   )%>%lca_class_plot("line")
#'
#' }
lca_class_plot = function(lcadata_graph, type="g"){

  #full
  full = lcadata_graph %>%
    ggplot()+
    geom_bar(aes(item, YES, fill = item_cluster), stat = "identity")+
    labs(x="측정문항", y ="YES응답확률", fill="측정문항의 영역")+
    #   coord_flip()+
    theme_bw()+
    theme(legend.position = "top",
          strip.text= element_text(size = 24, face="bold"),
          axis.text.x = element_text(size = 18, angle=90, face="bold", color="gray10"),
          axis.text.y = element_text(size = 15, angle=0, face="bold"),
          legend.text = element_text(size = 20, angle=0, face="bold"),
          axis.title = element_text(size = 18, angle=0, face="bold", color="gray30"),
    )+ geom_line(aes(x= item, y= YES),
                 group = "item_cluster",
                 linewidth= 1.2, linetype ="solid",
    )+
    geom_point(aes(x= item, y= YES,
                   #  color=item_cluster,
                   shape=item_cluster), show.legend = FALSE,
               size=6, alpha= 0.8)+
    labs(x="item", y ="Probability of YES(Career preparation)", fill="Measurement question areas")+
    # scale_fill_grey()+
    facet_wrap(~ Class)


  #line bw grapn
  line = lcadata_graph %>%
    ggplot()+
    #   geom_bar(aes(item, YES, fill = item_cluster), stat = "identity")+
    labs(x="측정문항", y ="YES응답확률", fill="측정문항의 영역")+
    #   coord_flip()+
    theme_bw()+
    theme(legend.position = "top",
          strip.text= element_text(size = 24, face="bold"),
          axis.text.x = element_text(size = 18, angle=90, face="bold"),
          axis.text.y = element_text(size = 16, angle=0, face="bold"),
          legend.text = element_text(size = 20, angle=0, face="bold"),
          axis.title = element_text(size = 18, angle=0, face="bold"),
    )+ geom_line(aes(x= item, y= YES),
                 group = "item_cluster",
                 linewidth= 1.0, linetype ="solid",
    )+
    geom_point(aes(x= item, y= YES,
                   #  color=item_cluster,
                   shape=item_cluster), show.legend = TRUE,
               size=4, alpha= 0.8)+
    labs(x="item", y ="Probability of YES", fill="Measurement question areas")+
    facet_wrap(~ Class)

  switch(type, g=full,full=full, line=line)

}
