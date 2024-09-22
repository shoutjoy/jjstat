#' lca_freq_plot LCA 빈도 그래프
#'
#' @param data df
#' @param lcadata lcadata
#'
#' @return  plot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' jut7c %>% lca_freq_plot(jutLca3)+ ylim(0, 380)
#' }
lca_freq_plot = function(data, lcadata ){

  g = data %>%
    dplyr::select(L01:L05, LCA_class)%>%
    group_by(LCA_class) %>%
    count()%>%
    ggplot(aes(LCA_class, n, fill=LCA_class))+
    geom_text(aes(label = paste0("N=", n,
                                 ", γ=", round(lca_gamma(lcadata),3)*100,"%")), #
              size=8, vjust =-.9)+
    geom_bar(stat = "identity")+
    theme_bw()+
    theme(axis.text= element_text(size=18, face="bold"),
          axis.title = element_text(size=20, color="gray30"),
          legend.text =element_text(size=18))+
    labs(x="Latent Class", y="Frequency and Gamma(Prior probability)")

  return(g)

}
