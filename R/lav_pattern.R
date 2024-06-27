#' 응답패턴 분석
#'
#' @param data data
#' @param n 구분되는 개수
#' @param dim 0L
#' @param top 20
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'

#' latentPattern(MarshWenHau, n=3)
#'
#' HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
#'                                  "x6","x7","x8","x9")]
#' latentPattern(HS9)
#'
#' #explain
#' HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
#' HSbinary %>%head()
#' lavTables(HSbinary, dim = 0L, categorical = names(HSbinary)) %>%head(20)
#'
#' #'
#' #'
#'
#' x11()
#' latentPattern(park516_kor[,1:6], n=5) %>%
#'   ggplot(aes(x= fct_reorder(pattern, desc(obs.freq)), y= obs.freq))+
#'   geom_bar(stat="identity")+theme_bw()+
#'   theme(axis.text.x= element_text(size=14, angle=90))
#' }
#'
#'
lav_Pattern= function(data, n=2, dim=0L, top=20){
  res = as.data.frame( lapply(data, cut, n, labels=FALSE) )%>%
    lavaan::lavTables(dim=dim, categorical = names(data))
  res%>%top_n(res$obs.freq, n= top)
}
