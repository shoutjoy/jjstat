
#' plspm_compare_bar
#'
#' @param data arragne data
#' @param x paths
#' @param y values
#' @param fill group
#' @param size_x textsize
#' @param angle xangle
#' @param position  dodge, stack
#' @param text  hide bartext
#' @param size  bargrahp text size
#'
#' @return graph
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' REs <- tibble::tribble(
#'   ~paths, ~grp, ~values,
#'   "자기효능감->진로동기", "group.남자", 0.902,
#'   "자기효능감->진로동기", "group.여자", 0.866,
#'   "자기효능감->진로태도", "group.남자", -0.639,
#'   "자기효능감->진로태도", "group.여자", -0.672,
#'   "자기효능감->진로준비", "group.남자", 0.149,
#'   "자기효능감->진로준비", "group.여자", 0.487,
#'   "진로동기->진로준비", "group.남자", 0.63,
#'   "진로동기->진로준비", "group.여자", 0.432,
#'   "진로태도->진로준비", "group.남자", -0.153,
#'   "진로태도->진로준비", "group.여자", 0.059
#' )
#'
#' # # 함수 호출 예시
#' REs %>% plspm_effect_bar()
#' REs %>% plspm_effect_bar(text=FALSE)
#' }
#'
plspm_effect_bar <- function(data, x=NULL, y=NULL, fill=NULL,size=5,
                        size_x= 12, angle= 90, position="dodge", text=TRUE) {
  Data <- data

  if (is.null(x) && is.null(y) && is.null(fill)) {
    x <- Data %>% select(1) %>% pull()
    y <- Data %>% select(3) %>% pull()
    fill <- Data %>% select(2) %>% pull()
    gg <- Data %>% ggplot(aes(x = x, y = y, fill = fill))
  } else {
    gg <- Data %>% ggplot(aes(x = {{x}}, y = {{y}}, fill = {{fill}}))
  }

  gg <- gg +
    geom_bar(stat="identity", position=position) +
    theme_bw() +
    scale_fill_grey(start=0.3, end=0.7) +
    theme(axis.text = element_text(size = size_x, angle=angle, face="bold"))+
    labs(x= "")

  if (text) {
    gg <- gg +
      geom_text(aes(label = values),
                vjust = ifelse(Data$values > 0, -0.5, 1.5),
                position = position_dodge(0.9), size = size)+
      labs(x= "")
  }

  return(gg)
}




#' plspm_compare_bar
#'
#' @param data arragne data
#' @param x paths
#' @param y values
#' @param fill group
#' @param size_x textsize
#' @param angle xangle
#' @param position  dodge, stack
#' @param text  hide bartext
#' @param size  bargrahp text size
#' @param xlab  default ""
#' @param ylab  values
#'
#' @return  graph
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' REs <- tibble::tribble(
#'   ~paths, ~grp, ~values,
#'   "자기효능감->진로동기", "group.남자", 0.902,
#'   "자기효능감->진로동기", "group.여자", 0.866,
#'   "자기효능감->진로태도", "group.남자", -0.639,
#'   "자기효능감->진로태도", "group.여자", -0.672,
#'   "자기효능감->진로준비", "group.남자", 0.149,
#'   "자기효능감->진로준비", "group.여자", 0.487,
#'   "진로동기->진로준비", "group.남자", 0.63,
#'   "진로동기->진로준비", "group.여자", 0.432,
#'   "진로태도->진로준비", "group.남자", -0.153,
#'   "진로태도->진로준비", "group.여자", 0.059
#' )
#'
#' # # 함수 호출 예시
#' REs %>% compare_bar()
#' REs %>% compare_bar(text=FALSE)
#' }
compare_bar <- function(data, x=NULL, y=NULL, fill=NULL,size=5,
                              size_x= 12, angle= 90,
                        position="dodge", xlab="", ylab="Values",
                        text=TRUE) {
  Data <- data

  if (is.null(x) && is.null(y) && is.null(fill)) {
    x <- Data %>% select(1) %>% pull()
    y <- Data %>% select(3) %>% pull()
    fill <- Data %>% select(2) %>% pull()
    gg <- Data %>% ggplot(aes(x = x, y = y, fill = fill))
  } else {
    gg <- Data %>% ggplot(aes(x = {{x}}, y = {{y}}, fill = {{fill}}))
  }

  gg <- gg +
    geom_bar(stat="identity", position=position) +
    theme_bw() +
    scale_fill_grey(start=0.3, end=0.7) +
    theme(axis.text = element_text(size = size_x, angle=angle, face="bold"))+
    labs(x= xlab, y=ylab)

  if (text) {
    gg <- gg +
      geom_text(aes(label = values),
                vjust = ifelse(Data$values > 0, -0.5, 1.5),
                position = position_dodge(0.9), size = size)+
      labs(x= xlab, y=ylab)
  }

  return(gg)
}

#' pdata plspm group change long data format
#'
#' @param data  wide data
#'
#' @return long
#' @export
#' @usage pdata(widedata, rownames_to_column=FALSE)

pdata = function(widedata, rownames_to_column=FALSE){

  res=  widedata%>%
    plspm_grp_summary()%>%
    dplyr::select(1, 3,4)%>%
    to_long("grp", rownames_to_column =rownames_to_column)
  res
}


