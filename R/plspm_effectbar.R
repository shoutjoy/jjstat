#' plspm effect bar graph
#'
#' @param x_pls plspm result data
#' @param axis_x axis text size
#' @param axis_y y axis size
#' @param mar mar = c(cex.names*11, 3, 1, 0.5)
#' @param border_col NA bar color 1:black, 2 red
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' dftest = data.frame(relationships = c("자기효능감 -> 진로동기", "자기효능감 -> 진로태도성숙", "자기효능감 -> 진로준비행동", "진로동기 -> 진로태도성숙", "진로동기 -> 진로준비행동", "진로태도성숙 -> 진로준비행"동),
#'  direct = c(0.883044251414839, 0.455400556420822, 0.414089720886173, 0, 0.39512331888417, 0.115100737249484),
#'  indirect = c(0, 0, 0.40132831512848, 0, 0, 0),
#'  total = c(0.883044251414839, 0.455400556420822, 0.815418036014653, 0, 0.39512331888417, 0.115100737249484)
#' )
#' dftest%>%plspm_effectbar(cex.names=1.2)
#' jutpls %>%plspm_effectbar(cex.names=1.2, border=1)
#' jutpls %>%plspm_effectbar(cex.names=1.2, col=c("gray30","gray70"))
#' #'
#' }
#'
plspm_effectbar<- function(x_pls,
                           axis_x = 1.3,
                           axis_y = 1.1,
                           mar = c(cex.names*11, 3, 1, 0.5),
                           border_col = 1,
                           col = c("#9E9AC8", "#DADAEB")){
  #논문에 넣기 위해 엑셀로 편집하기 쉽도록 구성한 표
  library(dplyr)

  a1 <-x_pls$effects %>% filter(total>0) #%>% kable(format ="pandoc",digits =3,
  #caption = "Total Effect(Direct & Indirect)")

  path_effs <- x_pls$effects %>% filter(total>0)
  path_effs1 <- as.matrix(path_effs[,2:3])
  # add rownames to path_effs
  rownames(path_effs1) = path_effs[,1]

  # setting margin size
  op = par(mar = mar)
  # barplots of total effects (direct + indirect)
  barplot(t(path_effs1), border = border_col,
          col = col,
          las = 2,
          ylim = c(0, 1),
          cex.names = axis_x,
          cex.axis = axis_y,
          legend = c("Direct", "Indirect"),
          args.legend = list(x = "top", ncol = 2, border = NA,
                             bty = "n", title = "Effects"))
  box()
  # resetting default margins
  par(op)

  return(a1)
}
