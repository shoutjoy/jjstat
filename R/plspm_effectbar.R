#' plspm effect bar graph
#'
#' @param x_pls plspm result data
#' @param axis_x axis text size
#' @param axis_y y axis size
#' @param mar mar = c(cex.names*11, 3, 1, 0.5)
#' @param border 1
#' @param ylim default c(0,1)
#' @param col NA bar color 1:black, 2 redc("#9E9AC8", "#DADAEB")
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' dftest = data.frame(relationships = c("자기효능감 -> 진로동기",
#' "자기효능감 -> 진로태도성숙",
#'  "자기효능감 -> 진로준비행동",
#' "진로동기 -> 진로태도성숙", "진로동기 -> 진로준비행동",
#' "진로태도성숙 -> 진로준비행"동),
#'  direct = c(0.883044251414839, 0.455400556420822,
#'   0.414089720886173, 0, 0.39512331888417,
#'   0.115100737249484),
#'  indirect = c(0, 0, 0.40132831512848, 0, 0, 0),
#'  total = c(0.883044251414839, 0.455400556420822,
#'   0.815418036014653, 0, 0.39512331888417,
#'   0.115100737249484)
#' )
#' dftest%>%plspm_effectbar(axis_x=1.2)
#' dftest%>%plspm_effectbar(axis_x=2)
#' jutpls %>%plspm_effectbar(axis_x=1.2, border=1)
#' jutpls %>%plspm_effectbar(axis_x=1.2, col=c("gray30","gray70"))
#' #' total
#' }
#'
plspm_effectbar <- function(x_pls,
                            axis_x = 1,
                            axis_y = 1.1,
                            mar = c(axis_x*10, 3, 1, 0.5), # Adjusted to remove cex.names
                            border = 1,ylim = c(0, 1),
                            col = c("#9E9AC8", "#DADAEB")) {
  # Load necessary library
  library(dplyr)

  if(length((x_pls))==13 |length((x_pls))== 11 ){
    plsdf = x_pls$effects
  }else{
    plsdf = x_pls
  }


  path_effs <- x_pls$effects
  path_effs1 <- as.matrix(path_effs[, 2:3])
  # Add rownames to path_effs1
  rownames(path_effs1) = path_effs[, 1]

  # Set margin size
  op = par(mar = mar)
  # Barplot of total effects (direct + indirect)
  barplot(t(path_effs1), border = border,
          col = col,
          las = 2,
          ylim = ylim,
          cex.names = axis_x,
          cex.axis = axis_y,
          legend = c("Direct", "Indirect"),
          args.legend = list(x = "top", ncol = 2, border = NA,
                             bty = "n", title = "Effects"))
  box()
  # Resetting default margins
  par(op)


  return(path_effs%>% Round(3)%>% cut_print())
}
