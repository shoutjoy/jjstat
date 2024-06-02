#' plspm_rotate_resid
#'
#' @param plotdata plotdata
#' @param resid_list resid list
#' @param plot TRUE
#'
#' @return plot data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' pathmodel = "
#' 자기효능감 =~ C_S1 +  C_S2 + C_S3 + C_S4 + C_S5
#' 진로태도 =~ B02 + B04 + B10 + B14
#' 진로동기 =~ A_M1 + A_M2 + A_M3
#' 진로준비 =~ D_P1 + D_P2 + D_P3
#'
#' 진로동기 ~ H1*자기효능감
#' 진로태도 ~  H2*자기효능감
#' 진로준비 ~ H5 *진로태도
#' 진로준비 ~  H4 * 진로동기
#' 진로준비 ~  H3*자기효능감
#' "
#'
#' plspm_diagram_model(pathmodel, layout="tree2",
#'                     rotation = 2,residuals = T, residScale = 6,
#'                     edge.label.position = 0.6)%>%
#'   plspm_rotate_resid(c(진로동기= 160, 진로태도= 160))
#'
#' #
#' plspm_diagram_model(pathmodel, layout="tree2",
#'                     rotation = 2,residuals = T, residScale = 6,
#'                     edge.label.position = 0.6)%>%
#'   plspm_curve(c("진로준비~~ 진로동기" = -2))%>%
#'   plspm_rotate_resid(c(진로동기= 160, 진로태도= 30))
#' }
#'
#'
plspm_rotate_resid = function(plotdata, resid_list=NULL, plot=TRUE){
  if(!is.null(resid_list)){
    plotdata <- semptools::rotate_resid(plotdata, rotate_resid_list = resid_list)

    if(plot){

    plot(plotdata)
    }
    return(plotdata)
  } else {
    stop("Input resid list ")
  }
}
