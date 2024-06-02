#' plspm_model_lav
#'
#' @param paths paths
#' @param blocks  blocks
#'
#' @return lavaan syntax
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' jutpath1 = plspm_paths(
#'   row_names = c("자기효능감","진로동기","진로태도","진로준비"),
#'   relationship = list(
#'     path(from="자기효능감", to=c("진로동기","진로태도","진로준비")),
#'     path("진로동기", c("진로준비")),
#'     path("진로태도", c("진로준비"))
#'   )  )
#' jutpath1
#'
#' #측정모형
#' jut_blocks <- plspm_blocks(
#'   자기효능감 = item("C_S1","C_S2","C_S3","C_S4","C_S5"),
#'   진로동기 = item("A_M1","A_M2","A_M3"),
#'   진로태도 = item(  "B02", "B04", "B10","B14" ),#결정성
#'   진로준비 = item("D_P1","D_P2","D_P3")
#' )
#'
#' jut_blocks
#' #'
#' plspm_model_syntax(jutpath1, jut_blocks) %>%cat()

#' }
#'
plspm_model_lav = function(paths, blocks){
  res = paste(
    plspm_blocks2lav(blocks),
    "\n",
    plspm_paths2lav(paths)
  )
  res
}
