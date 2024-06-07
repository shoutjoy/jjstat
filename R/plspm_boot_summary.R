#' plspm_boot_summary
#'
#' @param pls_boot pls boot data
#' @param type res, all, weight, loadings, paths, rsq, total.efs
#'
#' @return data
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#' jutpls_boot%>%plspm_boot_summary()
#' jutpls_boot$boot%>%plspm_boot_summary()
#'
#'}
#'
#'
plspm_boot_summary <- function(pls_boot, type="res") {
  if (length(pls_boot) == 13) {
    pls_df <- pls_boot$boot
  } else if (length(pls_boot) == 5) {
    pls_df <- pls_boot
  } else {
    stop("The structure of pls_boot is not recognized.")
  }

  res <- lapply(pls_df, function(x) {
    x %>%
      row2col("relationships") %>%
      Round(3) %>%
      add_t_sig(3, 4, 5, TRUE, ns = "") %>%
      unite_ci() %>%
      rename(
        경로계수 = Original,
        평균계수 = Mean.Boot,
        표준오차 = Std.Error
      )
  })

   weights= res$weights
   loadings = res$loadings
   paths =  res$paths
   rsq = res$rsq
   total.efs = res$total.efs

  # return(res)

   switch(type, res=res, all= res,
          weights = weights,
          loadings= loadings,
          paths =paths,
          rsq = rsq,
          total.efs= total.efs


          )

}
