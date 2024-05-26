
#' plspm_CRAVE
#'
#' @param plsdata plspmdata
#'
#' @return CR and AVE
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(satisfaction)
#' #model 1
#' model1 = "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#' # # Generate the matrix
#' sat_path = plspm_lav2path(model1,
#'                           fct_order=c("IMAG", "EXPE","QUAL", "VAL", "SAT", "LOY" ))
#' # sat_path
#'
#' # Method 2: paths
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY")
#'   )
#' )
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#' sat_blocks1
#'
#' # vector of modes (reflective indicators):auto
#' sat_mod = rep("A", 6)
#' #############
#'
#' # apply plspm
#' satpls = plspm_sem(satisfaction,
#'                    path_matrix = sat_path,
#'                    blocks = sat_blocks1,
#'                    scaled = FALSE)
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#'
#' #CR a AVE
#' satpls%>% plspm_CRAVE()
#'
#' satpls_summary%>% plspm_CRAVE()
#'
#' satpls_boot%>% plspm_CRAVE()
#'
#' satpls_boot_summary%>% plspm_CRAVE()
#' #'
#' }
#'
#'
#'
plspm_CRAVE = function(plsdata){

  if(length(plsdata)==11){
    plsdf = plsdata
  }else if(length(plsdata)==13){
    plsdf = summary(plsdata)
  }

  .cr = plsdf$unidim%>%
    row2col("Latent")%>%
    dplyr::select(Latent, C.alpha, DG.rho)%>%
    dplyr::rename(Cronbach= C.alpha, `CR(DG.rho)` = DG.rho)

  .ave = plsdf$inner_summary$AVE

  res = cbind.data.frame(.cr, AVE = .ave)
  res
}
