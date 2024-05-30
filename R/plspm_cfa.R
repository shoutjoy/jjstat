#' plspm CFA
#'
#' @param plsres_boot bootstrap plspm data
#' @param type all, cfa1, cfa2, item, CRAVE, fl1981, htmt, f2. total_effect
#' @param axis_x 1.2 setting efffecbar text size
#'
#' @return  multiple data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(satisfaction)
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY"))
#' )
#' sat_path
#'
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#'
#' sat_blocks1
#'
#' # apply plspm
#' # satpls = plspm(satisfaction,
#' #                path_matrix = sat_path,
#' #                blocks = sat_blocks1,
#' #                scaled = FALSE)
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#' ##
#' satpls_boot%>%plspm_cfa()
#' satpls_boot%>%plspm_cfa("res")
#'
#' satpls_boot%>%plspm_cfa(type="cfa1")
#' satpls_boot%>%plspm_cfa(type="cfa2")
#' satpls_boot%>%plspm_cfa(type="cor")
#' satpls_boot%>%plspm_cfa(type="model")
#' satpls_boot%>%plspm_cfa(type="inner_model") %>%plspm_inner_model_sig()
#' satpls_boot%>%plspm_cfa(type="outer_model")
#' satpls_boot%>%plspm_cfa(type="effectbar")
#'
#' }
#'
#'
plspm_cfa = function(plsres_boot, type="all", axis_x=1.2){
  # summary
  res_boot = lapply(plsres_boot$boot,
                    function(x){ x%>%
                        row2col("relationships")%>%
                        Round(3)%>%
                        add_t_sig(3,4,5,T,ns="")%>%
                        unite_ci() %>%
                        rename(경로계수=Original,
                               평균계수=Mean.Boot, 표준오차=Std.Error,
                               관계 = relationships )
                      #  dplyr::select(-Original)
                    })


  plsres_boot = plsres_boot

  weight = res_boot$weight
  loading = res_boot$loading
  path = res_boot$path
  rsq= res_boot$rsq
  total.efs = res_boot$total.efs

  # indicator = plspm_loadings(res_boot$boot$loading)
  item = res_boot$loadings %>%rename(신뢰구간=`95%CI`)%>%
    dplyr::select(-평균계수)%>%
    separate(관계,c("잠재변수","측정변수"), sep="-")
  CR_AVE = plspm_CRAVE(plsres_boot)
  CR_AVE_partial = plspm_CRAVE(plsres_boot)%>%
    dplyr::select(-Cronbach)%>%rename(잠재변수= Latent, CR=`CR(DG.rho)`)
  item_combine = full_join(item, CR_AVE_partial, by="잠재변수")%>%
    dplyr::select(-신뢰구간)%>%
    nice_table()%>%dall()

  fl = plspm_fl(plsres_boot)
  htmt = plspm_htmt(plsres_boot$data, plspm_extract_blocks(plsres_boot$model))
  total_effect = plsres_boot$effect %>%cut_print_all()
  effectssize = plspm_f2(plsres_boot)

  #effect join
  total_effect_sig = full_join(total_effect,
                               res_boot$total.efs%>%dplyr::select(-경로계수, -평균계수, -표준오차)%>%
                                 rename(relationships=관계),
                               by="relationships")


  # ###plot

  ##CFA
  loadings_plot = plspm_loadings_plot(plsres_boot)%>%
    suppressWarnings()
  corssloadings_plot = plspm_crossloadings_plot(plsres_boot, T)

  paths_plot = plspm_path_coefs_plot(plsres_boot)%>%
    suppressWarnings()
  effect_bar = plspm_effectbar(plsres_boot, axis_x=axis_x, col=c("gray30","gray70"))


  # list(item,CR_AVE, fl, htmt,total_effect,effectssize ,loadings_plot)
  all = list(
    item = item,
    CR_AVE = CR_AVE,
    item = item_combine,
    fl_1981 = fl,
    htmt= htmt,
    path = path%>%dplyr::select(-평균계수,-`95%CI`),
    total_effect_sig=total_effect_sig,
    f2= effectssize,
    loadings_plot=loadings_plot
  )

  switch(type,
         all = all,
         pass = plsres_boot,
         res = summary(plsres_boot),
         reshape2 =  res_boot,
         boot =  res_boot,
         item = item,
         cfa1 = item_combine,
         cfa2 = list(fornell_locker=fl, HTMT=htmt),
         CRAVE = CR_AVE,
         fl1981 = fl,
         htmt = htmt,
         cor = plsres_boot$score%>%cor %>%lowerMat("",1)%>%dall(),
         # paths = res_boot$boot$pahts,
         total_effect=total_effect,
         f2=effectssize,
         loadings_plot=loadings_plot,
         crossloadngs_plot= corssloadingsplot,
         paths_plot = paths_plot,
         effect_bar = effect_bar,
         inner_summary = plsres_boot$inner_summary,
         unidim = plsres_boot$unidim,
         model = plsres_boot$model,
         data = plsres_boot$data,

         weight = res_boot$weight,
         loading = res_boot$loading,
         path = res_boot$path,
         rsq= res_boot$rsq,
         total.efs = res_boot$total.efs,

         gof= plsres_boot$gof,
         crossloadings = plsres_boot$crossloadings,
         inner_model = plsres_boot$inner_model,
         outer_model = plsres_boot$outer_model,
         outer_mpath_coefsodel = plsres_boot$path_coefs,
         scores = plsres_boot$scores,
         manifests = plsres_boot$manifests,
  )
  # res_boot
}
