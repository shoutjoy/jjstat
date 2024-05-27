#' plspm_loadings_plot
#'
#' @param plsres plspm data
#'
#' @return plot
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
#' satpls = plspm(satisfaction,
#'                    path_matrix = sat_path,
#'                    blocks = sat_blocks1,
#'                    scaled = FALSE)
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#'
#'
#' #plot
#' satpls %>%plspm_loadings_plot()
#' satpls$outer_model%>%plspm_loadings_plot()
#'
#'  }
#'
#'
plspm_loadings_plot = function(plsres){

  if(length(plsres)==13){
    plsdf = plsres[["outer_model"]]
  }else{
    plsdf = plsres
  }
  print(plsdf)

  ggplot(plsdf,
         aes(x = name, y = loading, fill = block)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_text(aes(label= round(loading,2)), vjust=-.5)+
    # threshold line (to peek acceptable loadings above 0.7)
    geom_hline(yintercept = 0.7, color = 'gray30', linetype=2) +
    geom_hline(yintercept = 0.5, color = 'gray10', linetype=2) +
    # add title
    ggtitle("Barchart of Loadings") +
    theme_bw()+
    # rotate x-axis names
    theme(
      axis.text.x = element_text(size=12, angle = 90, face="bold"),
      axis.text.y = element_text(size=12, angle = 0, face="bold")
    )

}
