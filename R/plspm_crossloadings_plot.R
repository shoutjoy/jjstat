
#' plspm_crossloadings_plot
#'
#' @param plsres plspm result
#' @param wide plot type TRUE block, FALSE wide plot
#'
#' @return pplot
#' @export
#'
#' @examples
#'
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
#' #'
#' satpls %>% plspm_crossloadings_plot()
#' satpls %>% plspm_crossloadings_plot(T)
#' satpls$crossloadings %>% plspm_crossloadings_plot()
#' satpls$crossloadings %>% plspm_crossloadings_plot(T)
#' #'
#' #'
#' }
#'
plspm_crossloadings_plot = function(plsres, wide = F){

  if(length(plsres)==13){
    plsdf = plsres$crossloadings

  }else{
    plsdf = data.frame(plsres)
  }

  if(wide){
    plsdf %>%
      long_df(names_to = "latent",
              rownames_to_column = FALSE)%>%
      ggplot(aes(x = name, y = values, fill = block)) +
      geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
      geom_text(aes(label= substring(name,1,2)), vjust=-.5,size=3)+
      theme_bw() +
      labs(title = "Crossloadings for Convergent Validity",
           x = "blocks",
           y = "Crossloading valuess") +
      theme(axis.text.x = element_text(size = 11,angle = 90, hjust = 0.5, face="bold"),
            axis.text.y = element_text(size = 12,angle = 0, hjust = 0.5, face="bold"),
            # axis.strip = element_text(size = 16),
            legend.position = "right",
            legend.title = element_blank())+
      ylim(0,1.08)+
      geom_hline(yintercept = 0.5, color = "gray20", linetype = 2) +
      facet_wrap(block ~ latent )
  }else{
    plsdf %>%
      long_df(names_to = "latent",
              rownames_to_column = FALSE)%>%
      ggplot(aes(x = name, y = values, fill = block)) +
      geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
      geom_text(aes(label= substring(name,1,2)), vjust=-.5,size=3)+
      theme_bw() +
      labs(title = "Crossloadings for Convergent Validity",
           x = "blocks",
           y = "Crossloading valuess") +
      theme(axis.text.x = element_text(size = 11,angle = 90, hjust = 0.5, face="bold"),
            axis.text.y = element_text(size = 12,angle = 0, hjust = 0.5, face="bold"),
            # axis.strip = element_text(size = 16),
            legend.position = "right",
            legend.title = element_blank())+
      ylim(0,1.08)+
      geom_hline(yintercept = 0.5, color = "gray20", linetype = 2) +
      facet_wrap(~ latent , ncol=2)

  }
}
