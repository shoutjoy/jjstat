#' lav_remodel, Models that create indirect effects
#'
#' @param model lavaan(original model)
#' @param prefix model hypothesis "a", or "H"
#' @param auto TRUe indirect effec auto, FALSE all path
#' @param start_node NULL If you type it, it starts with
#' @param end_node NULL and that's it
#' @param cat text puput console
#' @param text Output text to save and type
#' @param interact TRUe :(use) using SESM, FALSE (:) using sem()
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#'
#' data(satisfaction)
#'
#' # Example usage
#' model1 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#'
#'
#' # lav_remodel(model1)
#' lav_remodel(model1)%>%cat("\n")
#' lav_remodel(model1, interact=FALSE)%>%cat("\n")
#'
#' lav_remodel(model1, auto=FALSE)%>%cat("\n")
#'
#' lav_remodel(model1, start="EXPE", prefix="H", cat=TRUE)
#' lav_remodel(model1, cat=TRUE)
#'
#' lav_remodel(model1, cat=TRUE, text=TRUE)
#' lav_remodel(model1, cat=TRUE, text=FALSE)
#'
#' lav_remodel(model1, cat=FALSE, text=TRUE)
#' lav_remodel(model1, cat=FALSE, text=FALSE)
#'
#'
#' satis_sem = sem(lav_remodel(model1), satisfaction)
#' summary(satis_sem)
#' model1 %>%class()
#'
#'
#' }
#'
#'
lav_remodel = function(model,  prefix="a", auto=TRUE, start_node=NULL, end_node=NULL,
                       cat=FALSE, text=FALSE, interact=TRUE, paths_name= TRUE ){

  mm_model = lav_extract_mm2(model, interact = interact)
  sm_model = lav_extract_sm(model, prefix = prefix)
  ind_model = lav_extract_ind(lav_extract_sm(model, prefix = prefix),
                              de=TRUE, auto=auto,
                              start_node = start_node,
                              end_node = end_node,
                              paths_name=paths_name)

  re_model = paste("##Measurement model",
                   mm_model,"\n",
                   "##structure model(htypothesis) \n",
                   sm_model, "\n\n",
                   "##New_Parameters(indirect effects) \n",
                   ind_model)

  if(cat){
    if(text){
      cat("\n",re_model,"\n\n")
      cat("\nInput model syntax Start----------------------\n\n")
      make_c_text(re_model)
      cat("\n Copy Your model syntax  End-----------------\n\n")
      re_model

    }else{
      cat("\n",re_model,"\n\n")
      re_model
    }

  }else{

    if(text){
      cat("\nInput model syntax----------------------\n\n")
      make_c_text(re_model)
      cat("\n Copy Your model syntax-----------------\n\n")
      return(re_model)

    }else{
      re_model
    }
  }}
