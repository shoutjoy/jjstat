#' extranct measurement
#'
#' @param lav_syn model lavaan syntax
#' @param text text output
#'
#' @return text model
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # # Example usage:
#' data(MarshWenHau)
#' str(MarshWenHau)
#'
#' mwh = MarshWenHau
#'
#' model2 <- "
#' f1 =~ x1 + x2 + x3
#' f1Xf2 =~ f1:f2
#' f2 =~ x4 + x5 + x6
#' f3 =~ y1 + y2 + y3
#'
#' f3 ~ f1 + f2
#' f3 ~ f1Xf2
#'
#' f1Xf2 ~~ 0*f1
#' f1Xf2 ~~ 0*f2
#' "
#'
#' # interaction model
#' intsem =  SEM(model2, data= MarshWenHau)
#' summary(intsem)
#' intsem %>%lav_semPaths2(color=list(lat="darkred",man="gold"))
#'
#'
#' ##2nd
#' #base model
#' lav_extract_mm(model2) %>%cat()
#' #interaction item list
#' lav_extract_imlist(model3)
#' #data
#' lav_latentProd(mwh, model3)%>%head()
#'
#' #extracted interaction term
#' lav_extract_int(model2, lav_extract_imlist(model3))
#'
#' # or interaction items ; imlist
#' imlist <- list(
#'   .f1Xf2 = c("f1", "f2"),
#'   .i_f1_f4 = c("f1", "f4")
#' )
#' # names(imlist)
#' lav_extract_int(model2, imlist)
#' #direct
#' lav_extract_int(model2, imlist= list(.f1Xf2 = c("f1", "f2"),
#'                                      .i_f1_f4 = c("f1", "f4")))
#' #estimates model
#' intsem =  SEM(lav_new_model(model2, lav_extract_imlist(model2)),
#'               data= lav_latentProd(mwh, model2))
#'
#' }
#'
lav_extract_mm <- function(lav_syn, text = FALSE) {
  lines <- unlist(strsplit(lav_syn, "\n"))
  measurement_models <- c()
  for (line in lines) {
    if (!grepl("^#", line) && grepl("=~", line) && !grepl(":", line)) {
      measurement_models <- c(measurement_models, trimws(line))
    }
  }
  base_model <- paste(measurement_models, collapse = "\n")
  if (text) {
    cat('base_model = "', "\n", base_model, "\n", '"', "\n", sep = "")
  } else {
    return(paste0("\n", base_model, "\n"))
  }
}


#' extranct measurement, Extract all structural models
#'
#' @param lav_syn model lavaan syntax
#' @param text text output
#' @param interact FALSE exclude :, TRue include :
#'
#' @return text model
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # # Example usage:
#' data(MarshWenHau)
#' str(MarshWenHau)
#'
#' mwh = MarshWenHau
#'
#' model2 <- "
#' f1 =~ x1 + x2 + x3
#' f1Xf2 =~ f1:f2
#' f2 =~ x4 + x5 + x6
#' f3 =~ y1 + y2 + y3
#'
#' f3 ~ f1 + f2
#' f3 ~ f1Xf2
#'
#' f1Xf2 ~~ 0*f1
#' f1Xf2 ~~ 0*f2
#' "
#'
#' # interaction model
#' intsem =  SEM(model2, data= MarshWenHau)
#' summary(intsem)
#' intsem %>%lav_semPaths2(color=list(lat="darkred",man="gold"))
#'
#'
#' ##2nd
#' #base model
#' lav_extract_mm(model2) %>%cat()
#' #interaction item list
#' lav_extract_imlist(model3)
#' #data
#' lav_latentProd(mwh, model3)%>%head()
#'
#' #extracted interaction term
#' lav_extract_int(model2, lav_extract_imlist(model3))
#'
#' # or interaction items ; imlist
#' imlist <- list(
#'   .f1Xf2 = c("f1", "f2"),
#'   .i_f1_f4 = c("f1", "f4")
#' )
#' # names(imlist)
#' lav_extract_int(model2, imlist)
#' #direct
#' lav_extract_int(model2, imlist= list(.f1Xf2 = c("f1", "f2"),
#'                                      .i_f1_f4 = c("f1", "f4")))
#' #estimates model
#' intsem =  SEM(lav_new_model(model2, lav_extract_imlist(model2)),
#'               data= lav_latentProd(mwh, model2))
#'
#' #'
#' model2 <- "
#'  f1 =~ x1 + x2 + x3
#'  f1Xf2 =~ f1:f2
#'  f2 =~ x4 + x5 + x6
#'  f3 =~ y1 + y2 + y3
#'
#'  f3 ~ f1 + f2
#'  f3 ~ f1Xf2
#'
#'  f1Xf2 ~~ 0*f1
#'  f1Xf2 ~~ 0*f2
#'  "
#'
#' lav_extract_mm2(model2)
#' lav_extract_mm2(model2, interact=F)
#' lav_extract_mm2(model2, interact=T)
#' #'
#' #'
#' }
#'
lav_extract_mm2 <- function(lav_syn, text = FALSE, interact= TRUE) {
  lines <- unlist(strsplit(lav_syn, "\n"))
  measurement_models <- c()

  if(interact){
    for (line in lines) {
      if (!grepl("^#", line) && grepl("=~", line) ) {
        measurement_models <- c(measurement_models, trimws(line))
      }
    }


  }else{
    for (line in lines) {
      if (!grepl("^#", line) && grepl("=~", line) && !grepl(":", line)) {
        measurement_models <- c(measurement_models, trimws(line))
      }
    }


  }

  base_model <- paste(measurement_models, collapse = "\n")
  if (text) {
    cat('base_model = "', "\n", base_model, "\n", '"', "\n", sep = "")
  } else {
    return(paste0("\n", base_model, "\n"))
  }
}


