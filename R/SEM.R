#' Title
#'
#' @param model A description of the user-specified model. Typically, the model is
#'  described using the lavaan model syntax. See model.syntax for more information.
#'   Alternatively, a parameter table (eg. the output of the lavaanify() function)
#'   is also accepted.
#' @param data An optional data frame containing the observed variables used in the model.
#'  If some variables are declared as ordered factors,
#'  lavaan will treat them as ordinal variables.
#' @param type res, fit, New_model, New_data, imlist
#' @param ordered Character vector. Only used if the data is in a data.frame.
#' Treat these variables as ordered (ordinal) variables, if they are endogenous
#'  in the model. Importantly, all other variables will be treated as numeric
#'   (unless they are declared as ordered in the data.frame.) Since 0.6-4,
#'   ordered can also be logical. If TRUE, all observed endogenous variables are
#'   #'    treated as ordered (ordinal). If FALSE, all observed endogenous variables
#'    are considered to be numeric (again, unless they are declared as ordered
#'    in the data.frame.)
#' @param sampling.weights A variable name in the data frame containing sampling
#' weight information. Currently only available for non-clustered data.
#' Depending on the sampling.weights.normalization option, these weights may be
#' rescaled (or not) so that their sum equals the number of observations
#' (total or per group).
#' @param sample.cov 	Numeric matrix. A sample variance-covariance matrix.
#' The rownames and/or colnames must contain the observed variable names.
#'  For a multiple group analysis, a list with a variance-covariance matrix
#'  for each group.
#' @param sample.mean A sample mean vector. For a multiple group analysis,
#' a list with a mean vector for each group.
#' @param sample.th Vector of sample-based thresholds.
#' For a multiple group analysis, a list with a vector of thresholds for each group.
#' @param sample.nobs Number of observations if the full data frame is missing and
#' only sample moments are given. For a multiple group analysis, a list or a vector
#' with the number of observations for each group.
#' @param group Character. A variable name in the data frame defining
#' the groups in a multiple group analysis.
#' @param cluster 	Character. A (single) variable name in the data frame defining
#'   the clusters in a two-level dataset.
#' @param constraints Additional (in)equality constraints not yet included
#' in the model syntax. See model.syntax for more information.
#' @param WLS.V A user provided weight matrix to be used by estimator "WLS";
#'  if the estimator is "DWLS", only the diagonal of this matrix will be used.
#'  For a multiple group analysis, a list with a weight matrix for each group.
#'   The elements of the weight matrix should be in the following order (if all data is
#'   continuous): first the means (if a meanstructure is involved),
#'   then the lower triangular elements of the covariance matrix including the
#'   diagonal, ordered column by column. In the categorical case: first
#'   the thresholds (including the means for continuous variables),
#'   then the slopes (if any), the variances of continuous variables (if any),
#'    and finally the lower triangular elements of the correlation/covariance
#'     matrix excluding the diagonal, ordered column by column.
#' @param NACOV A user provided matrix containing the elements of (N times)
#' the asymptotic variance-covariance matrix of the sample statistics.
#' For a multiple group analysis, a list with an asymptotic variance-covariance
#'  matrix for each group. See the WLS.V argument for information about
#'  the order of the elements.
#' @param ov.order Character. If "model" (the default), the order of the observed
#' variable names (as reflected for example in the output of lavNames())
#' is determined by the model syntax. If "data", the order is determined
#' by the data (either the full data.frame or the sample (co)variance matrix).
#'  If the WLS.V and/or NACOV matrices are provided,
#'  this argument is currently set to "data".
#' @param ind Automatically Adding Indirect Effects
#' @param interact interaction term extract possinle TRUE
#' @param auto auto genrate indirect path
#' @param n_name indirect name number n_name =1
#' @param prefix prefix "a" --"H" change possible
#' @param ... Many more additional options can be defined, using 'name = value'. See lavOptions for a complete list.
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' ## The industrialization and Political Democracy Example
#' ## Bollen (1989), page 332
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit <- sem(model, data = PoliticalDemocracy)
#' summary(fit, fit.measures = TRUE)
#' #'
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
#' ##2nd---------
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
#' }
#'
#'
SEM = function(model = NULL, data = NULL, type="res", ind=TRUE,
               interact=TRUE, auto=TRUE,n_name=1, prefix="a",
               ordered = NULL, sampling.weights = NULL,
               sample.cov = NULL, sample.mean = NULL, sample.th = NULL,
               sample.nobs = NULL, group = NULL, cluster = NULL,
               constraints = "", WLS.V = NULL, NACOV = NULL, ov.order = "model",
               ...){



  imlist = lav_extract_imlist(model)
  New_data = lav_latentProd(data, model)

  if(ind){
    New_model = lav_new_model(model, imlist)
    New_model = lav_remodel(New_model,
                            interact = interact,
                            auto = auto,
                            n_name=n_name,
                            prefix= prefix)

  }else{

    New_model = lav_new_model(model, imlist)
  }

  #  lav_extract_int(model2, lav_extract_imlist(model3))


  fit =  sem(model = New_model, data = New_data,
             ordered = ordered, sampling.weights = sampling.weights,
             sample.cov = sample.cov, sample.mean = sample.mean, sample.th = sample.th,
             sample.nobs = sample.nobs, group = group, cluster = cluster,
             constraints = constraints, WLS.V = WLS.V, NACOV = NACOV, ov.order = ov.order,
             ...

  )

  switch(type, res = fit, fit =fit , data= New_data, model = New_model, imlist=imlist)

}


#' Function to extract interaction terms from the lavaan syntax,
#' excluding commented lines.
#'
#' @param lav_syn lavaan syntax
#' @param prefix . dataname prefix
#'
#' @return list
#' @export
#'
#' @examples
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
lav_extract_imlist <- function(lav_syn, prefix = ".") {
  lines <- unlist(strsplit(lav_syn, "\n"))
  interaction_terms <- list()
  for (line in lines) {
    if (!grepl("^#", line) && grepl("=~", line) && grepl(":", line)) {
      interaction_name <- sub(" =~.*", "", trimws(line))
      interaction_name <- trimws(interaction_name) # 이름에서 공백 제거
      interaction_name <- paste0(prefix, interaction_name) # 접두어 추가
      components <- sub(".*=~ ", "", trimws(line))
      components <- unlist(strsplit(components, ":"))
      components <- trimws(components)
      interaction_terms[[interaction_name]] <- components
    }
  }
  return(interaction_terms)
}


#' Functions to generate data for interaction terms in a dataset
#'
#' @param data original data
#' @param lav_syn lavaan syntax
#' @param prefix . prefix
#'
#' @return data.frame
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
lav_latentProd <- function(data, lav_syn, prefix=".") {
  # interaction_terms를 생성할 때 prefix를 인수로 전달합니다.
  interaction_terms <- lav_extract_imlist(lav_syn, prefix)
  base_model <- lav_extract_mm(lav_syn)

  base_sem <- tryCatch({
    lavaan::sem(base_model, data = data)
  }, error = function(e) {
    stop("Error in SEM model fitting: ", e$message)
  })

  fs_data <- tryCatch({
    lavPredict(base_sem)
  }, error = function(e) {
    stop("Error in latent variable prediction: ", e$message)
  })

  # 잠재 변수 이름을 가져옵니다.
  latent_var_names <- colnames(fs_data)

  for (term in names(interaction_terms)) {
    components <- interaction_terms[[term]]
    interaction_name <- term

    # 상호작용 항의 구성 요소가 잠재 변수 이름과 일치하는지 확인합니다.
    if (all(components %in% latent_var_names)) {
      data[[interaction_name]] <- fs_data[, components[1]] * fs_data[, components[2]]
    } else {
      stop(paste("Error: Components", components[1], "or",
                 components[2], "not found in fs_data"))
    }
  }

  return(data)
}


#' lav_extract_int
#'
#' @param lav_syn lavaan
#' @param imlist item list
#'
#' @return interation data
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
lav_extract_int <- function(lav_syn, imlist=NULL) {
  # Split the input into lines
  lines <- unlist(strsplit(lav_syn, "\n"))

  # Filter lines that contain the character ":"
  extracted_lines <- lines[grep(":", lines)]

  # If imlist is not NULL, replace the relevant parts
  if (!is.null(imlist)) {
    for (i in seq_along(extracted_lines)) {
      parts <- unlist(strsplit(extracted_lines[i], " =~ "))
      if (length(parts) == 2) {
        left_part <- parts[1]
        right_part <- paste("=~", names(imlist)[i])
        extracted_lines[i] <- paste(left_part, right_part, sep=" ")
      }
    }
  }

  return(extracted_lines)
}



#' lav_new_model: Generate interaction new model
#'
#' @param lav_syn original lavaan syntax model
#' @param imlist interaction data lav_extract_imlist(model)
#'
#' @return model text
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
lav_new_model <- function(lav_syn, imlist=NULL) {
  # Split the input into lines
  lines <- unlist(strsplit(lav_syn, "\n"))

  # Find the indices of the lines that contain the character ":"
  colon_indices <- grep(":", lines)

  # Get the lines from lav_extract_int
  extracted_lines <- lav_extract_int(lav_syn, imlist)

  # Initialize a counter for the extracted lines
  extracted_counter <- 1

  # Replace the lines that contained ":" with the extracted lines
  for (index in colon_indices) {
    lines[index] <- extracted_lines[extracted_counter]
    extracted_counter <- extracted_counter + 1
  }

  # Combine the lines back into a single string
  new_model <- paste(lines, collapse = "\n")

  return(new_model)
}





#' lav_new_model: Generate interaction new model
#'
#' @param lav_syn original lavaan syntax model
#' @param imlist interaction data lav_extract_imlist(model)
#'
#' @return model text
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
#' intsem =  sem(lav_interact_model(model2, lav_extract_imlist(model2)),
#'               data= lav_latentProd(mwh, model2))
#'
#'
#'
#'
#' }
#'
lav_interact_model <- function(lav_syn, imlist=NULL) {
  # Split the input into lines
  lines <- unlist(strsplit(lav_syn, "\n"))

  # Find the indices of the lines that contain the character ":"
  colon_indices <- grep(":", lines)

  # Get the lines from lav_extract_int
  extracted_lines <- lav_extract_int(lav_syn, imlist)

  # Initialize a counter for the extracted lines
  extracted_counter <- 1

  # Replace the lines that contained ":" with the extracted lines
  for (index in colon_indices) {
    lines[index] <- extracted_lines[extracted_counter]
    extracted_counter <- extracted_counter + 1
  }

  # Combine the lines back into a single string
  new_model <- paste(lines, collapse = "\n")

  return(new_model)
}

