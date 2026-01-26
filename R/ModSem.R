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
#' @param cat output syntax default FALSE
#' @param paths_name inde path name default TRUE
#' @param add_model add model syntax
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

ModSEM <- function(model = NULL, data = NULL, type = "res", ind = TRUE, cat=FALSE,
                interact = TRUE, auto = TRUE, n_name = 1, prefix = "a",
                paths_name=TRUE,add_model=NULL,
                ordered = NULL, sampling.weights = NULL,
                sample.cov = NULL, sample.mean = NULL, sample.th = NULL,
                sample.nobs = NULL, group = NULL, cluster = NULL,
                constraints = "", WLS.V = NULL, NACOV = NULL, ov.order = "model",
                ...) {

  # 조건에 따라 imlist와 New_data를 생성
  if (grepl(":", model)) {
    imlist <- lav_extract_imlist(model)
    New_data <- lav_latentProd(data, model)
  } else {
    imlist <- NULL
    New_data <- data
  }

  # 조건에 따라 New_model을 생성
  if (ind) {
    New_model <- lav_new_model(model, imlist)
    New_model <- lav_remodel(New_model,
                             interact = interact,
                             auto = auto, cat=cat,
                             n_name = n_name,
                             paths_name = paths_name,
                             add_model = add_model,
                             prefix = prefix)
  } else {
    New_model <- lav_new_model(model, imlist)
  }

  # SEM 모델 피팅
  fit <- tryCatch({
    sem(model = New_model, data = New_data,
        ordered = ordered, sampling.weights = sampling.weights,
        sample.cov = sample.cov, sample.mean = sample.mean, sample.th = sample.th,
        sample.nobs = sample.nobs, group = group, cluster = cluster,
        constraints = constraints, WLS.V = WLS.V, NACOV = NACOV, ov.order = ov.order,
        ...)
  }, error = function(e) {
    stop("Error in SEM model fitting: ", e)
  })

  switch(type, res = fit, fit = fit, data = New_data, model = New_model, imlist = imlist)
}

