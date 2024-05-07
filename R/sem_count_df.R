#' Functions for calculating degrees of freedom in SEM
#'
#' @param model lavaan syntax
#' @param type fit, all
#' @param growth grouwth =FALSE
#' @param sample.nobs  100(default)
#' @param whatLabels model, If it is an actual measurement, est
#' @param sig FALSE not outpur significant
#' @param plot plot FALSE
#'
#' @return table and plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' m1 <- "
#' y1 =~ x1 + x2 +x5
#' y2 =~ x3 + x4 +x6
#' y1 ~~ y2"
#' sem_count_df(m1)
#' sem_count_df(m1,"fit")
#'
#' #그래프를 보고 싶을 때
#' diagram_model(m1)
#' #이함수에서 보려면
#' sem_count_df(m1, plot=TRUE)
#' #전체를 보여주는 함수
#' sem_count_df(m1,"all")
#' sem_count_df(m1,"all", plot=TRUE )
#' #'
#' #'
#' m1a <- "
#' y1 =~ x1 + x2 +x5
#' y2 =~ x3 + x4 +x6
#' y1 ~~ 0*y2"
#' sem_count_df(m1a)
#' sem_count_df(m1a,"all")
#'
#' #이함수에서 보려면
#' sem_count_df(m1, plot=TRUE)
#' #'
#'
#' }
sem_count_df = function(model,
                        type="fit",
                        growth = FALSE,
                        sample.nobs = 100,
                        whatLabels="model",
                        sig=FALSE, plot=FALSE
){
  model_string = model

  # Remove instances of '~~' and '=~' from the model string
  modified_model_string <- gsub("~~", "", model_string)
  modified_model_string <- gsub("=~", "", modified_model_string)

  # Check if the modified model string contains any '~'

  if(growth){
    model.type ="growth"
  }else{
    if (grepl("~", modified_model_string, fixed = TRUE)) {
      # return("sem")
      model.type ="sem"
    } else {
      # return("cfa")
      model.type ="cfa"
    }
  }
  #generate sample data simulated
  testdata =  lavaan::simulateData( model = model,
                                    sample.nobs = sample.nobs,
                                    model.type = model.type  )
  # test calculated
  lav_obj = lavaan::sem(model, data= testdata)

  Known_information =lav_partable_independence(lav_obj)%>%lav_partable_ndat()

  para = inspect(lav_obj)

  para_lambda = para$lambda
  para_theta = para$theta
  para_psi =para$psi

  n_lambda = para_lambda%>%count_ne()
  n_theta = para_theta%>%count_ne()
  n_psi = para_psi%>%count_ne()

  measure_parameter = n_lambda + n_theta + n_psi

  df = Known_information - measure_parameter

  # Number of known information : p(p+1)/2

  measure_parameter_df = cbind.data.frame(
    Known_information,
    df= df,
    measure_parameter,
    n_lambda, n_theta, n_psi)
  colnames(measure_parameter_df)=c("total_information[p(p+1)/2]",
                                   "df",
                                   "model_parameter","lambda","theta","psi")

  if(plot){
    x11()
    g = diagram2(lav_obj, whatLabels=whatLabels,sig=sig)
  }else{
    g = NULL

  }


  measure_parameter_df= tibble(measure_parameter_df)
  # res=list( Parameters_to_measure=inspect(lav_obj),
  # # para_lambda,para_theta, para_psi,
  #           estimates = lav_obj ,
  #          Known_information = Known_information)

  res = list(measure_parameter_df,
             Parameters_to_measure=inspect(lav_obj),
             g=g )
  fit = tibble(measure_parameter_df)


  switch(type, all = res, fit= fit, res=res)
}


#
#' 0이 아닌 요소의 개수를 세는 함수
#'
#' @param matrix matrix
#'
#' @return count
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' #  행렬
#' matrix2 <- matrix(c(0, 1, 2, 0, 0, 0,
#'                     0, 0, 0, 0, 3, 4), nrow = 6, byrow = TRUE)
#' matrix2
#' count_ne(matrix2)
#'
#' # 주어진 theta 행렬
#'
#' theta <- matrix(c(5, 0, 0, 0, 0, 0,
#'                   0, 6, 0, 0, 0, 0,
#'                   0, 0, 7, 0, 0, 0,
#'                   0, 0, 0, 8, 0, 0,
#'                   0, 0, 0, 0, 9, 0,
#'                   0, 0, 0, 0, 0, 10), nrow = 6, byrow = TRUE)
#' theta
#' count_ne(theta)
#' }
#'
count_ne <- function(matrix) {
  count <- 0
  n <- nrow(matrix)
  for (i in 1:n) {
    for (j in 1:ncol(matrix)) {
      if (matrix[i, j] != 0) {
        count <- count + 1
      }
    }
  }
  return(count)
}
