#' Regression with covariance, generate structural equation data restoration
#'
#' @param covmat cov matrix
#' @param n sample size default = 300
#' @param dv_mean dv mean
#' @param iv_means iv means
#' @param means total means default null
#' @param cormat correlation matrix
#' @param sds sds is need cor to cov
#' @param seed set seed today Sys.Data
#'
#' @return sim data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #simdata mean setting 0
#' #set.seed(todaty)
#' cov(mtcars[,c("mpg","hp","wt")]) %>%cov_simdata(n=32)
#' cov(mtcars[,c("mpg","hp","wt")]) %>%cov_simdata(n=32, means=c(0,0,0))
#'
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32, means=c(20.09, 146.69 ,3.22))
#'
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32, dv_mean=20.09, iv_means=c(146.69 ,3.22))
#'
#' #simdata
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32) %>%
#'   lm(formula = mpg ~ hp + wt)%>%summary()
#'
#' #revised mean data Closer to real-world data
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32, dv_mean=20.09, iv_means=c(146.69 ,3.22)) %>%
#'   lm(formula = mpg ~ hp + wt)%>%summary()
#'
#'
#' #real data
#' lm(formula = mpg ~ hp +wt,data=mtcars)%>%summary()
#'
#'
#'
#' # define sample statistics
#' x <- MASS::Cars93[,c("EngineSize", "Horsepower", "RPM")]
#' covx <- cov(x)
#' covx <- matrix(c(1.076122, 39.776999, -339.163745,
#'                  39.776999, 2743.079, 1146.634,
#'                  -339.163745, 1146.634, 356088.7097), nrow = 3,
#'                dimnames = list(c("EngineSize", "Horsepower", "RPM"),
#'                                c("EngineSize", "Horsepower", "RPM")))
#' covx
#' # 함수 실행
#' sim_data <- cov_simdata(covmat = covx, n = 100)
#' head(sim_data)
#' # check
#' sim_data <- cov_simdata(covmat = covx, n = 100)
#' lm( EngineSize~ Horsepower+RPM, data = sim_data) %>% summary()
#' #real check
#' lm( EngineSize ~ Horsepower +RPM , data=x)%>% summary()
#'
#'
#'
#' #'
#' }
cov_simdata <- function(covmat = NULL, n = 300,
                        dv_mean = 0, iv_means = rep(0, ncol(covmat)-1),
                        means=NULL,
                        cormat = NULL, sds = rep(1, ncol(cormat)),
                        seed = NULL) {
  # setseed as.integer(Sys.Date()
  if (is.null(seed)) {
    seed = as.integer(Sys.Date())
    cat("Seed value:", seed, "\n")
  }
  set.seed(seed)

  # Number of variables
  nvar <- ncol(covmat)

  # Create a covariance matrix
  if (!is.null(cormat)) {
    covx <- jjstat::Cor2cov(cormat, sds = sds)
  } else if (!is.null(covmat)) {
    covx <- covmat
  } else {
    stop("Either 'covmat' or 'cormat' must be provided.")
  }

  if(is.null(means)){
    # Generate random numbers from multivariate normal distributions
    sim_data <- MASS::mvrnorm(n, mu = c(dv_mean, iv_means), Sigma = covx, empirical = TRUE)
  }else{
    sim_data <- MASS::mvrnorm(n, mu = means, Sigma = covx, empirical = TRUE)
  }

  # Convert to dataframes
  sim_data <- as.data.frame(sim_data)

  return(sim_data)
}



#' Regression with covariance, generate structural equation data restoration
#'
#' @param covmat cov matrix
#' @param n sample size default = 300
#' @param dv_mean dv mean
#' @param iv_means iv means
#' @param means total means default null
#' @param cormat correlation matrix
#' @param sds sds is need cor to cov
#' @param seed set seed today Sys.Data
#'
#' @return sim data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #simdata mean setting 0
#' #set.seed(todaty)
#' cov(mtcars[,c("mpg","hp","wt")]) %>%cov_simdata(n=32)
#' cov(mtcars[,c("mpg","hp","wt")]) %>%cov_simdata(n=32, means=c(0,0,0))
#'
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32, means=c(20.09, 146.69 ,3.22))
#'
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32, dv_mean=20.09, iv_means=c(146.69 ,3.22))
#'
#' #simdata
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32) %>%
#'   lm(formula = mpg ~ hp + wt)%>%summary()
#'
#' #revised mean data Closer to real-world data
#' cov(mtcars[,c("mpg","hp","wt")]) %>%
#'   cov_simdata(n=32, dv_mean=20.09, iv_means=c(146.69 ,3.22)) %>%
#'   lm(formula = mpg ~ hp + wt)%>%summary()
#'
#'
#' #real data
#' lm(formula = mpg ~ hp +wt,data=mtcars)%>%summary()
#'
#'
#'
#' # define sample statistics
#' x <- MASS::Cars93[,c("EngineSize", "Horsepower", "RPM")]
#' covx <- cov(x)
#' covx <- matrix(c(1.076122, 39.776999, -339.163745,
#'                  39.776999, 2743.079, 1146.634,
#'                  -339.163745, 1146.634, 356088.7097), nrow = 3,
#'                dimnames = list(c("EngineSize", "Horsepower", "RPM"),
#'                                c("EngineSize", "Horsepower", "RPM")))
#' covx
#' # 함수 실행
#' sim_data <- simdata_cov(covmat = covx, n = 100)
#' head(sim_data)
#' # check
#' sim_data <- simdata_cov(covmat = covx, n = 100)
#' lm( EngineSize~ Horsepower+RPM, data = sim_data) %>% summary()
#' #real check
#' lm( EngineSize ~ Horsepower +RPM , data=x)%>% summary()
#'
#'
#'
#' #'
#' }
simdata_cov <- function(covmat = NULL, n = 300,
                        dv_mean = 0, iv_means = rep(0, ncol(covmat)-1),
                        means=NULL,
                        cormat = NULL, sds = rep(1, ncol(cormat)),
                        seed = NULL) {
  # setseed as.integer(Sys.Date()
  if (is.null(seed)) {
    seed = as.integer(Sys.Date())
    cat("Seed value:", seed, "\n")
  }
  set.seed(seed)

  # Number of variables
  nvar <- ncol(covmat)

  # Create a covariance matrix
  if (!is.null(cormat)) {
    covx <- jjstat::Cor2cov(cormat, sds = sds)
  } else if (!is.null(covmat)) {
    covx <- covmat
  } else {
    stop("Either 'covmat' or 'cormat' must be provided.")
  }

  if(is.null(means)){
    # Generate random numbers from multivariate normal distributions
    sim_data <- MASS::mvrnorm(n, mu = c(dv_mean, iv_means), Sigma = covx, empirical = TRUE)
  }else{
    sim_data <- MASS::mvrnorm(n, mu = means, Sigma = covx, empirical = TRUE)
  }

  # Convert to dataframes
  sim_data <- as.data.frame(sim_data)

  return(sim_data)
}
