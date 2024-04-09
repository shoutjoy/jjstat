#' Add skewness and kurtosis normality checks to descriptive statistics results
#'
#' @param data mysummary data
#' @param skew select skew
#' @param kurt select kurt
#'
#' @return normalitydata
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mysummary(mtcars)
#' mysummary(mtcars)%>%add_normality()
#'
#'
#' }
#'
add_normality = function(data,  skew ="Skew", kurt="Kurt"){
  Skew = all_of(skew)
  Kurt = all_of(kurt)

  res= data %>% mutate(
    Skew_z = Skew/sqrt((6*N*((N-1))/((N-2)*(N+1)*(N+3)))),
    Kurt_z = Kurt/sqrt((24*N*(N-1)*(N-1))/((N-3)*(N-2)*(N+3)*(N-5))),
    skew_TF = ifelse( abs(Skew_z) < 1.96, "Good",
                      ifelse( abs(Skew_z) < 3, "Fair","NO")),
    kurt_TF = ifelse( abs(Kurt_z) < 1.96, "Good",
                      ifelse( abs(Kurt_z) < 3, "Fair","NO"))
  )
  res%>%select(-MIN,-MAX)
}


#' Multivariate normality tests
#'
#' @param multiVariable_data data
#'
#' @return normality
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars%>%Normality_test()
#' }
#'
Normality_test= function(multiVariable_data){
  multiVariable_data%>%
    MVN::mvn(mvnTest = "mardia",univariateTest="SW")

}
