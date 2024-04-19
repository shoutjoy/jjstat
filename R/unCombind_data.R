#' Functions to uncombind combind_data again
#'
#' @param data conbind data
#' @param into column name
#' @param sep  separate sep= " "
#' @param pattern remove tail
#' @param imp imputation tail
#'
#' @return results  two data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Generate example data2
#' combined_data2 <- data.frame(
#'   "H" = c("1.0587162 ***", "1.1269248 ***", "0.9163776 ***", "0.9530237 ***"),
#'   "H(H)" = c("1.1477347 ***", "0.5743712 ***", "1.4060606 ***", "0.8624314 ***"),
#'   "L" = c("0.50877193 ***", "1.94957983 *", "0.03515152 ***", "1.44531611 **")
#' )
#' # Generate example data2
#' combined_data <- data.frame(
#'   "H" = c("1.0587162(***)", "1.1269248(***)", "0.9163776(***)", "0.9530237(***)"),
#'   "H(H)" = c("1.1477347(***)", "0.5743712(***)", "1.4060606(***)", "0.8624314(***)"),
#'   "L" = c("0.50877193(***)", "1.94957983(*)", "0.03515152(***)", "1.44531611(**)")
#' )
#'
#' combined_data2
#' unCombind(combined_data2,sep=" ")
#'
#' combined_data
#' unCombind(combined_data, sep="\\(", pattern=")")
#'
#'
#' }
unCombind_data = function(data,
                     into =  c("value","sig"),
                     sep =" ",
                     pattern = NULL,
                     imp = ""){

  N_col = ncol(data)
  N_colnames = colnames(data)
  N_row = nrow(data)
  N_rownames = rownames(data)

  s1 = data %>% rownames_to_column() %>%
    pivot_longer(cols=2:(ncol(data)+1), names_to = "names", values_to="values") %>%
    separate(values, into= into, sep = sep)
  s1$value = as.numeric(s1$value)
  #pattern check
  if(is.null(pattern)){
    s1$sig = s1$sig
  }else{
    s1$sig = gsub(pattern, imp, s1$sig)
  }
  #result
  m1 = matrix(s1$value, ncol= N_col) %>%
    `colnames<-`(c(N_colnames))%>%
    `rownames<-`(c(N_rownames))

  m2 = matrix(s1$sig, ncol= N_col) %>%
    `colnames<-`(c(N_colnames))%>%
    `rownames<-`(c(N_rownames))

  #output
  list(raw = data, m1 = m1, m2 = m2)

}



#' Functions to uncombind combind_data again
#'
#' @param data conbind data
#' @param into column name
#' @param sep  separate sep= " "
#' @param pattern remove tail
#' @param imp imputation tail
#' @param type "all", res1 = m1, res2=m2
#'
#' @return results  two data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Generate example data2
#' combined_data2 <- data.frame(
#'   "H" = c("1.0587162 ***", "1.1269248 ***", "0.9163776 ***", "0.9530237 ***"),
#'   "H(H)" = c("1.1477347 ***", "0.5743712 ***", "1.4060606 ***", "0.8624314 ***"),
#'   "L" = c("0.50877193 ***", "1.94957983 *", "0.03515152 ***", "1.44531611 **")
#' )
#' # Generate example data2
#' combined_data <- data.frame(
#'   "H" = c("1.0587162(***)", "1.1269248(***)", "0.9163776(***)", "0.9530237(***)"),
#'   "H(H)" = c("1.1477347(***)", "0.5743712(***)", "1.4060606(***)", "0.8624314(***)"),
#'   "L" = c("0.50877193(***)", "1.94957983(*)", "0.03515152(***)", "1.44531611(**)")
#' )
#'
#' combined_data2
#' unCombind(combined_data2,sep=" ")
#'
#' combined_data
#' unCombind(combined_data, sep="\\(", pattern=")")
#'
#'
#' }
unCombind = function(data,
                     into =  c("value","sig"),
                     sep =" ",
                     type="all",
                     pattern = NULL,
                     imp = ""
){

  N_col = ncol(data)
  N_colnames = colnames(data)
  N_row = nrow(data)
  N_rownames = rownames(data)

  s1 = data %>% rownames_to_column() %>%
    pivot_longer(cols=2:(ncol(data)+1), names_to = "names", values_to="values") %>%
    separate(values, into= into, sep = sep)
  s1$value = as.numeric(s1$value)
  #pattern check
  if(is.null(pattern)){
    s1$sig = s1$sig
  }else{
    s1$sig = gsub(pattern, imp, s1$sig)
  }
  #result
  m1 = matrix(s1$value, ncol= N_col) %>%
    `colnames<-`(c(N_colnames))%>%
    `rownames<-`(c(N_rownames))

  m2 = matrix(s1$sig, ncol= N_col) %>%
    `colnames<-`(c(N_colnames))%>%
    `rownames<-`(c(N_rownames))

  #output
  res = list(raw = data, m1 = m1, m2 = m2)

  switch(type,
         res1 = m1,
         m1 = m1,
         res2 = m2,
         m2 = m2,
         all = res )

}
