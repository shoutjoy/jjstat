#' Search for and fill in empty data
#'
#' @param df data.frame
#' @param pattern default ""
#' @param imp iimputation , defalut NA
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' New = data.frame(term = c("(Intercept)", "age30대",
#'  "age40대", "gendermale", "manufacturergoogle",
#'  "manufacturersamsung",
#'  "age30대:gendermale", 'age40대:gendermale',
#'   'age30대:manufacturergoogle', "age40대:manufacturergoogle",
#'   "age30대:manufacturersamsung", 'age40대:manufacturersamsung'),
#' estimate = c(6.72127226828228, 0.222516673400011, 0.613427717767302,
#'  -0.451934840902044, -0.0405765656320795, 0.380612934433137,
#'   0.00538723055632342, 0.52238521880372, 0.0022025527504637,
#'   0.0243934423439851, 0.509454602341916, 0.00845576255419509),
#' std.error = c(0.029551066353128, 0.0393330112847198,
#' 0.0358559955535251, 0.0300846458760622, 0.0387717417205405,
#'  0.0352086354160786, 0.0383047023682364, 0.0356296478558013,
#'  0.0519877886919915, 0.0462375166064463,
#'  0.0454909790784573, 0.0420442735967295),
#' statistic = c(227.446014568974, 5.65724988074979,
#'  17.1080933131975, -15.0221093764524, -1.04654998283409,
#'  10.8102154467288, 0.140641493687488, 14.6615319050554,
#'   0.0423667327632075, 0.527568176976533,
#'    11.1990247882612, 0.201115677138321),
#' p.value = c(0, 1.53817738615173e-08, 1.29158703632758e-65,
#' 5.26066233107499e-51, 0.295307180741282, 3.0794597517271e-27,
#'  0.888153166786026, 1.13678932467476e-48,
#'  0.966206347935429, 0.59779908524448,
#'  4.12246475726497e-29, 0.840608123856931)
#' )%>%tibble()
#'
#' New
#'
#' dfs= New%>%p_mark_sig(ns="", unite=TRUE, unite_col="estimate")
#'
#' dfs%>%
#'   replace_df("0.000","< .001", col="p.value")
#'
#' dfs%>%
#'   replace_df("0.000","< .001", col=c("p.value", "std.error"))
#'
#'

#' }
#'
#'
replace_df = function(df, pattern = NA, imp = "", col = NULL) {
  if(is.null(col)) {
    df[df == pattern] <- imp
  } else {
    df[col] <- lapply(df[col], format)
    df[df == pattern] <- imp
  }
  df
}



#' Search for and fill in NA data
#'
#' @param df data.frame
#' @param pattern default ""
#' @param imp iimputation , defalut NA
#'
#' @return data.frame
#' @export
#'
replace_df2 = function(df, pattern = NA, imp = "", cat=TRUE) {
  na_counts <- colSums(is.na(df))
  cols_with_na <- names(na_counts[na_counts > 0])

  if(cat){
    cat("\n\n",paste0("Columns with [" ,pattern, "] values:"), cols_with_na, "\n\n")
  }


  for (col in cols_with_na) {
    df[[col]][is.na(df[[col]])] <- imp
  }

  return(df)
}


#' #' Search for and fill in NA data
#' #'
#' #' @param df data.frame
#' #' @param pattern default ""
#' #' @param imp iimputation , defalut NA
#' #'
#' #' @return data.frame
#' #' @export
#' #'
#' replace_NA = function(df, pattern = NA, imp = "", cat=TRUE) {
#'   na_counts <- colSums(is.na(df))
#'   cols_with_na <- names(na_counts[na_counts > 0])
#'
#'   if(cat){
#'     cat("\n\n",paste0("Columns with [" ,pattern, "] values:"), cols_with_na, "\n\n")
#'   }
#'
#'
#'   for (col in cols_with_na) {
#'     df[[col]][is.na(df[[col]])] <- imp
#'   }
#'
#'   return(df)
#' }

