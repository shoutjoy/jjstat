#' Search for and fill in empty data
#'
#' @param df data.frame
#' @param pattern default ""
#' @param imp iimputation , defalut NA
#'
#' @return data.frame
#' @export
#'
replace_df = function(df, pattern = NA, imp = "" ){


  df[df == pattern ] <- imp

  df
}



#' Search for and fill in empty data
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


