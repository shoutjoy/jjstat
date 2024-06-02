#' plspm_indicator_order
#'
#' @param model_syntax  lavaan
#'
#' @return text
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_indicator_order(mod)
#'
#' plspm_indicator_factor(mod)
#' }
#'
plspm_indicator_order <- function(model_syntax) {
  ptable <- lavaan::lavParseModelString(model_syntax,
                                        as.data.frame. = TRUE)
  ptable2 <- ptable[ptable$op == "=~", ]
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }
  out <- ptable2$rhs
  names(out) <- ptable2$lhs
  out
}

#' plspm_indicator_factor
#'
#' @param model_syntax lavaan
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_indicator_order(mod)
#'
#' plspm_indicator_factor(mod)
#' }
#'
plspm_indicator_factor <- function(model_syntax) {
  ptable <- lavaan::lavParseModelString(model_syntax,
                                        as.data.frame. = TRUE)
  ptable2 <- ptable[ptable$op == "=~", ]
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }
  out <- ptable2$rhs
  names(out) <- ptable2$lhs
  res= names(out)
  res
}


#' plspm_ind_order2
#'
#' @param model_syntax lavaan
#'
#' @return input text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mod <-
#'   'f1 =~ x01 + x02 + x03 + x06
#'     f3 =~ x08 + x09 + x10 + x03
#'     f2 =~ x04 + x05 + x06 + x07
#'     f4 =~ x11 + x12 + x13 + x14
#'     f3 ~ f1 + f2
#'     f4 ~ f3
#'    '
#' plspm_ind_order2(mod)
#'
#' }
#'
plspm_ind_order2 <- function(model_syntax) {
  # Parse the model string into a parameter table
  ptable <- lavaan::lavParseModelString(model_syntax, as.data.frame. = TRUE)

  # Filter the table for latent variable definitions (op == "=~")
  ptable2 <- ptable[ptable$op == "=~", ]

  # Ensure there are no duplicate indicators
  ptable2 <- ptable2[!duplicated(ptable2$rhs), ]

  # Check if any factor loadings are found
  if (nrow(ptable2) == 0) {
    stop("No factor loadings found.")
  }

  # Extract indicators and latent variable names
  indicators <- ptable2$rhs
  latent_vars <- ptable2$lhs

  # Create a named vector in the order they appear in the model syntax
  unique_latent_vars <- unique(latent_vars)
  ordered_result <- unlist(lapply(unique_latent_vars, function(lv) {
    inds <- indicators[latent_vars == lv]
    setNames(inds, rep(lv, length(inds)))
  }))

  # Create the output string using cat
  cat('ind_order = c(',
      paste(paste0(names(ordered_result), ' = "', ordered_result, '"'), collapse = ', '),
      ')', sep = '\n')
}





#' plspm_indicator_order
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' plspm_indicator_order(nfl_pls_boot)
#' }
#'
plspm_boot_ind_order = function(plsres_boot){
  res = plsres_boot$model$gens$mvs_names%>%unique()
  res
}




#' plspm_indicator_factor
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' plspm_indicator_factor(nfl_pls_boot)
#' }
plspm_boot_ind_factor  = function(plsres_boot){
  res =plsres_boot$model$gens$lvs_names
  res
}




##
#' layout_mat_name
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' layout_mat_name(nfl_pls_boot)
#' }
layout_mat_name= function(plsres_boot){
  mres = unique(plsres_boot$model$gens$mvs_names)
  lres = plsres_boot$model$gens$lvs_names
  Res = c(mres, lres)
  Res
}





