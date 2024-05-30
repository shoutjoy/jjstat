
#' Effect Size Black
#'
#' @param plsres pls result
#' @param type res, f2, R2, ceofs, inner
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #'
#' # Example data for plsres
#' plsres <- list(
#'   path_coefs = data.frame(
#'     자기효능감 = c(0, 0.884, 0.681, 0.373),
#'     진로동기 = c(0, 0, 0, 0.353),
#'     진로태도 = c(0, 0, 0, 0.188),
#'     진로준비 = c(0, 0, 0, 0),
#'     row.names = c("자기효능감", "진로동기", "진로태도", "진로준비")
#'   ),
#'   inner_summary = data.frame(
#'     R2 = c(0.0000000, 0.7817016, 0.4631848, 0.7195506),
#'     row.names = c("자기효능감", "진로동기", "진로태도", "진로준비")
#'   )
#' )
#' plspm_f2(plsres, type="res")
#' plspm_f2(plsres, type="f2")
#' # etc
#' satpls_boot%>%plspm_f2()
#' satpls_boot%>%plspm_f2("f2")
#' satpls_boot%>%plspm_f2("coefs")
#' satpls_boot%>%plspm_f2("r2")
#' satpls_boot%>%plspm_f2("inner")
#' }
#'
#'
#'
plspm_f2 = function(plsres, type="res"){

  path_coefs_data = plsres[["path_coefs"]]
  inner_summary_data = plsres[["inner_summary"]]

  res = full_join(path_coefs_data %>%
                    t()%>%   #행렬을 전환
                    long_df("from","to","coefs")%>% #rownames_to_column and  pivot_longer
                    filter(coefs !=0), # coefs = 0 , remove
                  inner_summary_data %>%
                    row2col("to")%>%#rownames_to_column
                    dplyr::select(to, R2)%>%
                    filter(R2 !=0),  #R2=0 remove
                  by="to")%>%
    mutate(Rred = R2-(R2*coefs),
           diff = R2-Rred,
           exR2 = 1-R2,
           f2 = diff/exR2)%>%
    mutate(effectsize = case_when(
      f2 >= 0.35 ~ "large",
      f2 >= 0.15 ~ "medium",
      f2 >= 0.02 ~ "small",
      f2 < 0.2  ~ "ns"
    ))%>%
    dplyr::select(from, to, coefs, R2, f2, effectsize)


  f2 = res%>%dplyr::select(from,to, f2)%>%Unite(1,2,"paths","->")#%>%col2row()

  switch(type, res = res,
         f2 = f2,
         coefs =path_coefs_data,
         r2 =inner_summary_data[,c(1,2)],
         R2 = inner_summary_data[, c(1, 2)],
         inner =inner_summary_data,
  )
}
#
# plspm_f2 <- function(plsres, type="res") {
#
#   path_coefs_data <- plsres[["path_coefs"]]
#   inner_summary_data <- plsres[["inner_summary"]]
#
#   # Calculate f2 effect sizes
#   res <- full_join(
#     path_coefs_data %>%
#       t() %>%  # Transpose the matrix
#       as.data.frame() %>%
#       rownames_to_column("from") %>%
#       pivot_longer(cols = -from, names_to = "to", values_to = "coefs") %>%
#       filter(coefs != 0),  # Remove zero coefficients
#     inner_summary_data %>%
#       rownames_to_column("to") %>%
#       dplyr::select(to, R2) %>%
#       filter(R2 != 0),  # Remove zero R2 values
#     by = "to"
#   ) %>%
#     mutate(
#       Rred = R2 - (R2 * coefs),
#       diff = R2 - Rred,
#       exR2 = 1 - R2,
#       f2 = ifelse((diff / exR2) < 0, 0, diff / exR2) # Handle negative f2 values
#     ) %>%
#     mutate(effectsize = case_when(
#       f2 >= 0.35 ~ "large",
#       f2 >= 0.15 ~ "medium",
#       f2 >= 0.02 ~ "small",
#       f2 < 0.02 ~ "ns"
#     )) %>%
#     dplyr::select(from, to, coefs, R2, f2, effectsize)
#
#   # Create a data frame with paths and f2 values
#   f2 <- res %>%
#     dplyr::select(from, to, f2) %>%
#     unite("paths", from, to, sep = "->")
#
#   # Return the appropriate result based on the type argument
#   switch(type,
#          res = res,
#          f2 = f2,
#          coefs = path_coefs_data,
#          r2 = inner_summary_data[, c(1, 2)],
#          R2 = inner_summary_data[, c(1, 2)],
#          inner = inner_summary_data
#   )
# }
