
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
#' #' #'
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY"))
#' )
#' sat_path
#'
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#'
#' sat_blocks1
#'
#' satpls = plspm(satisfaction,
#'                path_matrix = sat_path,
#'                blocks = sat_blocks1,
#'                scaled = FALSE)
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
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

  #
  if(length(plsres)==13){
    path_coefs_data = plsres$path_coefs
    inner_summary_data = plsres$inner_summary
  }else{
    pass
  }

  #
  # path_coefs_data = plsres[["path_coefs"]]
  # inner_summary_data = plsres[["inner_summary"]]

  coeff1 =  path_coefs_data %>%
    t()%>%   #행렬을 전환
    long_df("from","to","coefs")%>% #rownames_to_column and  pivot_longer
    dplyr::filter(coefs != 0)

  r2_part = inner_summary_data %>%
    row2col("to")%>%#rownames_to_column
    dplyr::select(to, R2)%>%
    dplyr::filter(R2 !=0)

  res = full_join(coeff1,  r2_part, by="to")%>%
    dplyr::mutate(Rred = R2-(R2*coefs),
           diff = R2-Rred,
           exR2 = 1-R2,
           f2 = diff/exR2)%>%
    dplyr::mutate(effectsize = case_when(
      f2 >= 0.35 ~ "large(>.35)",
      f2 >= 0.15 ~ "medium(>.15)",
      f2 >= 0.02 ~ "small(>.02)",
      f2 < 0.2  ~ "ns"
    ))%>%
    dplyr::select(from, to, coefs, R2, f2, effectsize)


  f2 = res%>%dplyr::select(from,to, f2)%>%
           Unite(1,2,"paths","->")#%>%col2row()

  switch(type, res = res,
         f2 = f2,
         r2 =inner_summary_data[,c(1,2)],
         R2 = inner_summary_data[, c(1, 2)],
         inner =inner_summary_data)
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
