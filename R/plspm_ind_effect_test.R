#' Test for all indirect effects
#'
#' @param bootdataboot data
#' @param ... paths
#' @param show show paths details
#' @param est  Original or Mean.Boot
#' @param sobel aroian, sobel, goodman
#'
#' @return data result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #'
#' ple usage:
#'   satpls_boot <- list(boot = list(paths = data.frame(
#'     Original = c(0.578959128, 0.200724200, 0.275149576, 0.848344408,
#'                  0.105477650, -0.002753995, 0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'     Mean.Boot = c(0.58232765, 0.21209094, 0.27486564, 0.84793899,
#'                   0.10992051, -0.01008222, 0.67780527, 0.15132543, 0.56312717, 0.50757755),
#'     Std.Error = c(0.04358293, 0.06056988, 0.07729000, 0.01864573,
#'                   0.06968826, 0.06507903, 0.07760527, 0.08440236, 0.08723293, 0.07856694),
#'     perc.025 = c(0.497644719, 0.107561558, 0.140641502, 0.815794494,
#'                  -0.025566471, -0.147788188, 0.519959683, -0.008629516, 0.392664443, 0.351774130),
#'     perc.975 = c(0.6646198, 0.3275312, 0.4161131, 0.8828875,
#'                  0.2545785, 0.1086856, 0.8166486, 0.2991045, 0.7012494, 0.6556333),
#'     row.names = c("IMAG -> EXPE", "IMAG -> SAT", "IMAG -> LOY",
#'                   "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT",
#'                   "QUAL -> VAL", "QUAL -> SAT", "VAL -> SAT", "SAT -> LOY")
#'   )))
#'   # bootdata$boot$paths
#'
#'
#'
#'
#'   plspm_ind_effect_test(satpls_boot,
#'                       "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                       "IMAG -> SAT -> LOY")
#'
#'   plspm_ind_effect_test(satpls_boot$boot$paths,
#'                         "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                         "IMAG -> SAT -> LOY")
#'
#'   plspm_ind_effect_test(satpls_boot,
#'                       "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                       "IMAG -> SAT -> LOY", est = "Mean.Boot")
#'
#'   plspm_ind_effect_test(satpls_boot$boot$paths,
#'                         "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                         "IMAG -> SAT -> LOY", est = "Mean.Boot")
#'   plspm_ind_effect_test(satpls_boot$boot$paths,
#'                         "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                         "IMAG -> SAT -> LOY",
#'                         est = "Mean.Boot",
#'                         sobel="sobel")
#'
#'   plspm_ind_effect_test(satpls_boot$boot$paths,
#'                         "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                         "IMAG -> SAT -> LOY",
#'                         est = "Mean.Boot",
#'                         sobel="goodman")
#'
#'
#'   # For individual path strings
#'   plspm_ind_effect_test(satpls_boot)
#'   plspm_ind_effect_test(satpls_boot, show=TRUE)
#'   plspm_ind_effect_test(satpls_boot, show=FALSE)
#'
#'   plspm_ind_effect_test(satpls_boot,
#'                       "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'                       "IMAG -> SAT -> LOY")
#'
#'   # For a list of paths from find_paths(satpls_boot)$paths
#'   plspm_ind_effect_test(satpls_boot, find_paths(satpls_boot))
#'
#'   plspm_ind_effect_test(satpls_boot, "IMAG -> EXPE -> SAT -> LOY")
#'   plspm_ind_effect_test(satpls_boot,  "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'    "IMAG -> SAT -> LOY")
#'
#'
#'   find_paths(satpls_boot, type= "ind")
#'   #default
#'   find_paths(satpls_boot, type= "paths")
#'
#'   RRR = find_paths(bootdata, type="paths")
#'   RRR[1]
#'   RRR[2]
#'   RRR[3]
#'   RRR[4]
#'   RRR[5]
#'   RRR[6]
#'
#'   plspm_ind_effect_test(satpls_boot,  RRR[1],RRR[2],RRR[3],RRR[4],RRR[5] )
#'
#'   plspm_ind_effect_test(satpls_boot, find_paths(satpls_boot,"ind")%>%unlist())
#'
#'
#'
#'
#' }
#'
plspm_ind_effect_test <- function(bootdata, ...,
                                  show=TRUE,
                                  est="Mean.Boot",
                                  sobel="aroian") {

  # data <- convert_plspm_to_list(data)
  if (is.list(bootdata) && "boot" %in% names(bootdata)) {
    ana_data <- bootdata$boot$paths %>% tibble::rownames_to_column("paths")
  } else {
    ana_data <- as.data.frame(bootdata) %>% tibble::rownames_to_column("paths")
  }

  # paths_input is null -> calculation All paths
  paths_input <- list(...)


  # Check if paths_input is NULL or empty
  if (is.null(paths_input) || length(paths_input) == 0) {
    paths_input = list(find_paths(bootdata, type ="paths" ) )
  }

  # Flatten the input list to handle both individual path strings and a list of paths
  paths_list <- unlist(paths_input)

  results_list <- list()

  for (path_string in paths_list) {
    collapsed_data <- collapse_path2(ana_data, path_string, est = est) #select est

    sobel_result <- sobel_test_extend(coefficients = collapsed_data$coefficients,
                                      se_values = collapsed_data$se_values,
                                      show = show, sobel=sobel)
    results_list[[path_string]] <- sobel_result
  }

  # final_results <- dplyr::bind_rows(results_list, .id = "path")
  final_results <- dplyr::bind_rows(results_list)%>%
    dplyr::rename(SE=sobel_se, Z = z_value, p.value=p_value)#%>%
  # dplyr::select(paths, ind_effect, SE, Z, p.value,sig)

  cat("\n Estimate coefficients : ",est,", \n",
      "indirect effect significance method:",sobel,"\n")

  return(final_results)
}
