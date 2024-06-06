#' Extract only the corresponding paths from the data with the given material and the given paths
#'
#' @param data boot
#' @param path_string path IMAG -> EXPE -> VAL

#'
#' @return paths
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#'
#' # Example usage
#' sdata <- data.frame(
#'   paths = c("IMAG -> EXPE", "IMAG -> SAT", "IMAG -> LOY",
#'    "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT",
#'             "QUAL -> VAL", "QUAL -> SAT",
#'             "VAL -> SAT", "SAT -> LOY"),
#'   Original = c(0.578959128, 0.200724200, 0.275149576,
#'    0.848344408, 0.105477650, -0.002753995,
#'                0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'   Mean.Boot = c(0.58232765, 0.21209094, 0.27486564,
#'   0.84793899, 0.10992051, -0.01008222,
#'                 0.67780527, 0.15132543, 0.56312717, 0.50757755),
#'   Std.Error = c(0.04358293, 0.06056988, 0.07729000,
#'   0.01864573, 0.06968826, 0.06507903,
#'                 0.07760527, 0.08440236, 0.08723293, 0.07856694),
#'   perc.025 = c(0.497644719, 0.107561558, 0.140641502,
#'    0.815794494, -0.025566471, -0.147788188,
#'                0.519959683, -0.008629516, 0.392664443, 0.351774130),
#'   perc.975 = c(0.6646198, 0.3275312, 0.4161131,
#'    0.8828875, 0.2545785, 0.1086856,
#'                0.8166486, 0.2991045, 0.7012494, 0.6556333)
#' )
#' collapse_path(sdata, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")
#' ## paths  Original  Std.Error
#' ## 1 IMAG -> EXPE 0.5789591 0.04358293
#' ## 2  EXPE -> VAL 0.1054776 0.06968826
#' ## 3   VAL -> SAT 0.5893307 0.08723293
#' ## 4   SAT -> LOY 0.4954793 0.07856694
#'
#'
#' #활용방법
#' collapse_path(sdata,
#' path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")%>%
#' find_paths()%>%
#' find_paths_cal()
#'
#' #' }
#'

collapse_path <- function(data, path_string) {
  if(length(data) == 13 && is.list(data)){
    data <- data$boot$paths %>%row2col("paths")
  } else {
    data <- data
  }

  # Split the input string into individual components
  components <- unlist(strsplit(path_string, " -> "))

  # Remove duplicates while preserving order
  unique_components <- unique(components)

  # Create pairs of consecutive components
  pairs <- sapply(seq_along(unique_components[-length(unique_components)]), function(i) {
    paste(unique_components[i], unique_components[i + 1], sep = " -> ")
  })

  # Ensure 'paths' column exists in data
  if (!"paths" %in% colnames(data)) {
    stop("The data does not contain a 'paths' column.")
  }

  # Extract matching rows from data
  extracted_data <- data %>% filter(paths %in% pairs)

  # Create coefficients and se_values vectors
  coefficients <- setNames(extracted_data$Original, extracted_data$paths)
  se_values <- setNames(extracted_data$Std.Error, paste0("se", 1:nrow(extracted_data)))

  # Combine into a single data frame
  result_df <- data.frame(
    paths = extracted_data$paths,
    Original = extracted_data$Original,
    Std.Error = extracted_data$Std.Error
  )
}


#' 소벡테스트에 입력될 값으로 산정하여 list로 출력
#'
#' @param data boot
#' @param path_string paths
#' @param show message
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # Example usage
#' sdata <- data.frame(
#'   paths = c("IMAG -> EXPE", "IMAG -> SAT", "IMAG -> LOY",
#'    "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT",
#'             "QUAL -> VAL", "QUAL -> SAT",
#'             "VAL -> SAT", "SAT -> LOY"),
#'   Original = c(0.578959128, 0.200724200, 0.275149576,
#'    0.848344408, 0.105477650, -0.002753995,
#'                0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'   Mean.Boot = c(0.58232765, 0.21209094, 0.27486564,
#'   0.84793899, 0.10992051, -0.01008222,
#'                 0.67780527, 0.15132543, 0.56312717, 0.50757755),
#'   Std.Error = c(0.04358293, 0.06056988, 0.07729000,
#'   0.01864573, 0.06968826, 0.06507903,
#'                 0.07760527, 0.08440236, 0.08723293, 0.07856694),
#'   perc.025 = c(0.497644719, 0.107561558, 0.140641502,
#'    0.815794494, -0.025566471, -0.147788188,
#'                0.519959683, -0.008629516, 0.392664443, 0.351774130),
#'   perc.975 = c(0.6646198, 0.3275312, 0.4161131,
#'    0.8828875, 0.2545785, 0.1086856,
#'                0.8166486, 0.2991045, 0.7012494, 0.6556333)
#' )
#' #check 1
#' collapse_path2(sdata, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")
#'
#' #check 2
#' collapse_path2(sdata, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY", show=TRUE)
#' #'
#' # TEST
#'
#' collapse_path2(sdata, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")
#' collapse_path2(sdata, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY", show=TRUE)
#' collapse_path2(satpls_boot, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY", show=TRUE)
#'
#' # 소벨테스트
#' sobel_test_extend(coefficients= c("IMAG -> EXPE" = 0.578959128,
#'                                   "EXPE -> SAT" = -0.002753995,
#'                                   "SAT -> LOY" = 0.495479322),
#'                   se_values = c(se1 = 0.04358293,
#'                                 se2 = 0.06507903,
#'                                 se3 = 0.0785669))
#'
#'
#' Resdata  =   collapse_path2(sdata, path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")
#'
#' sobel_test_extend(coefficients= Resdata$coefficients, Resdata$se_values)
#'
#'
#' sobel_test_extend(
#'   coefficients= collapse_path2(sdata,
#'               path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")$coefficients,
#'   se_values =  collapse_path2(sdata,
#'         path_string= "IMAG -> EXPE -> VAL -> SAT -> LOY")$se_values)
#'
#'
#' sobel_test_extend(coefficients= collapse_path2(sdata,
#'          path_string=  "IMAG -> EXPE -> SAT -> LOY")$coefficients,
#'          se_values =  collapse_path2(sdata, path_string= "IMAG -> EXPE -> SAT -> LOY")$
#'                     se_values, show=FALSE)
#'
#' }
#'
#'
collapse_path2 <- function(data, path_string, show=FALSE) {

  if(length(data) == 13 && is.list(data)){
    data <- data$boot$paths %>%tibble::rownames_to_column("paths")
  } else {
    data <- data
  }

  if(is.list(data) && "boot" %in% names(data)){
    data <- data$boot$paths %>% tibble::rownames_to_column("paths")
  } else {
    data <- as.data.frame(data)
  }
  # Split the input string into individual components
  components <- unlist(strsplit(path_string, " -> "))

  # Remove duplicates while preserving order
  unique_components <- unique(components)

  # Create pairs of consecutive components
  pairs <- sapply(seq_along(unique_components[-length(unique_components)]), function(i) {
    paste(unique_components[i], unique_components[i + 1], sep = " -> ")
  })

  # Extract matching rows from data
  extracted_data <- data %>% dplyr::filter(paths %in% pairs)

  # Create coefficients and se_values vectors
  coefficients <- setNames(extracted_data$Original, extracted_data$paths)
  se_values <- setNames(extracted_data$Std.Error, paste0("se", 1:nrow(extracted_data)))


  if(show){

    # Print the results
    cat("\n")
    cat("coefficients = c(\n", paste(paste0("    \"", names(coefficients), "\" = ", coefficients), collapse = ",\n "), ")\n")
    cat("\n")
    cat("se_values = c(\n", paste(paste0("     ",names(se_values), " = ", se_values), collapse = ",\n "), ")\n\n")

  }

  res =  list(coefficients=coefficients,se_values=se_values )
  res
}
