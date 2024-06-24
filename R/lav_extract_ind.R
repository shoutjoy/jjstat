#' lav_extract_ind, indirect effect
#'
#' @param model lavaan
#' @param start_node path start node
#' @param end_node  path end node
#' @param prefix hypothesis "a" if U want change "H"
#' @param cat test show cat TRUR
#' @param type type "ind","new_parameters","combine"
#' @param de when direct paht input
#' @param auto TRUE path auto, FLASE all ind path
#' @param paths_name TRUE change paths name
#'
#' @return text
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#'
#' # Example usage
#' model1 <- "
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#'
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#'
#' # Extracting paths from the model with default start_node = NULL and end_node = NULL
#' lav_extract_mm(model1) %>%cat("\n")
#' lav_extract_sm(model1) %>%cat("\n")
#'
#' lav_extract_ind(model1)%>%cat("\n")
#' lav_extract_ind(model1, auto=TRUE)%>%cat("\n")
#'
#' lav_extract_ind(model1,start_node="EXPE", auto=TRUE)%>%cat("\n")
#' lav_extract_ind(model1,start_node="EXPE")%>%cat("\n")
#' lav_extract_ind(model1,start_node="QUAL")%>%cat("\n")
#' lav_extract_ind(model1,start_node="IMAG", auto=TRUE)%>%cat("\n")
#'
#' lav_extract_ind(lav_extract_sm(model1), de=TRUE) %>%cat("\n")
#'
#' lav_extract_ind(lav_extract_sm(model1), de=TRUE, auto=TRUE)%>%cat("\n")
#' lav_extract_ind(lav_extract_sm(model1), start_node="IMAG", de=TRUE) %>%cat('\n')
#'
#' lav_extract_ind(lav_extract_sm(model1),"IMAG", prefix="H", de=TRUE) %>%cat('\n')
#'
#' # Example usage
#' de_paths <- "
#' EXPE  ~ a1* IMAG
#' QUAL  ~ a2* EXPE
#' VAL  ~ a3* QUAL
#' VAL  ~ a4* EXPE
#' SAT  ~ a5* IMAG
#' SAT  ~ a6* EXPE
#' SAT  ~ a7* QUAL
#' SAT  ~ a8* VAL
#' LOY  ~ a9* SAT
#' LOY  ~ a10* IMAG
#' "
#'
#' lav_extract_ind(de_paths)
#' #출력
#' #  lav_extract_ind(model1, cat=TRUE)
#'
#' lav_extract_ind(de_paths, cat=TRUE)
#'
#' lav_extract_ind(de_paths)%>%make_c_text()
#' }
#'
lav_extract_ind <- function(model, start_node = NULL, end_node = NULL,
                            prefix="a", cat=FALSE, type="ind",
                            de=FALSE, auto=TRUE,
                            paths_name=FALSE) {
  # Extract the model paths
  if(de){
    de_paths <- model
  } else {
    de_paths <- lav_extract_sm(model, prefix=prefix)
  }

  # Split the de_paths string by newline to separate each line
  lines <- strsplit(de_paths, "\n")[[1]]
  lines <- trimws(lines)  # Trim any leading or trailing whitespace

  # Initialize lists to store paths and coefficients
  path_list <- list()
  coef_list <- list()
  all_nodes <- c()
  direct_path_included <- FALSE

  for (line in lines) {
    if (nchar(line) > 0) {
      # Split the line by '~' and extract dependent and independent variables with coefficient
      parts <- strsplit(line, "~")[[1]]
      dependent <- trimws(parts[1])
      independent_part <- trimws(parts[2])

      # Split the independent part by '*' to separate coefficient and variable
      coef_var <- strsplit(independent_part, "\\*")[[1]]
      coef <- trimws(coef_var[1])
      independent <- trimws(coef_var[2])

      # Append to path and coefficient lists
      path_list <- append(path_list, list(c(independent, dependent)))
      coef_list <- append(coef_list, list(coef))

      # Collect all unique nodes
      all_nodes <- unique(c(all_nodes, independent, dependent))
    }
  }

  # Automatically set start_node if auto is TRUE and start_node is NULL
  if (is.null(start_node) && auto) {
    start_node <- path_list[[1]][1]
  }

  # Determine the end nodes (those that are not independent variables)
  if (is.null(end_node)) {
    end_nodes <- setdiff(all_nodes, sapply(path_list, `[[`, 1))
  } else {
    end_nodes <- end_node
  }

  # Create a data frame to store the final paths and their coefficients
  paths <- character()
  ind_coefs <- character()
  IEs <- character()

  find_paths <- function(current_path, current_coefs) {
    last_node <- tail(current_path, 1)
    if (last_node %in% end_nodes) {
      if (length(current_path) > 2) { # Exclude direct paths
        path <- paste(current_path, collapse = " -> ")
        paths <<- c(paths, path)
        ind_coefs <<- c(ind_coefs, paste(current_coefs, collapse = "*"))
        path_name <- paste(substr(unlist(strsplit(path, " -> ")), 1, 1), collapse = "")
        IEs <<- c(IEs, paste0("IE_", sprintf("%02d", length(IEs) + 1), if (paths_name) paste0("_", path_name) else ""))
      } else {
        direct_path_included <<- TRUE
      }
    } else {
      for (i in seq_along(path_list)) {
        if (path_list[[i]][1] == last_node) {
          find_paths(c(current_path, path_list[[i]][2]), c(current_coefs, coef_list[[i]]))
        }
      }
    }
  }

  # Start finding paths from each independent variable leading to end nodes
  if (is.null(start_node)) {
    start_nodes <- setdiff(sapply(path_list, `[[`, 1), end_nodes)
  } else {
    start_nodes <- start_node
  }

  for (start_node in start_nodes) {
    for (i in seq_along(path_list)) {
      if (path_list[[i]][1] == start_node) {
        find_paths(path_list[[i]], coef_list[[i]])
      }
    }
  }

  # Create the final data frame
  result <- data.frame(paths = paths, ind_coefs = ind_coefs, IEs = IEs)

  if (direct_path_included) {
    cat("The direct path is included.\n")
  }

  result <- result %>%
    mutate(syn = paste0(IEs, " := ", ind_coefs))

  # Combine original paths with new parameters
  original_paths <- paste(lines, collapse = "\n")
  if (paths_name) {
    new_parameters <- paste(result$syn, collapse = "\n")
  } else {
    new_parameters <- paste(result$syn, "     #", result$paths, collapse = "\n")
  }
  combined_output <- paste(original_paths,
                           "\n\n# NEW PARAMETER Indirect effect \n", new_parameters)

  if(cat) {
    cat(paste("\n", combined_output, "\n\n"))
  }

  switch(type,
         ind = new_parameters,
         new_parameters = new_parameters,
         combine = combined_output)
}
