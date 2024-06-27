
#' lav_extract_path
#'
#' @param model model
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 모델 문자열
#' model1a <- "
#' EXPE ~ H1*IMAG
#' QUAL ~ H2*EXPE
#' VAL ~ H3*QUAL + H4*EXPE
#' SAT ~ H5*IMAG + H6*EXPE + H7*QUAL + H8*VAL
#' LOY ~ H9*SAT + H10*IMAG
#' "
#'
#' model1 <- "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~ QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#'
#' # 함수 호출
#' lav_extract_path(model1a)
#'
#' lav_extract_path(model1)

#'
#' }
#'
#'
lav_extract_path <- function(model) {
  # Split the model string into individual lines
  lines <- unlist(strsplit(model, "\n"))

  # Remove empty lines and trim whitespace
  lines <- lines[lines != ""]
  lines <- trimws(lines)

  # Filter lines that contain paths (i.e., contain ~)
  path_lines <- lines[grep("~", lines)]
  path_lines <- path_lines[!grepl("=~", path_lines)]

  # Initialize a list to store paths and a counter for hypothesis labels
  paths <- list()
  counter <- 1

  # Loop through each line and extract paths
  for (line in path_lines) {
    # Remove the variable on the left-hand side
    lhs_split <- unlist(strsplit(line, "~"))
    rhs <- lhs_split[2]

    # Extract individual paths from the right-hand side
    rhs_paths <- unlist(strsplit(rhs, "\\+"))
    rhs_paths <- trimws(rhs_paths)

    # Store each path and corresponding label in the list
    for (path in rhs_paths) {
      dependent <- trimws(lhs_split[1])
      independent <- ifelse(grepl("\\*", path),
                            trimws(unlist(strsplit(path, "\\*"))[2]), trimws(path))
      paths <- append(paths, list(c(paste0("H", counter),
                                    paste0(independent, "->", dependent))))
      counter <- counter + 1
    }
  }

  # Convert the list of paths to a data frame
  paths_df <- do.call(rbind, paths)
  colnames(paths_df) <- c("hypo", "paths")
  paths_df <- as.data.frame(paths_df)

  return(paths_df)
}



#' lav_extract_ind_paths, Create an indirect effect hypothesis
#'
#' @param paths_data direct path
#' @param first_node start node name
#' @param allpaths all and ind(FALSE)
#' @param paths_name TRUE hypothesis output
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' paths_data <- data.frame(
#'   hypo = c("H1", "H2", "H3", "H4", "H5",
#'   "H6", "H7", "H8", "H9", "H10"),
#'   paths = c("IMAG->EXPE", "EXPE->QUAL",
#'   "QUAL->VAL", "EXPE->VAL", "IMAG->SAT",
#'    "EXPE->SAT", "QUAL->SAT", "VAL->SAT", "SAT->LOY", "IMAG->LOY")
#' )
#' # 함수 실행
#' lav_extract_ind_paths(paths_data, allpaths = TRUE)
#' lav_extract_ind_paths(paths_data, allpaths = FALSE)
#' #'
#' lav_extract_ind_paths(paths_data, allpaths = FALSE, paths_name = TRUE)
#' lav_extract_ind_paths(paths_data, allpaths = FALSE, paths_name = FALSE)
#' #'
#' lav_extract_path(model1a)%>%lav_extract_ind_paths()
#'
#' lav_extract_path(model1)%>%lav_extract_ind_paths()
#' #'
#' lav_extract_path(model1a) %>%
#'   lav_extract_ind_paths() %>%
#'   sem_lav_ind_hypo()
#' #'
#' ## 구조모형에서 나타나는 간접효과 가설은 다음과 같다.
#' ## 간접효과[H1]: 간접효과 IMAG -> SAT ->
#' LOY 는 통계적으로 유의할 것이다.
#' ## 간접효과[H2]: 간접효과 IMAG -> EXPE -
#' > SAT -> LOY 는 통계적으로 유의할 것이다.
#' ## 간접효과[H3]: 간접효과 IMAG -> EXPE -
#' > VAL -> SAT -> LOY 는 통계적으로 유의할 것이다.
#' ## 간접효과[H4]: 간접효과 IMAG -> EXPE
#' -> QUAL -> SAT -> LOY 는 통계적으로 유의할 것이다.
#' ## 간접효과[H5]: 간접효과 IMAG -> EXPE
#' -> QUAL -> VAL -> SAT -> LOY 는 통계적으로 유의할 것이다.
#' #' #'
#' #'
#'
#' # Example usage with the provided model
#' model10 <- "
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
#' # Extract paths from the model
#' lav_extract_path(model10)
#' lav_extract_path(model10)%>%lav_extract_ind_paths()
#'
#' lav_extract_sm(model10)%>%lav_extract_path()%>%lav_extract_ind_paths()
#' lav_extract_sm(model10)%>%lav_extract_ind_paths()
#' lav_extract_sm(model10)%>%lav_extract_ind_paths("EXPE")
#'
#' }
#'
lav_extract_ind_paths <- function(paths_data,
                                  first_node= NULL,
                                  paths_name = FALSE,
                                  allpaths = FALSE) {
  if (!is.data.frame(paths_data)) {
    paths_data <- lav_extract_path(paths_data)
  }

  # 가능한 모든 경로 조합을 생성하는 내부 함수
  generate_all_paths <- function(paths_df, start_node = NULL) {
    # 각 경로를 구성 요소로 분할
    path_components <- strsplit(paths_df$paths, "->")

    # 경로에서 인접 목록 생성
    adj_list <- list()
    for (i in 1:nrow(paths_df)) {
      path <- paths_df$paths[i]
      nodes <- strsplit(path, "->")[[1]]
      for (j in 1:(length(nodes) - 1)) {
        node <- trimws(nodes[j])
        next_node <- trimws(nodes[j + 1])
        if (!is.null(adj_list[[node]])) {
          adj_list[[node]] <- unique(c(adj_list[[node]], next_node))
        } else {
          adj_list[[node]] <- next_node
        }
      }
    }

    # 모든 시작 노드를 찾기 위해 노드 목록 생성
    start_nodes <- if (is.null(start_node)) {
      unique(unlist(lapply(path_components, `[`, 1)))
    } else {
      c(start_node)
    }

    # 재귀적으로 모든 경로를 생성하는 함수
    generate_paths <- function(node, adj_list, path, path_indices) {
      if (!is.null(adj_list[[node]])) {
        for (next_node in adj_list[[node]]) {
          next_path_idx <- which(paths_df$paths == paste(node, next_node, sep = "->"))
          generate_paths(next_node, adj_list, c(path, next_node), c(path_indices, next_path_idx))
        }
      } else {
        if (length(path) > 2) {
          all_paths <<- c(all_paths, list(list(path = path, indices = path_indices)))
        }
      }
    }

    # 모든 시작 노드에서 경로를 생성
    all_paths <- list()
    for (start_node in start_nodes) {
      generate_paths(start_node, adj_list, start_node, numeric())
    }

    # 경로와 가설을 합치는 함수
    combine_path_hypothesis <- function(path_info) {
      path <- path_info$path
      indices <- path_info$indices
      path_with_hypothesis <- paste(path[1], paste(sapply(2:length(path), function(i) {
        paste0(paths_df$hypo[indices[i - 1]], "*", path[i])
      }), collapse = " -> "), sep = " -> ")
      return(path_with_hypothesis)
    }

    # 경로 이름을 포함할 경우 처리
    if (paths_name) {
      all_paths <- sapply(all_paths, combine_path_hypothesis)
    } else {
      all_paths <- sapply(all_paths, function(p) paste(p$path, collapse = " -> "))
    }

    # 중복된 H 제거
    if (paths_name) {
      all_paths <- sapply(all_paths, function(path) {
        gsub("H\\*(H\\d+\\*)", "\\1", path)
      })
    }

    all_paths_df <- data.frame(paths = all_paths, stringsAsFactors = FALSE)

    return(all_paths_df)
  }

  # #  첫 번째 경로의 모든 시작 노드 추출
  # if(auto){
  #   first_path_components <- strsplit(paths_data$paths[1], "->")[[1]]
  #   first_path_starts <- unique(trimws(first_path_components))
  # }


  # 입력 데이터를 사용하여 가능한 모든 경로를 생성
  if (allpaths) {
    all_paths <- generate_all_paths(paths_data)
  } else {
    all_paths <- generate_all_paths(paths_data, start_node = first_node)
  }

  # 최종 결과 필터링
  final_paths <- unique(all_paths)
  final_paths <- final_paths[order(nchar(final_paths$paths), final_paths$paths), ]

  return(final_paths)
}
