
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

  # Initialize a list to store paths and a counter for hypothesis labels
  paths <- list()
  counter <- 1

  # Loop through each line and extract paths
  for (line in lines) {
    # Remove the variable on the left-hand side
    lhs_split <- unlist(strsplit(line, "~"))
    rhs <- lhs_split[2]

    # Extract individual paths from the right-hand side
    rhs_paths <- unlist(strsplit(rhs, "\\+"))
    rhs_paths <- trimws(rhs_paths)

    # Store each path and corresponding label in the list
    for (path in rhs_paths) {
      dependent <- trimws(lhs_split[1])
      independent <- ifelse(grepl("\\*", path), trimws(unlist(strsplit(path, "\\*"))[2]), trimws(path))
      paths <- append(paths, list(c(paste0("H", counter), paste0(independent, "->", dependent))))
      counter <- counter + 1
    }
  }

  # Convert the list of paths to a data frame
  paths_df <- do.call(rbind, paths)
  colnames(paths_df) <- c("hypo", "paths")
  paths_df <- as.data.frame(paths_df)

  return(paths_df)
}


#' lav_extract_ind_paths
#'
#' @param paths_data direct path
#' @param allpaths all and ind(FALSE)
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
#'   hypo = c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10"),
#'   paths = c("IMAG->EXPE", "EXPE->QUAL", "QUAL->VAL", "EXPE->VAL", "IMAG->SAT", "EXPE->SAT", "QUAL->SAT", "VAL->SAT", "SAT->LOY", "IMAG->LOY")
#' )
#' # 함수 실행
#' lav_extract_ind_paths(paths_data, allpaths = TRUE)
#' lav_extract_ind_paths(paths_data, allpaths = FALSE)
#'
#'
#'
#' lav_extract_path(model1a)%>%lav_extract_ind_paths()
#'
#' lav_extract_path(model1)%>%lav_extract_ind_paths()
#' }
#'
lav_extract_ind_paths <- function(paths_data, allpaths = FALSE) {
  # 가능한 모든 경로 조합을 생성하는 내부 함수
  generate_all_paths <- function(paths_df, start_node = NULL) {
    # 각 경로를 구성 요소로 분할
    path_components <- strsplit(paths_df$paths, "->")

    # 경로에서 인접 목록 생성
    adj_list <- list()
    for (path in paths_df$paths) {
      nodes <- strsplit(path, "->")[[1]]
      for (i in 1:(length(nodes) - 1)) {
        node <- trimws(nodes[i])
        next_node <- trimws(nodes[i + 1])
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
    generate_paths <- function(node, adj_list, path) {
      if (!is.null(adj_list[[node]])) {
        for (next_node in adj_list[[node]]) {
          generate_paths(next_node, adj_list, c(path, next_node))
        }
      } else {
        all_paths <<- c(all_paths, paste(path, collapse = " -> "))
      }
    }

    # 모든 시작 노드에서 경로를 생성
    all_paths <- c()
    for (start_node in start_nodes) {
      generate_paths(start_node, adj_list, start_node)
    }

    # 직접 효과를 제외하고 all_paths를 데이터 프레임으로 변환
    all_paths <- all_paths[sapply(all_paths, function(x) length(strsplit(x, " -> ")[[1]]) > 2)]
    all_paths_df <- data.frame(paths = all_paths, stringsAsFactors = FALSE)

    return(all_paths_df)
  }

  # 첫 번째 경로의 시작 노드 추출
  first_path_start <- trimws(strsplit(paths_data$paths[1], "->")[[1]][1])

  # 입력 데이터를 사용하여 가능한 모든 경로를 생성
  if (allpaths) {
    all_paths <- generate_all_paths(paths_data)
  } else {
    all_paths <- generate_all_paths(paths_data, start_node = first_path_start)
  }

  # 최종 결과 필터링
  final_paths <- unique(all_paths)
  final_paths <- final_paths[order(nchar(final_paths$paths), final_paths$paths), ]

  return(final_paths)
}
