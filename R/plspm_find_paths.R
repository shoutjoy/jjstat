#' Resolver knows the path
#'
#' @param data boot data
#' @param est Original
#' @param type es, all, est, ind, paths(default)
#' @return result text
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예제 데이터
#' satpls_boot <- list(boot = list(paths = data.frame(
#'   Original = c(0.578959128, 0.200724200, 0.275149576, 0.848344408, 0.105477650, -0.002753995, 0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'   Mean.Boot = c(0.57466537, 0.20926883, 0.28748175, 0.84907549, 0.09629728, -0.02304841, 0.68789406, 0.13300281, 0.59112195, 0.48611617),
#'   Std.Error = c(0.04941293, 0.05440409, 0.07095248, 0.02022514, 0.05731729, 0.06209752, 0.06286102, 0.08889983, 0.08591013, 0.07920673),
#'   perc.025 = c(0.46120153, 0.09494506, 0.14255079, 0.80126496, -0.02555946, -0.15182035, 0.57655200, -0.01836564, 0.41297885, 0.34267859),
#'   perc.975 = c(0.65528114, 0.30696127, 0.41537966, 0.88210331, 0.19678402, 0.09426639, 0.81986383, 0.32270662, 0.75274086, 0.63147051),
#'   row.names = c("IMAG -> EXPE", "IMAG -> SAT", "IMAG -> LOY", "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT", "QUAL -> VAL", "QUAL -> SAT", "VAL -> SAT", "SAT -> LOY")
#' )))
#'
#' # 함수 실행
#' find_paths_all(satpls_boot$boot$paths)
#' find_paths_all(satpls_boot$boot$paths,"all")
#' }
#'
find_paths <- function(data, type = "paths", est = "Original") {


  if (length(data) == 13) {
    data <- data$boot$paths
  } else {
    data <- data
  }

  # 경로를 추출하는 내부 함수
  extract_paths <- function(data) {
    # 행 이름을 'paths' 열로 변환
    data$paths <- rownames(data)
    rownames(data) <- NULL

    # 'paths' 열을 추출하여 데이터 프레임 생성
    paths_df <- data.frame(paths = data$paths, stringsAsFactors = FALSE)

    return(paths_df)
  }

  # 가능한 모든 경로 조합을 생성하는 내부 함수
  generate_all_paths <- function(paths_df) {
    # 각 경로를 구성 요소로 분할
    path_components <- strsplit(paths_df$paths, " -> ")

    # 경로에서 인접 목록 생성
    adj_list <- list()
    for (path in paths_df$paths) {
      nodes <- strsplit(path, " -> ")[[1]]
      for (i in 1:(length(nodes) - 1)) {
        if (!is.null(adj_list[[nodes[i]]])) {
          adj_list[[nodes[i]]] <- unique(c(adj_list[[nodes[i]]], nodes[i + 1]))
        } else {
          adj_list[[nodes[i]]] <- nodes[i + 1]
        }
      }
    }

    # Function to recursively generate all paths from a given node
    generate_paths <- function(node, adj_list, path) {
      if (!is.null(adj_list[[node]])) {
        for (next_node in adj_list[[node]]) {
          generate_paths(next_node, adj_list, c(path, next_node))
        }
      } else {
        all_paths <<- c(all_paths, paste(path, collapse = " -> "))
      }
    }

    # Generate all paths starting from "IMAG"
    all_paths <- c()
    generate_paths("IMAG", adj_list, "IMAG")

    # Convert all_paths to a data frame
    all_paths_df <- data.frame(paths = all_paths, stringsAsFactors = FALSE)

    return(all_paths_df)
  }

  # # 경로 추출
  paths_df <- extract_paths(data)

  # 가능한 모든 경로 조합 생성
  all_paths_df <- generate_all_paths(paths_df)

  # de_est 데이터 프레임 생성
  library(dplyr)
  de_est <- data %>%
    rownames_to_column(var = "paths") %>%
    select(paths, !!rlang::sym(est), Std.Error)

  # 결과를 리스트로 반환
  result <- list(de_est = de_est, ind = all_paths_df)
  est <- de_est
  ind <- all_paths_df
  paths_vector <- all_paths_df

  attr(result, "class") <- "plspm_paths"

  switch(type, res = result, all = result, est = est, ind = ind, paths = paths_vector)
}



#' Resolver knows the path
#'
#' @param data boot data
#' @param path_col paths
#' @param est Original
#' @param se Ste.Error
#' @param type es, all, est, ind, paths(default)
#'
#' @return paths types
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # Example usage
#' sdata <- data.frame(
#'   paths = c("IMAG -> EXPE", "IMAG -> SAT", "IMAG -> LOY", "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT",
#'             "QUAL -> VAL", "QUAL -> SAT", "VAL -> SAT", "SAT -> LOY"),
#'   Original = c(0.578959128, 0.200724200, 0.275149576, 0.848344408, 0.105477650, -0.002753995,
#'                0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'   Mean.Boot = c(0.58232765, 0.21209094, 0.27486564, 0.84793899, 0.10992051, -0.01008222,
#'                 0.67780527, 0.15132543, 0.56312717, 0.50757755),
#'   Std.Error = c(0.04358293, 0.06056988, 0.07729000, 0.01864573, 0.06968826, 0.06507903,
#'                 0.07760527, 0.08440236, 0.08723293, 0.07856694),
#'   perc.025 = c(0.497644719, 0.107561558, 0.140641502, 0.815794494, -0.025566471, -0.147788188,
#'                0.519959683, -0.008629516, 0.392664443, 0.351774130),
#'   perc.975 = c(0.6646198, 0.3275312, 0.4161131, 0.8828875, 0.2545785, 0.1086856,
#'                0.8166486, 0.2991045, 0.7012494, 0.6556333)
#' )
#'
#' #'
#' find_paths(sdata)
#' #'
#' find_paths(sdata)%>%find_paths_cal() #OK
#' find_paths_cal(find_paths(sdata)) #OK
#' find_paths_cal(sdata) #error

#' find_paths(satpls_boot)

#'
#' }
#'
#'
#'
find_paths1 <- function(data, type = "paths",
                       path_col = "paths",
                       est = "Original", se = "Std.Error") {


if (length(data) == 13) {
  data <- data$boot$paths %>% row2col("paths")
} else if (length(data) == 5) {
  data <- data %>% row2col("paths")
} else if (length(data) == 6) {
  data <- data
} else {
  data <- data
}

  # 데이터에 col1 추가
  data <- data %>%
    mutate(col1 = row_number())

  # 경로 컬럼의 이름을 path_col에 맞게 동적으로 설정
  paths_data <- data[[path_col]]
  paths_data1 <- data[, c(path_col, est, se)]

  # 시작 노드가 선행하지 않는 노드를 찾기
  all_starts <- unique(sapply(paths_data, function(x) strsplit(x, " -> ")[[1]][1]))
  all_ends <- unique(sapply(paths_data, function(x) strsplit(x, " -> ")[[1]][2]))
  start_nodes <- setdiff(all_starts, all_ends)

  # 경로 연결을 위한 재귀 함수 정의
  connect_paths <- function(paths, current_path) {
    # 현재 경로의 마지막 노드 추출
    last_node <- tail(strsplit(as.character(current_path), " -> ")[[1]], 1)

    # 현재 경로의 마지막 노드가 시작 노드인 모든 경로 찾기
    next_paths <- paths %>%
      filter(grepl(paste0("^", last_node, " -> "), .[[path_col]]))

    # 더 이상 연결할 경로가 없으면 현재 경로 반환
    if (nrow(next_paths) == 0) {
      return(current_path)
    }

    # 다음 경로를 재귀적으로 연결
    result <- c()
    for (i in 1:nrow(next_paths)) {
      new_path <- paste0(current_path, " -> ",
                         strsplit(as.character(next_paths[[i, path_col]]), " -> ")[[1]][2])
      result <- c(result, connect_paths(paths, new_path))
    }

    return(result)
  }

  # 선행하지 않는 시작 노드들에 대해 연결 작업 수행
  all_connected_paths <- c()
  for (start in start_nodes) {
    start_paths <- data %>% filter(grepl(paste0("^", start, " -> "), .[[path_col]]))
    start_paths <- as.data.frame(start_paths) # 명확히 데이터프레임으로 변환
    for (i in 1:nrow(start_paths)) {
      connected_paths <- connect_paths(data, as.character(start_paths[[i, path_col]]))
      all_connected_paths <- c(all_connected_paths, connected_paths)
    }
  }

  # 결과를 데이터 프레임으로 변환
  result <- data.frame(paths = unique(all_connected_paths), stringsAsFactors = FALSE)
  result <- result %>% filter(str_count(paths, " -> ") > 1)
  res <- list(de_est = paths_data1, ind = result)
  attr(res, "class") <- "plspm_paths"

  est <- paths_data1
  ind <- result
  paths_vector <- result$paths

  # 구분을 위한 클래스
  switch(type, res = res, all = res, est = est, ind = ind, paths = paths_vector)
}


#' real paths calculation est
#'
#' @param paste_path_result  find_paths data
#' @param digits round 3
#' @param est  Original  or Mean.Boot
#' @param type all=res, ind,  est
#'
#' @return data
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
#'
#' #'
# #모든 경우에 적용되도록 설정
# find_paths_cal( find_paths(satpls_boot,"all") )
# find_paths_cal(satpls_boot)
#
# find_paths(sdata,"all")%>%class()
#
# find_paths(sdata,"all")%>%find_paths_cal() #OK
# find_paths_cal(find_paths(sdata,"all")) #OK
#
# # find_paths_cal(sdata) #error
#
# find_paths_cal(find_paths(satpls_boot$boot$paths%>%row2col("paths"), "all") )
# find_paths_cal( find_paths(satpls_boot,"all") )
#'
#' ##realdata analysis
#' data(satisfaction)
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
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#' #' find_paths(satpls_boot)
#' find_paths_cal(find_paths(satpls_boot,"all") )
#'
#' #값을 계산하여 보기
#' find_paths(jutpls_boot,"all")%>%find_paths_cal()
#' #Mean.Boot자료 보기
#' find_paths(jutpls_boot,"all", est="Mean.Boot")%>%find_paths_cal()
#'
#'
#'
#' }
#'
#'
find_paths_cal <- function(paste_path_result, type = "all", digits = 3, est = "Original") {
  if (class(paste_path_result) == "plspm_paths") {
    paste_path_result1 <- paste_path_result
    de_est <- paste_path_result1$de_est
    ind_paths <- paste_path_result1$ind
    cat("plspm_paths\n")
  } else if (is.list(paste_path_result)) {
    paste_path_result1 <- find_paths(paste_path_result, type = "all", est = est)
    de_est <- paste_path_result1$de_est
    ind_paths <- paste_path_result1$ind
    cat("data.frame\n")
  } else if (class(paste_path_result) == "plspm" && length(paste_path_result) == 13) {
    paste_path_result1 <- find_paths(paste_path_result, type = "all", est = est)
    de_est <- paste_path_result1$de_est
    ind_paths <- paste_path_result1$ind
  } else {
    paste_path_result1 <- find_paths(paste_path_result, type = "all", est = est)
    de_est <- paste_path_result1$de_est
    ind_paths <- paste_path_result1$ind
    cat("list\n")
  }

  ind_paths <- ind_paths %>%
    rowwise() %>%
    mutate(
      ind_est = {
        path_elements <- strsplit(paths, " -> ")[[1]]
        partial_products <- sapply(1:(length(path_elements) - 1), function(i) {
          part_path <- paste(path_elements[i], path_elements[i + 1], sep = " -> ")
          de_est_val <- de_est[de_est$paths == part_path, 2]
          if (length(de_est_val) == 0) {
            return(NA) # 값을 찾지 못할 경우 NA 반환
          }
          return(de_est_val)
        })
        if (any(is.na(partial_products))) {
          return(NA) # NA가 포함된 경우 NA 반환
        }
        prod(partial_products, na.rm = TRUE)
      }
    )

  # 숫자형 열에 대해서만 round 적용
  ind_paths <- ind_paths %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))

  paste_path_result1$ind <- ind_paths
  paste_path_result1$de_est <- paste_path_result1$de_est %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))

  result <- switch(type,
                   all = paste_path_result1,
                   res = paste_path_result1,
                   ind = paste_path_result1$ind,
                   est = paste_path_result1$de_est)
  return(result)
}

      # cal = {
      #   # 경로를 " -> "로 분할하여 각 요소를 추출
      #   path_elements <- strsplit(paths, " -> ")[[1]]

      #   # 각 부분 경로에 대해 Original 값을 찾아 문자열로 저장
      #   partial_cal <- sapply(1:(length(path_elements) - 1), function(i) {
      #     # 부분 경로 생성
      #     part_path <- paste(path_elements[i], path_elements[i + 1], sep = " -> ")

      #     # # de_est에서 부분 경로에 해당하는 Original 값을 찾아 문자열로 반환
      #     # paste0(de_est$Original[de_est$paths == part_path])
      #      # de_est에서 부분 경로에 해당하는 Original 값을 찾아 문자열로 반환
      #     format(round(de_est$Original[de_est$paths == part_path], 3), justify="left")
      #   })

      #   # 모든 부분 경로의 Original 값을 문자열로 결합하여 cal로 반환
      #   paste(partial_cal, collapse = " * ")
      # }
