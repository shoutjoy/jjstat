#' plspm ind efffect bootstrap
#'
#' @param plsres plspm result boot
#' @param ... paths
#' @param data data
#' @param path_matrix  paths
#' @param blocks blocks
#' @param modes models
#' @param B b= 100
#'
#' @return resutl paths
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #'
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
#' # vector of modes (reflective indicators):auto
#' sat_mod = rep("A", 6)
#'
#' # satpls = plspm(satisfaction, sat_path, sat_blocks1, scaled = FALSE)
#' # satpls
#' # satpls %>%summary()
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#'
#'
#' long_paths <- c(
#'   "IMAG -> EXPE -> QUAL -> VAL -> SAT -> LOY",
#'   "IMAG -> EXPE -> QUAL -> SAT -> LOY",
#'   "IMAG -> EXPE -> VAL -> SAT -> LOY",
#'   "IMAG -> EXPE -> SAT -> LOY",
#'   "IMAG -> SAT -> LOY"
#' )
#'
#' result <- plspm_get_boot(satpls_boot, long_paths, B = 100)
#' result
#' result %>% Round(3)
#' result %>% unite_ci()
#'
#' resultall <- plspm_get_boot(satpls_boot, find_paths(satpls_boot), B = 100)
#' resultall
#'
#'
#' }
#'
#'
plspm_ind_effect_boot <- function(plsres = NULL,
                           ..., #paths
                           data = NULL,
                           path_matrix = NULL,
                           blocks = NULL,
                           modes = NULL,
                           B = 100) {
  if (!is.null(plsres)) {
    data <- plsres$data
    path_matrix <- plsres$model$IDM
    blocks <- plspm_extract_blocks(plsres$model)
    modes <- plsres$model$specs$modes
  }

  # 여러 경로를 받아 처리
  paths_input <- unlist(list(...))

  # 경로가 하나의 문자열로 합쳐진 경우와 개별적으로 전달된 경우 처리
  if (length(paths_input) == 1 && grepl(",", paths_input[[1]])) {
    ind_paths <- unlist(strsplit(paths_input[[1]], ", "))
  } else if(length(paths_input) != 1 ) {
    ind_paths <- unlist(paths_input)
  }

  # 경로를 분해
  paths_list <- ind_paths
  indirect_effects_list <- vector("list", length(paths_list))
  names(indirect_effects_list) <- paths_list

  results <- data.frame()

  # 진행 표시줄 설정
  pb <- progress_bar$new(
    format = " Bootstraping progress [:bar] :percent in :elapsed",
    total = length(paths_list) * B, clear = FALSE, width = 60
  )

  for (path_idx in 1:length(paths_list)) {
    paths <- strsplit(paths_list[path_idx], "->")[[1]]
    num_paths <- length(paths) - 1

    if (num_paths < 2) stop("간접 경로는 최소 2개의 경로를 포함해야 합니다.")

    indirect_effects <- numeric(B)

    for (i in 1:B) {
      boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
      plspm_result <- plspm(boot_sample, path_matrix, blocks, modes)

      indirect_effect <- 1
      for (j in 1:num_paths) {
        start <- trimws(paths[j])
        end <- trimws(paths[j + 1])
        if (!(end %in% rownames(plspm_result$path_coefs)) || !(start %in% colnames(plspm_result$path_coefs))) {
          stop(paste("경로", start, "->", end, "는 유효하지 않습니다."))
        }
        path_coef <- plspm_result$path_coefs[end, start]
        indirect_effect <- indirect_effect * path_coef
      }

      indirect_effects[i] <- indirect_effect

      pb$tick()  # 진행 표시줄 업데이트
    }

    mean_indirect_effect <- mean(indirect_effects)
    se_indirect_effect <- sd(indirect_effects)
    ci_lower <- quantile(indirect_effects, 0.025)
    ci_upper <- quantile(indirect_effects, 0.975)

    result <- data.frame(
      paths = paths_list[path_idx],
      Mean.Boot = mean_indirect_effect ,
      Std.Error = se_indirect_effect,
      ci_0.025 = ci_lower,
      ci_0.975 = ci_upper
    )
    results <- bind_rows(results, result)
  }

  indirect_effects = indirect_effects
  rownames(results) <- NULL  # 행 이름을 제거하여 깔끔한 출력
  return( as_tibble(results ))

}
