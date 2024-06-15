#' add_pca_scores
#'
#' @param data data
#' @param ... select variable
#' @param method niplas, prcomp
#' @param adjust  Options to change the sign of prcomp c(F, T ...)
#'
#' @return  add data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' data(offense)
#'
#' nfl_path = matrix(
#'   c(0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 1, 1, 0, 0,
#'     1, 0, 0, 1, 0),
#'   nrow = 5, ncol = 5, byrow = TRUE )
#'
#' rownames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' colnames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' nfl_path
#' Offense = offense %>% add_pca_scores(rush1 = c(1:3), pass1 = c(4:6))
#'
#'Offense =  add_pca_scores(offense,
#'                rush1 = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'                pass1 = c("YardsPassComp", "PassYards", "PassFirstDown"))
#'
#'
#' nfl_blocks2 = list(7:8, 1:3, 4:6,  18:19, 9:11)
#'
#' plspm_blocks_match(data=Offense, blocks=nfl_blocks2,
#'                    paths=nfl_path)
#'
#' plspm_blocks_match(data=Offense, blocks=nfl_blocks2,
#'                    paths=nfl_path)%>%make_list_text()
#'
#' nfl_blocks2 = list(
#'   Speical = c("FieldGoals", "OtherTDs"),
#'   Rushing = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'   Passing = c("YardsPassComp", "PassYards", "PassFirstDown"),
#'   Offense = c("rush1", "pass1"),
#'   Scoring = c("PointsGame", "OffensTD", "TDGame")
#' )
#' nfl_blocks2
#' nfl_mode2 =c("B","A","A","A","A")
#' nfl_pls2 = plspm(Data=Offense, nfl_path, nfl_blocks2, modes = nfl_mode2)
#' nfl_pls2%>%summary()
#'
#' nfl_pls_boot2 = plspm_sem(Data=Offense, nfl_path, nfl_blocks2, modes = nfl_mode)
#' x11()
#' nfl_pls_boot2 %>%plspm_path_coefs_plot()
#' nfl_pls_boot2 %>%plspm_semPaths2()
#'
#' #'
#' #'
#'
#' # 함수 테스트
#' offense <- data.frame(
#'   YardsRushAtt = rnorm(10),
#'   RushYards = rnorm(10),
#'   RushFirstDown = rnorm(10),
#'   YardsPassComp = rnorm(10),
#'   PassYards = rnorm(10),
#'   PassFirstDown = rnorm(10)
#' )
#'
#' # 테스트 케이스 1
#' offense1 <- add_pca_scores(offense,
#'                            rush1 = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'                            pass1 = c("YardsPassComp", "PassYards", "PassFirstDown"))
#'
#' str(offense1)
#'
#' # 테스트 케이스 2
#' offense2 <- add_pca_scores(offense, method = "prcomp",
#'                            rush1 = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'                            pass1 = c("YardsPassComp", "PassYards", "PassFirstDown"))
#'
#' str(offense2)
#'
#' # 테스트 케이스 3 (adjust 옵션 포함)
#' offense3 <- add_pca_scores(offense, method = "prcomp", adjust = c(F, T),
#'                            rush1 = c("YardsRushAtt", "RushYards", "RushFirstDown"),
#'                            pass1 = c("YardsPassComp", "PassYards", "PassFirstDown"))
#'
#' str(offense3)
#' #'
#' #'
#'
#' }
#'
add_pca_scores <- function(data, ..., method = "nipals", adjust = NULL) {
  args <- list(...)

  if (method == "nipals") {
    for (name in names(args)) {
      cols <- args[[name]]
      if (is.character(cols)) {
        selected_cols <- data[, cols, drop = FALSE]
      } else if (is.numeric(cols)) {
        selected_cols <- data[, cols]
      } else {
        stop("Columns should be specified as either numeric indices or character names.")
      }

      pca_result <- plsdepot::nipals(selected_cols)
      data[[name]] <- pca_result$scores[, 1]
    }
  } else if (method == "prcomp") {
    for (i in seq_along(args)) {
      name <- names(args)[i]
      cols <- args[[name]]
      if (is.character(cols)) {
        selected_cols <- data[, cols, drop = FALSE]
      } else if (is.numeric(cols)) {
        selected_cols <- data[, cols]
      } else {
        stop("Columns should be specified as either numeric indices or character names.")
      }

      pca_result <- stats::prcomp(selected_cols, scale. = TRUE)
      scores <- pca_result$x[, 1]

      if (!is.null(adjust) && length(adjust) == length(args)) {
        if (adjust[i]) {
          scores <- -scores
        }
      }

      data[[name]] <- scores
    }
  }

  return(data)
}
