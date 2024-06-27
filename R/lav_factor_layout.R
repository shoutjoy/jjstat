#' 자동화된 semPaths모델 layout
#'
#' @param sem_res lavaan object
#' @param positions 각 변수의 위치를 변경 position = list("motiv" =c(1, 3))
#' @param nrow 4
#' @param ncol 5
#' @param new_position  list(c(1, 1), c(1, 2), c(3, 1), c(2, 3),c(2, 4))
#' @param variables 직업입력하는 변수명
#' @param ... ETC
#'
#' @return MAT
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(motivation)
#' str(motivation)
#' # Test the function with debug prints
#' motive_model <- "
#' achieve ~ motiv + ses + iq
#' motiv ~ ses + health + iq
#' "
#'
#' # Assuming 'motivation' is your data frame
#' motiv_sem <- SEM(motive_model, data = motivation)
#'
#' motiv_sem%>%summary(stand=TRUE)
#'
#' # 적용 예시
#' motiv_sem %>%
#'   lav_semPaths2(rotation = 2, se=TRUE,
#'   edge.label.position = 0.4,
#'                 layout =
#'                 lav_factor_layout(motiv_sem,
#'        new_position = list(c(1, 1), c(1, 2),
#'        c(3, 1), c(2, 3),c(2, 4)) ),
#'             rotate_resid = c("motiv" =160, "achieve"=160))
#'
#'
#' # 적용 예시
#' motiv_sem %>%
#'   lav_semPaths2(rotation = 2, se=TRUE,
#'   edge.label.position = 0.4,
#'                 layout = lav_factor_layout(motiv_sem,
#'                                            position = list("motiv" =c(1, 3))),
#'                 rotate_resid = c("motiv" =160, "achieve"=160))
#'
#' #'
#' }
#'
lav_factor_layout <- function(sem_res, positions = NULL, nrow = 4, ncol = 5,
                              new_position=NULL,variables=NULL,    ...) {

  if(is.null(variables)){
  # Extract the latent variable names
  lvs_names <- rev(unlist(sem_res@Model@dimNames[[1]][1]))

  }else{
    lvs_names <- variables
  }

  # Number of latent variables
  n_lvs <- length(lvs_names)

  # Initialize a matrix with NA values
  mat <- matrix(NA, nrow = nrow, ncol = ncol)

  # Default positions
  if(is.null(new_position)){
    default_positions <- list(c(2, 1), c(1, 2), c(3, 2), c(2, 3),
                              c(2, 4), c(3, 4), c(2, 5), c(4, 5))
  }else{
    default_positions = new_position
  }

  # Add additional positions from ...
  additional_positions <- list(...)
  if (length(additional_positions) > 0) {
    default_positions <- c(default_positions, additional_positions)
  }

  # Use provided positions or fall back to default: Functions to reposition
  if (!is.null(positions)) {
    for (name in names(positions)) {
      pos <- positions[[name]]
      mat[pos[1], pos[2]] <- name
    }
  }

  # Assign remaining positions using defaults
  index <- 1
  for (i in seq_len(n_lvs)) {
    if (is.null(positions) || !(lvs_names[i] %in% names(positions))) {
      while (index <= length(default_positions) && !is.na(mat[default_positions[[index]][1], default_positions[[index]][2]])) {
        index <- index + 1
      }


      if (index <= length(default_positions)) {
        pos <- default_positions[[index]]
        mat[pos[1], pos[2]] <- lvs_names[i]
        index <- index + 1
      }
    }
  }

  # Remove columns that contain only NA values
  mat <- mat[, colSums(is.na(mat)) < nrow(mat)]
  # Remove rows that contain only NA values
  mat <- mat[rowSums(is.na(mat)) < ncol(mat), ]
  return(mat)
}
