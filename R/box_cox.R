#' Apply Box-Cox Style Transformations to a Data Frame
#'
#' This function applies various mathematical transformations to all numeric columns in a data frame.
#'
#' @param df A data frame with numeric columns to be transformed.
#' @param use A character string specifying the transformation to apply. Options are:
#'   \itemize{
#'     \item \code{"log"}: Logarithmic transformation (\eqn{\log(x + 1)})
#'     \item \code{"sqrt"}: Square root transformation (\eqn{\sqrt{x}})
#'     \item \code{"inverse"}: Inverse transformation (\eqn{1 / (x + 1)})
#'     \item \code{"square"}: Square transformation (\eqn{x^2})
#'     \item \code{"cube_root"}: Cube root transformation (\eqn{x^{1/3}})
#'     \item \code{"exp"}: Exponential transformation (\eqn{e^x})
#'   }
#'
#' @return A data frame with transformed numeric columns.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Apply transformations to a data frame
#' df <- data.frame(
#'   col1 = c(1, 2, 3),
#'   col2 = c(4, 5, 6)
#' )
#'
#' # Logarithmic transformation
#' box_cox(df, use = "log")
#'
#' # Square root transformation
#' box_cox(df, use = "sqrt")
#'
#' # Cube root transformation
#' box_cox(df, use = "cube_root")
#' }
box_cox <- function(df, use = "log") {
  # 지원하는 변환 유형 정의
  valid_transforms <- c("log", "sqrt", "inverse", "square", "cube_root", "exp")

  # 변환 유형 확인
  if (!(use %in% valid_transforms)) {
    stop("지원하지 않는 변환 유형입니다. 유효한 옵션은 다음과 같습니다: ", paste(valid_transforms, collapse = ", "))
  }

  # 변환 적용
  df_transformed <- switch(use,
                           log = df %>% mutate(across(everything(), ~ log(. + 1))),
                           sqrt = df %>% mutate(across(everything(), ~ sqrt(.))),
                           inverse = df %>% mutate(across(everything(), ~ 1 / (. + 1))),
                           square = df %>% mutate(across(everything(), ~ .^2)),
                           cube_root = df %>% mutate(across(everything(), ~ .^(1/3))),
                           exp = df %>% mutate(across(everything(), ~ exp(.)))
  )

  return(df_transformed)
}
