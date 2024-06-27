
#' lavaan htmt
#'
#' @param sem_res lavaan result
#' @param data sem data
#' @param blocks sem blocks list data
#' @param htmt2 true using htmt2
#' @param sig true *
#' @param cut cut default 0.9, roburst 0.85, general 1
#' @param digits 3
#' @param imp "" upper triangle matrix
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data(satisfaction)
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
#' VAL ~  QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#' sat_sem0 <- sem(model1, data = satisfaction)
#'
#' #check
#' lav_htmt(sat_sem0)
#' # using data
#' lav_htmt(data= lav_return_data(sat_sem0), blocks= lav_return_blocks(sat_sem0))
#' #model data
#' lav_htmt(data= satisfaction, blocks =  lav_extract_mm(model1)%>% plspm_lav2blocks())
#' # using data
#' lav_htmt(data= satisfaction, blocks = model1)
#'
#' #option
#' lav_htmt(sat_sem0, htmt2=T, cut=0.85)
#'
#'
#' }
#'
lav_htmt <- function(sem_res = NULL, data = NULL, blocks = NULL,
                     htmt2 = TRUE,
                     sig = TRUE, cut = 0.9, digits = 3, imp = "") {

  library(dplyr)
  library(lavaan)
  library(purrr)
  if (is.null(data) && is.null(blocks)) {
    data <- lav_return_data(sem_res)
    blocks <- lav_return_blocks(sem_res)
  } else if (!is.null(data) && !is.null(blocks)) {
    data <- data
    if(is.list(blocks)){
      blocks <- blocks
    }else{
      blocks <-  lav_extract_mm(blocks)%>% plspm_lav2blocks()
    }

  } else {
    stop("Either provide a lavaan object or both data and blocks.")
  }

  htmt_matrix <- matrix(0, nrow = length(blocks), ncol = length(blocks))
  rownames(htmt_matrix) <- colnames(htmt_matrix) <- names(blocks)

  for (i in seq_along(blocks)) {
    for (j in seq_along(blocks)) {
      if (i != j) {
        indicators_i <- blocks[[i]]
        indicators_j <- blocks[[j]]

        hetero_corrs <- cor(data[, indicators_i], data[, indicators_j])

        mono_corrs_i <- cor(data[, indicators_i])
        mono_corrs_j <- cor(data[, indicators_j])

        if (htmt2) {
          mean_hetero_corrs <- exp(mean(log(abs(hetero_corrs[hetero_corrs != 0]))))
          mean_mono_corrs_i <- exp(mean(log(abs(mono_corrs_i[upper.tri(mono_corrs_i) & mono_corrs_i != 0]))))
          mean_mono_corrs_j <- exp(mean(log(abs(mono_corrs_j[upper.tri(mono_corrs_j) & mono_corrs_j != 0]))))
        } else {
          mean_hetero_corrs <- mean(abs(hetero_corrs))
          mean_mono_corrs_i <- mean(abs(mono_corrs_i[upper.tri(mono_corrs_i)]))
          mean_mono_corrs_j <- mean(abs(mono_corrs_j[upper.tri(mono_corrs_j)]))
        }

        htmt_value <- mean_hetero_corrs / sqrt(mean_mono_corrs_i * mean_mono_corrs_j)

        if (sig && htmt_value < cut) {
          htmt_matrix[i, j] <- paste0(format(round(htmt_value, digits), nsmall = digits), "*")
        } else {
          htmt_matrix[i, j] <- format(round(htmt_value, digits), nsmall = digits)
        }
      } else {
        htmt_matrix[i, j] <- imp
      }
    }
  }

  htmt_lower <- htmt_matrix
  htmt_lower[upper.tri(htmt_lower)] <- imp

  return(as.data.frame(htmt_lower))
}
