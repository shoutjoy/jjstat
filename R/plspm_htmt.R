
#' Define a function to calculate HTMT
#'
#' @param data data
#' @param blocks latent =~ item, plspl blocks
#' @param htmt2 TRUE Apply geometric mean
#' @param sig sig= TRUE- > show star
#' @param cut = 0.9, 0.85. 1
#' @param digits = 3
#'
#' @return result
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' sat_path = plspm_paths(
#' row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#' relationship = list(
#'   path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'   path("EXPE", c("QUAL","VAL","SAT")),
#'   path("QUAL", c("VAL","SAT")),
#'   path("VAL",c("SAT")),
#'   path("SAT","LOY"))
#' )
#'
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
#' # plspm_blocks2lav(sat_blocks1) %>% cat("\n")
#' data(satisfaction)
#' plspm_htmt(satisfaction, sat_blocks1)
#'
#' #'
#' #'
#' plspm_htmt(satisfaction, sat_blocks1)
#' plspm_htmt(satisfaction, sat_blocks1, sig=F)
#'
#' plspm_htmt(satisfaction, sat_blocks1,sig=F, htmt2=FALSE)
#' plspm_htmt(satisfaction, sat_blocks1,sig=T, cut=0.9, htmt2=FALSE)
#'
#' ## Reference
#' semTools::htmt(plspm_blocks2lav(sat_blocks1),
#'                satisfaction,
#'                htmt2 = T)
#'
#'  satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1,
#'   scaled = FALSE, boot.val =TRUE, br=100)
#'  #If you've bootstrapped
#'  plspm_htmt(satpls_boot)
#' #'
#'
#' }
#'
#'
#'
plspm_htmt <- function(plsres = NULL, data, blocks,  htmt2 = TRUE,
                       sig = TRUE, cut = 0.9, digits=3) {


  if(!is.null(plsres)){
   data = plsres$data
   blocks = plspm_extract_blocks(plsres$model)

  }else{

    data = data
    blocks = blocks
  }


  htmt_matrix <- matrix(0, nrow = length(blocks), ncol = length(blocks))
  rownames(htmt_matrix) <- colnames(htmt_matrix) <- names(blocks)

  for (i in seq_along(blocks)) {
    for (j in seq_along(blocks)) {
      if (i != j) {
        indicators_i <- blocks[[i]]
        indicators_j <- blocks[[j]]

        # 이질 특성-단일 방법 상관 계수
        hetero_corrs <- cor(data[, indicators_i], data[, indicators_j])

        # 단일 특성-단일 방법 상관 계수
        mono_corrs_i <- cor(data[, indicators_i])
        mono_corrs_j <- cor(data[, indicators_j])

        # 상관 계수의 평균 계산
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
          htmt_matrix[i, j] <- paste0(format(round(htmt_value, digits),
                                             nsmall=digits), "*")
        } else {
          htmt_matrix[i, j] <- round(htmt_value, digits)
        }
      }
    }
  }

  #result
  return(as.data.frame(format(round2(htmt_matrix,digits),
                              nsmall=digits))%>%
           lowerMat("","")
  )

}




#' Define a function to calculate HTMT
#'
#' @param data data
#' @param blocks latent =~ item, plspl blocks
#' @param htmt2 TRUE Apply geometric mean
#' @param sig sig= TRUE- > show star
#' @param cut = 0.9, 0.85. 1
#' @param digits = 3
#' @param imp imputation
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
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
#' # plspm_blocks2lav(sat_blocks1) %>% cat("\n")
#' data(satisfaction)
#' plspm_htmt2(satisfaction, sat_blocks1)
#'
#' #'
#' #'
#' plspm_htmt2(satisfaction, sat_blocks1)
#' plspm_htmt2(satisfaction, sat_blocks1, sig=F)
#'
#' plspm_htmt2(satisfaction, sat_blocks1,sig=F, htmt2=FALSE)
#' plspm_htmt2(satisfaction, sat_blocks1,sig=T, cut=0.9, htmt2=FALSE)
#'
#' ## Reference
#' semTools::htmt(plspm_blocks2lav(sat_blocks1),
#'                satisfaction,
#'                htmt2 = T)
#' #'
#'
#' }
#'
plspm_htmt2 <- function(data, blocks, htmt2 = TRUE,
                        sig = TRUE, cut = 0.9, digits = 3,imp="") {


  htmt_matrix <- matrix(0, nrow = length(blocks), ncol = length(blocks))
  rownames(htmt_matrix) <- colnames(htmt_matrix) <- names(blocks)

  for (i in seq_along(blocks)) {
    for (j in seq_along(blocks)) {
      if (i != j) {
        indicators_i <- blocks[[i]]
        indicators_j <- blocks[[j]]

        # 이질 특성-단일 방법 상관 계수
        hetero_corrs <- cor(data[, indicators_i], data[, indicators_j])

        # 단일 특성-단일 방법 상관 계수
        mono_corrs_i <- cor(data[, indicators_i])
        mono_corrs_j <- cor(data[, indicators_j])

        # 상관 계수의 평균 계산, lower matirx
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
          htmt_matrix[i, j] <- paste0(format(round(htmt_value, digits),
                                             nsmall = digits), "*")
        } else {
          htmt_matrix[i, j] <- format(round(htmt_value, digits), nsmall = digits)
        }
      } else {
        htmt_matrix[i, j] <- imp
      }
    }
  }

  # Lower triangular matrix로 변환
  htmt_lower <- htmt_matrix
  htmt_lower[upper.tri(htmt_lower)] <- imp

  return(as.data.frame(htmt_lower))
}
