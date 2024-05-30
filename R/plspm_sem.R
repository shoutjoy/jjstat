#' PLS-PM: Partial Least Squares Path Modeling
#'
#' @param Data matrix or data frame containing the manifest variables.
#' @param path_matrix A square (lower triangular) boolean matrix
#' representing the inner model (i.e. the path relationships between latent variables).
#' @param blocks 	list of vectors with column indices
#' or column names from Data indicating the sets of
#' manifest variables forming each block (i.e.
#' which manifest variables correspond to each block).
#' @param modes optional argument for runing the non-metric approach;
#' it is a list of string vectors indicating the type of measurement
#' scale for each manifest variable specified in blocks. scaling must be
#' specified when working with non-metric variables. Possible values:
#' "num" (linear transformation, suitable for numerical variables),
#' "raw" (no transformation), "nom" (non-monotonic transformation,
#' suitable for nominal variables), and "ord" (monotonic transformation,
#' suitable for ordinal variables).
#' @param scaling character vector indicating the type of measurement f
#' or each block. Possible values are: "A", "B", "newA", "PLScore",
#' "PLScow". The length of modes must be equal to the length of blocks.
#' @param scheme 	string indicating the type of inner weighting scheme.
#'  Possible values are "centroid", "factorial", or "path".
#' @param scaled standardized. Only used when scaling = NULL.
#' When (TRUE, data is scaled to standardized values (mean=0 and variance=1).
#'  The variance is calculated dividing by N instead of N-1).
#' @param tol decimal value indicating the tolerance criterion
#' for the iterations (tol=0.000001). Can be specified between 0 and 0.001.
#' @param maxiter integer indicating the maximum number of iterations
#'  (maxiter=100 by default). The minimum value of maxiter is 100.
#' @param plscomp optional vector indicating the number of PLS components
#' (for each block) to be used when handling non-metric data
#' (only used if scaling is provided)
#' @param boot.val 	whether bootstrap validation should be performed.
#'  (FALSE by default).
#' @param br number bootstrap resamples. Used only when boot.val=TRUE. When boot.val=TRUE, the default number of re-samples is 100.
#' @param dataset whether the data matrix used in the computations should be retrieved (TRUE by default).
#' @param summary  TRUE
#'
#' @return plspm result
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' ## Not run:
#' ## typical example of PLS-PM in customer satisfaction analysis
#' ## model with six LVs and reflective indicators
#' # load dataset satisfaction
#' data(satisfaction)
#' # path matrix
#' # IMAG = c(0,0,0,0,0,0)
#' # EXPE = c(1,0,0,0,0,0)
#' # QUAL = c(0,1,0,0,0,0)
#' # VAL = c(0,1,1,0,0,0)
#' # SAT = c(1,1,1,1,0,0)
#' # LOY = c(1,0,0,0,1,0)
#' # sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#'
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY")
#'   )
#' )
#'
#' # plot diagram of path matrix
#' innerplot(sat_path)
#' # blocks of outer model
#' # sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#' #' #####################
#' library(jjstat)
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
#'
#' }
plspm_sem <- function(Data, path_matrix, blocks, modes = rep("A", ncol(path_matrix)),
                      scaling = NULL, scheme = "centroid", scaled = TRUE,
                      tol = 1e-06, maxiter = 100, plscomp = NULL,
                      boot.val = TRUE, br = 500, seed=NULL,
                      dataset = TRUE, summary = TRUE) {

  cat("\n
    Wait for it.
    PLaunch Bootstrap
    PARTIAL LEAST SQUARES PATH MODELING (PLS-PM)
    jjstat package By Park Joonghee PhD \n\n\n")


  # setseed as.integer(Sys.Date()
  if (is.null(seed)) {
    seed = as.integer(Sys.Date())
    cat("Seed value:", seed, " \n")
  }
  set.seed(seed)


  library(progress)
  # 기본 PLSPM 분석 수행
  res <- plspm::plspm(Data = Data, path_matrix = path_matrix,
                      blocks = blocks, modes = modes,
                      scaling = scaling, scheme = scheme, scaled = scaled,
                      tol = tol, maxiter = maxiter, plscomp = plscomp,
                      boot.val = TRUE, br = br, dataset = dataset)

  # summary
  res_boot = lapply(res$boot,
                    function(x){ x%>%
                        row2col("relationships")%>%
                        Round(3)%>%
                        add_t_sig(3,4,5,T,ns="")%>%
                        unite_ci() %>%
                        rename(경로계수=Original,
                               평균계수=Mean.Boot, 표준오차=Std.Error
                                )
                      #  dplyr::select(-Original)
                    })


  # 부트스트랩 검증 수행
  if (boot.val & !is.null(br) & br > 0) {
    pb <- progress::progress_bar$new(
      format = "Bootstrapping [:bar] :percent :eta",
      total = br, clear = FALSE, width = 60
    )

    boot_results <- list()

    for (i in 1:br) {
      pb$tick()
      boot_data <- Data[sample(nrow(Data), replace = TRUE), ]
      boot_res <- plspm::plspm(Data = boot_data, path_matrix = path_matrix,
                               blocks = blocks, modes = modes,
                               scaling = scaling, scheme = scheme, scaled = scaled,
                               tol = tol, maxiter = maxiter, plscomp = plscomp,
                               boot.val = FALSE, br = NULL, dataset = FALSE)
      boot_results[[i]] <- boot_res$path_coefs#%>%
        # long_df("to","from","coef")%>%
        # dplyr::filter(coefs !=0)%>%
        # Unite("from","to","paths", sep="->")%>%
        # dplyr::pull(coefs)
    }
    # res_colnames <- boot_res$path_coefs%>%
    #   long_df("to","from","coef")%>%
    #   dplyr::filter(coefs !=0)%>%
    #   Unite("from","to","paths", sep="->")%>%
    #   dplyr::pull(paths)
    #
    # # # 부트스트랩 통계 계산
    # boot_coefs <- do.call(rbind, boot_results)
    # colnames(boot_coefs) = res_colnames
    # boot_means <- apply(boot_coefs, 2, mean)
    # boot_se <- apply(boot_coefs, 2, sd)
    # #
    # res$bootstrap <- list(means.boot2 = boot_means, se.boot2 = boot_se)
    # bootstrap <- cbind.data.frame(means.boot2 = boot_means,
    #                               se.boot2 = boot_se)
  }


  if (summary) {
    # print(summary(res))
    cat("\n")
    print(res_boot)
    cat("\n Additional TEST(CFA) (Park Joonghee PhD). \n\n")

    cat("\n (1) Indicator Validity  \n")
    print( dall(res_boot$loadings %>%rename(신뢰구간=`95%CI`)))

    cat("\n (2) Internal Consistency: Composite Reliability, Convergent Validity\n")
    print(plspm_CRAVE(res) )

    cat("\n (3) Fornell & Locker(1981) \n")
    print(plspm_fl(res) )

    cat("\n (4) HTMT(heterotrait-monotrait ratio of the correlations)\n")
    print(plspm_htmt(res$data, plspm_extract_blocks(res$model)) )

    cat("\n (5) Total effect: direct, indirect \n")
    print(res$effect %>% full_join(
                  res_boot$total.efs%>%dplyr::select(1,5,6),
                  by="relationships"
                  )%>%
            cut_print())

    cat("\n (6) effect size  \n")
    print(plspm_f2(res))

    # cat("\n (7) additional boot Coeff \n")
    # print(boot_coefs)

    x11()
    cat("\n (7) Paths coeff sig \n")
    plspm_path_coefs_plot(res)
    return(res)
  }
}


#' #'
#' plspm_sem <- function(Data, path_matrix, blocks, modes = rep("A", ncol(path_matrix)),
#'                       scaling = NULL, scheme = "centroid", scaled = TRUE,
#'                       tol = 1e-06, maxiter = 100, plscomp = NULL,
#'                       boot.val = TRUE, br = 500, dataset = TRUE, summary = TRUE) {
#'
#'   cat("\n
#'     Wait for it.
#'     PLaunch Bootstrap
#'     PARTIAL LEAST SQUARES PATH MODELING (PLS-PM)
#'     jjstat package By Park Joonghee PhD \n\n\n")
#'
#'   library(progress)
#'   # 기본 PLSPM 분석 수행
#'   res <- plspm::plspm(Data = Data, path_matrix = path_matrix,
#'                       blocks = blocks, modes = modes,
#'                       scaling = scaling, scheme = scheme, scaled = scaled,
#'                       tol = tol, maxiter = maxiter, plscomp = plscomp,
#'                       boot.val = TRUE, br = br, dataset = dataset)
#'
#'   # summary
#'   res_boot = lapply(res$boot,
#'          function(x){ x%>%
#'              row2col("relationships")%>%
#'              Round(3)%>%
#'              add_t_sig(3,4,5,T,ns="")%>%
#'              unite_ci() %>%
#'              dplyr::select(-Original)%>%dall()
#'          })
#'
#'
#'   # 부트스트랩 검증 수행
#'   if (boot.val & !is.null(br) & br > 0) {
#'     pb <- progress::progress_bar$new(
#'       format = "Bootstrapping [:bar] :percent :eta",
#'       total = br, clear = FALSE, width = 60
#'     )
#'
#'     boot_results <- list()
#'
#'     for (i in 1:br) {
#'       pb$tick()
#'       boot_data <- Data[sample(nrow(Data), replace = TRUE), ]
#'       boot_res <- plspm::plspm(Data = boot_data, path_matrix = path_matrix,
#'                                blocks = blocks, modes = modes,
#'                                scaling = scaling, scheme = scheme, scaled = scaled,
#'                                tol = tol, maxiter = maxiter, plscomp = plscomp,
#'                                boot.val = FALSE, br = NULL, dataset = FALSE)
#'       boot_results[[i]] <- boot_res$path_coefs%>%
#'         long_df("to","from","coefs")%>%
#'         filter(coefs !=0)%>%
#'         Unite("from","to","paths", sep="->")%>%
#'         pull(coefs)
#'     }
#'     res_colnames <- boot_res$path_coefs%>%
#'       long_df("to","from","coefs")%>%
#'       filter(coefs !=0)%>%
#'       Unite("from","to","paths", sep="->")%>%
#'       pull(paths)
#'     # 부트스트랩 통계 계산
#'     boot_coefs <- do.call(rbind, boot_results)
#'     colnames(boot_coefs) = res_colnames
#'     boot_means <- apply(boot_coefs, 2, mean)
#'     boot_se <- apply(boot_coefs, 2, sd)
#'
#'     # res$bootstrap <- list(means.boot2 = boot_means, se.boot2 = boot_se)
#'   }
#'
#'
#'
#'   if (summary) {
#'     # print(summary(res))
#'     print(summary(res_boot))
#'   }
#'
#'   return(res)
#' }
