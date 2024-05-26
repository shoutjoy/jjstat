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
#'
#' sat_blocks <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#'
#'
#' # vector of modes (reflective indicators)
#' sat_mod = rep("A", 6)
#' # apply plspm
#' satpls = plspm_sem(satisfaction, sat_path, sat_blocks, modes = sat_mod,
#'                scaled = FALSE)
#' # plot diagram of the inner model
#' innerplot(satpls)
#' # plot loadings
#' outerplot(satpls, what = "loadings")
#'
#' }
plspm_sem = function(Data, path_matrix, blocks, modes = rep("A",ncol(path_matrix)),
                     scaling = NULL, scheme = "centroid", scaled = TRUE,
                     tol = 1e-06, maxiter = 100, plscomp = NULL,
                     boot.val = TRUE, br = 200, dataset = TRUE, summary=TRUE){

  #using plspm
  res =  plspm::plspm(Data = Data, path_matrix = path_matrix,
                      blocks = blocks, modes = modes,
                      scaling = scaling, scheme = "centroid", scaled = scaled,
                      tol = tol, maxiter = maxiter, plscomp = plscomp,
                      boot.val = boot.val, br = br, dataset = dataset)

  if(summary){
    print(summary(res))
    return(res)
  }else{
    return(res)
  }
}
