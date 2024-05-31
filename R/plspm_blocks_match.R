#'  Matching plspm blocks names
#'
#' @param data  data
#' @param blocks plspm blocks
#' @param paths plspm paths
#' @param paths_name  path colnames
#'
#' @return block data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data(satisfaction)
#'
#'
#' # path matrix
#' IMAG = c(0,0,0,0,0,0)
#' EXPE = c(1,0,0,0,0,0)
#' QUAL = c(0,1,0,0,0,0)
#' VAL = c(0,1,1,0,0,0)
#' SAT = c(1,1,1,1,0,0)
#' LOY = c(1,0,0,0,1,0)
#' sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#' colnames(sat_path) = rownames(sat_path)
#' sat_path
#'
#' # blocks of outer model
#' sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#' sat_blocks
#'
#'
#' #by hand Very Very tired !!!!
#' cnames = satisfaction%>%colnames()
#' latent = colnames(sat_path) #"IMAG", "EXPE", "QUAL", "VAL",  "SAT",  "LOY"
#' latent
#'
#' list(IMAGE = cnames[1:5],
#'      EXPE = cnames[6:10],
#'      QUAL= cnames[11:15],
#'      VAL= cnames[16:19],
#'      SAT= cnames[20:23],
#'      LOY= cnames[24:27])
#'
#' #' #Using function : all input
#' plspm_blocks_match(data=satisfaction, blocks= sat_blocks, paths=sat_path )
#'
#' # nothing paths --> auto names
#' plspm_blocks_match(data= satisfaction, blocks=sat_blocks )
#'
#' # Input paths_name
#' plspm_blocks_match(data= satisfaction, blocks=sat_blocks,
#'                    paths_name =c("IMAG", "EXPE", "QUAL", "VAL",  "SAT",  "LOY") )
#'
#'
#' sat_blocks1 = plspm_blocks_match(satisfaction, paths=sat_path, blocks=sat_blocks )
#' sat_blocks1
#'
#' #YOu draw models
#' paste(plspm_paths2lav(sat_path) ,
#'       plspm_blocks2lav(sat_blocks1))%>%
#'   diagram_model(rotation = 1, sizeLat = 6, sizeMan = 4, sizeMan2=3,
#'                 mar=c(2,2,2,2))

#' }
#'
plspm_blocks_match <- function(data, blocks, paths=NULL, paths_name=NULL) {
  # Extract variable names for each block
  variable_names <- lapply(blocks,
                           function(block) {names(data)[block] }
  )
  # Extracting block names
  if(is.null(paths) & is.null(paths_name) ){
    block_names = paste0("Lat",1:length(blocks))
  }else if(!is.null(paths) & is.null(paths_name)){
    block_names <- colnames(paths)
  }else if(is.null(paths) & !is.null(paths_name)){
    block_names = paths_name
  }


  # # 결과를 리스트로 저장
  # setNames: setting list names
  result <- setNames(variable_names, block_names)

  return(result)
}

