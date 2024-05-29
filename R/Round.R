#' Find and round numeric variables only
#'
#' @param data data.frame
#' @param digits default 2
#' @param exclude exclude variable not adapted
#' @param type data type tibble or data.frame , matrix
#'
#' @return rounding data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # vector
#' Round(12.345654)
#'
#' ##data.frame
#' mtcars%>% Round(1)
#'
#'  ##
#' dff = data.frame(A = c(1.23456, 2.34567, 3.45678),
#'                  B = c(4.56789, 5.67890, 6.78901),
#'                 C = c("a", "b", "c"))
#'  ## rounding 1
#'  dff%>%Round(1)
#'
#' #exclue variable B
#'  dff%>%Round(1, exclude ="B")
#'
#' }
Round <- function(data, digits=3, type= "tibble", exclude = NULL){


  if(is.data.frame(data)){

    original_order <- colnames(data)
    # Rounding by pulling out excluded variables separately
        if(is.null(exclude)){
           data <- data[, original_order]  %>%
           mutate_if(is.numeric, round, digits)
           # Preserve column order

        }else{
           excluded_data <- data %>% dplyr::select(all_of(exclude))
          rounded_data <- data %>% dplyr::select(-all_of(exclude))

           rounded_data <- rounded_data %>% mutate_if(is.numeric, round, digits)
          # Insert excluded variables in their original column order

          data <- cbind(rounded_data, excluded_data)
           # data <- data[, original_order]  # Preserve column order
          }


  }else{
    data <- sapply(data,
                   function(x) {if(is.numeric(x)){round(x, digits)}})
  }

  options(pillar.sigfig = digits)
  tibble = tibble::tibble(data)

  data.frame = as.data.frame(data)

  matrix = as.matrix(data)

  switch(type,
         tibble = tibble,
         matirx = matrix,
         data.frame = data.frame)

}





#' numeric round
#'
#' @param data data
#' @param digits 2
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' dataset = data.frame(
#' var = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
#' N = c(32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32),
#' MEAN = c(20.090625, 6.1875, 230.721875, 146.6875, 3.5965625, 3.21725, 17.84875, 0.4375, 0.40625, 3.6875, 2.8125),
#' SD = c(6.0269480520891, 1.78592164694654, 123.938693831382, 68.5628684893206, 0.534678736070971, 0.978457442989697, 1.78694323609684, 0.504016128774185, 0.498990917235846, 0.737804065256947, 1.61519997763185),
#' MIN = c(10.4, 4, 71.1, 52, 2.76, 1.513, 14.5, 0, 0, 3, 1),
#' MAX = c(33.9, 8, 472, 335, 4.93, 5.424, 22.9, 1, 1, 5, 8),
#' Skew = c(0.610655017573288, -0.174611913930169, 0.381657034147599, 0.726023656361273, 0.265903904603492, 0.423146464177225, 0.369045278509436, 0.240257688369044, 0.364015894329692, 0.52885446231085, 1.05087376449851),
#' Kurt = c(-0.372766029820891, -1.76211977398177, -1.2072119466699, -0.135551121097421, -0.714700615734489, -0.0227107528393127, 0.335114219571393, -2.00193762400794, -1.92474142839069, -1.06975067515432, 1.25704307347743))
#' dataset
#' dataset%>% round2()
#'
#'
#'}
#'
#'
round2 = function(data, digits=2){
  # 데이터 타입에 따라 처리
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {
      data[[i]] <- round(data[[i]], digits)
    }
  }
  data
}
