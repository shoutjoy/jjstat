
#' Alpha Table Combination
#'
#' @param data Data frame containing the variables
#' @param ... Variable names to be included in the alpha table, column name and number. Variable names to be included in the alpha table
#' @param check.keys check.keys=TRUE
#' @return alpha table
#' @export
#'
#' @examples
#' \dontrun{
#' ## select variable
#' bind_alpha_table(data = stat_onl, c("S_Review", "S_Add_learn", "S_Feedback"))
#'
#' #error
#' bind_alpha_table(data = stat_onl, c(S_Review, S_Add_learn, S_Feedback))
#'
#' #select col number
#' bind_alpha_table(data = stat_onl, c(9:11))
#'
#' # mutilple data
#' bind_alpha_table( stat_onl,
#'                   c("S_Review", "S_Add_learn", "S_Feedback"),
#'                   c("SE_Place", "SE_Time"),
#'                   c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
#'                   c("upgrade","satisfy","fatigue_inverse") )
#'
#'
#' #mixed multiple
#' bind_alpha_table( stat_onl,
#'                   c("S_Review", "S_Add_learn", "S_Feedback"),
#'                   c("SE_Place", "SE_Time"),
#'                   c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
#'                   c(5:6, 21) )
#'
#'
#'
#'
#' }
#'
bind_alpha_table = function(data, ..., check.keys=TRUE){
  # Arguments:
  #   data: Data frame containing the variables
  #   ...: Variable names to be included in the alpha table
  form = list(...)

  if(length(form)==1){
    result = jjstat::alpha_table( subset(data, select = form[[1]]),
                                  show="data", variable = letters[1] ,
                                  check.keys=check.keys)

  }else if(length(form) > 1 ){
    result = jjstat::alpha_table(subset(data,select = form[[1]]),
                                 show="data", variable = letters[1],
                                 check.keys=check.keys)
    for (i in 2:length(form)) {
      result <- rbind(result,
                      jjstat::alpha_table(subset(data, select = form[[i]]),
                                          show="data", variable = letters[i],
                                          check.keys=check.keys))
    }
  }

  result
}



#' bind_alpha_table문자 정리 함수
#'
#' @param data bind_alpha_table result
#'
#' @return table data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' jutr2 %>% select(B01:B25) %>%
#'   bind_alpha_table(c(1:5),c(6:10),c(11:15),c(16:20),c(21:25))%>%
#'   nice_Var()%>%  # <----this
#'   nice_table()
#' }
nice_Var = function(data){
  data %>% dplyr::mutate(Var= substring(Var, 1, 1))
}

