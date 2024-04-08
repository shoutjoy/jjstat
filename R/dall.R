
#' view data.frame
#'
#' @param data tibble
#' @param type all, head
#' @param n  rows n = 10 (default)
#'
#' @return data.frame
#' @export
#'

dall= function(data, type ="all", n=10){

all =  data %>% data.frame()
head = data %>% data.frame() %>% head(n)

switch(type, all= all, head = head )

}



#' Print all
#'
#' @param data tibble
#' @param n rows n = Inf
#'
#' @return view all
#' @export
#'

pall= function(data, n=Inf){
  data %>%print(n=n)
}
