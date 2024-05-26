#' add_t_sig
#'
#' @param data data
#' @param unite unite sig star +t
#' @param digits 3
#' @param col position t value
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' New_df = data.frame(rownames = c('IMAG -> EXPE', 'IMAG -> QUAL', 'IMAG -> VAL', 'IMAG -> SAT'),
#'                     Original = c(0.578959128452789, 0.491156739303251, 0.393411171344413, 0.490971137879492),
#'                     Mean.Boot = c(0.584251828061525, 0.495822941940513, 0.400070801457516, 0.5022633230577),
#'                     Std.Error = c(0.0518709126012371, 0.0510037451930923, 0.0528454527326974, 0.0681551639743143),
#'                     perc.025 = c(0.477709787839589, 0.391876781940297, 0.308196019466015, 0.369649328752085),
#'                     perc.975 = c(0.664779330969341, 0.58560953201285, 0.495717252789497, 0.614490783702303)
#' )
#'
#' New_df%>% mutate(t = Original/Std.Error) %>%
#'   add_t_sig(unite=TRUE, col= 5)
#' New_df %>%
# mutate(t = Original/Std.Error) %>%
#   add_t_sig(unite=FALSE, col= 3)
#'
#'
#'
#'
#' }
add_t_sig= function(data, unite= TRUE, digits=3, col=ncol(data)){

  res = data%>%mutate_col(
    sig = ifelse(data$t > 3.29,"***",
                 ifelse(data$t>2.58,"**",
                        ifelse(data$t> 1.96,"*","ns"))), col= col)

  if(unite){

    res = res%>%
      Round(digits, type="data.frame")%>%
      unite(t,t,sig, sep="")
  }
  res
}
