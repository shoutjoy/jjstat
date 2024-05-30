#' add_t_sig
#'
#' @param data data
#' @param unite unite sig star +t
#' @param digits 3
#' @param col position t value
#' @param ns ns is ns sig
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' New_df = data.frame(rownames = c('IMAG -> EXPE', 'IMAG -> QUAL', 'IMAG -> VAL', 'IMAG -> SAT'),
#'   Original = c(0.578959128452789, 0.491156739303251, 0.393411171344413, 0.490971137879492),
#'  Mean.Boot = c(0.584251828061525, 0.495822941940513, 0.400070801457516, 0.5022633230577),
#'  Std.Error = c(0.0518709126012371, 0.0510037451930923, 0.0528454527326974, 0.0681551639743143),
#'  perc.025 = c(0.477709787839589, 0.391876781940297, 0.308196019466015, 0.369649328752085),
#'   perc.975 = c(0.664779330969341, 0.58560953201285, 0.495717252789497, 0.614490783702303)
#' )
#' #Auto-create a separate T without creating a SIG and combine it with the SIG to move it
#' New_df %>% add_t_sig(est=3, se=4, col=4, unite=T)
#'
#'
#' # Move only sigs after creating a t
#' New_df %>% add_t_sig(est=3, se=4, col=4, unite=F)
#'
#'# 열을 결합
#' New_df %>% add_t_sig(est=3, se=4, col=4, unite=F) %>%Unite(2,4)
#'
#' New_df %>% add_t_sig(est=3, se=4, col=4, unite=F)%>%Round(3) %>%Unite(2,4)
#'
#' }
add_t_sig <- function(data, est, se, col = ncol(data) + 1,
                      unite = FALSE, digits = 3, ns = " ns") {

  # Check if est and se are numeric
  if (is.numeric(est) && is.numeric(se)) {
    est <- names(data)[est]
    se <- names(data)[se]
  }

  # Calculate t and sig columns
  res <- data %>% mutate(
    t = ifelse(!!sym(se) == 0, 0, !!sym(est) / !!sym(se)),
    sig = ifelse(abs(t) == "", ns,
                 ifelse(abs(t) > 3.29, "***",
                        ifelse(abs(t) > 2.58, "**",
                               ifelse(abs(t) > 1.96, "*", ns))))
  )

  # Round t column
  res <- res %>% mutate(t = ifelse(t == 0, "",
                                   format(round(t, digits),justify="left")  ))

  # If unite is TRUE, combine t and sig columns
  if (unite) {
    res <- res %>%
      unite(t, t, sig, sep = "")
  }

  # Move the sig (or combined t) column to the specified position
  col_name <- if (unite) "t" else "sig"
  if (col <= ncol(res)) {
    res <- res %>% select(1:(col-1), !!sym(col_name), col:ncol(res))
  } else {
    res <- res %>% select(everything(), !!sym(col_name))
  }

  res
}
