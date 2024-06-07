
#' Chi-square analysis by cell
#'
#' @param data table
#' @param type res, alll()default), chisq_result , observed   expected,   residuals ,  chisq_cell ,  stdres, chisq_cell_sig ,  cramersv ,  OE_ratio,    OE_sig ,   graph
#' @param trans plot transpose
#' @param plot TRUE
#' @param digits digits= 3
#'
#' @return chisq
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#'
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"),
#'                     party = c("Democrat","Independent", "Republican"))
#' M %>% chisq_test_cell()
#'
#' #notsig
#' examtest  = matrix(c(7,14,13,6), nrow=2,
#'                    dimnames= list(c("사전검사","사후검사"),
#'                                   c("합격","불합격")))
#' examtest
#' fisher.test(examtest)
#'
#' examtest %>% chisq_test_cell()
#' }
#'
#'
chisq_test_cell <- function(data,type="res", trans = FALSE, plot=TRUE,  digits=3) {
  data = data
  # Calculate chi-squared test
  if(is.table(data)){
    data= data %>%as.data.frame()%>%to_table()
  }else{
    data =as.matrix(data)
  }

  chisq_result <- chisq.test(data)
  cramersv = cramers_v(data)
  # Extract standardized residuals
  observed <- chisq_result$observed
  expected <- chisq_result$expected
  OE_ratio = observed/expected
  stdres <- chisq_result$stdres
  residuals <- chisq_result$residuals
  chisq_cell <- chisq_result$residuals^2

  # df= (ncol(data)-1)*(nrow(data)-1)
  # p_values <- pchisq(chisq_cell, 1, lower.tail = FALSE)
  # # Create a matrix to store significance markers
  markers <- matrix("", nrow = nrow(stdres), ncol = ncol(stdres))
  # # Mark values based on thresholds
  markers[abs(stdres) >= 1.96] <- "*"
  markers[abs(stdres) >= 2.58] <- "**"
  markers[abs(stdres) >= 3.29] <- "***"

  # # Combine stdres and markers
  chisq_sig <- combine_data(round(chisq_cell, digits), markers,"")
  OE_sig <- combine_data(round(OE_ratio, digits), markers,"")
  chisq_sig <- format(chisq_sig, justify="left")

  oesig_add = OE_sig%>%long_df("row","col","sig", cols=1:ncol(OE_sig)+1)
  oeplot = OE_ratio%>%long_df("row","col","values")

  if(plot){
    if(trans){
      # x11()
      # oesig_add = OE_sig%>%long_df("row","col","sig", cols=1:ncol(OE_sig)+1)
      graph = bind_cols(oeplot, sig=oesig_add$sig)%>%
        ggplot(aes(x=row, y=values))+
        geom_bar(stat="identity", aes(fill=row), show.legend = FALSE)+
        geom_text(aes(label= sig), vjust=-.3)+
        theme_bw()+
        facet_wrap(~ col)+
        scale_fill_grey()

    }else{
      # x11()
      graph = bind_cols(oeplot, sig=oesig_add$sig)%>%
        ggplot(aes(x=col, y=values))+
        geom_bar(stat="identity", aes(fill=col), show.legend = FALSE)+
        geom_text(aes(label= sig), vjust=-.3)+
        theme_bw()+
        facet_wrap(~ row)+
        scale_fill_grey()
    }
  }else{
    graph=NULL
  }

  all= list(chisq_result = chisq_result,
            observed = observed,
            expected = expected,
            residuals = residuals,
            chisq_cell = chisq_cell%>%addmargins(),
            stdres = stdres,
            chisq_cell_sig = chisq_sig,
            cramersv = cramersv,
            OE_ratio = OE_ratio,
            OE_sig = OE_sig,
            oeplot=oeplot,
            oesig_add= oesig_add,
            graph = graph)

  res = list(chisq_result = chisq_result,
             #  observed= observed,
             #  expected=expected,
             #  residuals = residuals,
             #  chisq_cell = chisq_cell%>%addmargins(),
             #  stdres = stdres,
             chisq_cell_sig = chisq_sig,
             cramersv = cramersv,
             #  OE_ratio=OE_ratio,
             OE_sig = OE_sig,
             graph=graph )

  switch(type, res=res, all=all, chisq_result = chisq_result,
         observed = observed,
         expected = expected,
         residuals = residuals,
         chisq_cell = chisq_cell%>%addmargins(),
         stdres = stdres,
         chisq_cell_sig = chisq_sig,
         cramersv = cramersv,
         OE_ratio = OE_ratio,
         OE_sig = OE_sig,
         graph = graph
  )

}
