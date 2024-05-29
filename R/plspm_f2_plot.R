#'  Function to plot f2 values
#'
#' @param plsres pls result
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #'
#' # Example data for plsres
#' plsres <- list(
#'   path_coefs = data.frame(
#'     자기효능감 = c(0, 0.884, 0.681, 0.373),
#'     진로동기 = c(0, 0, 0, 0.353),
#'     진로태도 = c(0, 0, 0, 0.188),
#'     진로준비 = c(0, 0, 0, 0),
#'     row.names = c("자기효능감", "진로동기", "진로태도", "진로준비")
#'   ),
#'   inner_summary = data.frame(
#'     R2 = c(0.0000000, 0.7817016, 0.4631848, 0.7195506),
#'     row.names = c("자기효능감", "진로동기", "진로태도", "진로준비")
#'   )
#' )
#'
#' # Run the function
#' plspm_f2(plsres, type="res")
#'
#'
#'
#' plspm_f2_plot(plsres)
#' }
#'
#'
plspm_f2_plot <- function(plsres) {

  f2_result <- plspm_f2(plsres, type="res")%>%
    Unite(1,2,"paths","->")
  # Convert f2_result to data frame for ggplot
  f2_result_df <- as.data.frame(f2_result)

  ggplot(f2_result_df, aes(x = paths, y = f2, fill=effectsize)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.02, linetype=2)+
    geom_hline(yintercept = 0.15, linetype=2, color="steelblue")+
    geom_hline(yintercept = 0.35, linetype=2, color= "red")+
    geom_text(aes(label= round(f2,3)), vjust=-0.5)+
    theme_bw() +
    labs(title = "f2 Values for Each Path",
         x = "Paths",
         y = "f2 Value") +
    theme(axis.text.x = element_text(size=12,
                                     angle = 90,
                                     hjust = 1,
                                     face="bold"))

}
