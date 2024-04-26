#' summary sem result and apa report
#'
#' @param lav_obj lavaan object
#' @param effect select "~"
#' @param effect2 select 2nd
#' @param type all, effect, apa, plot
#' @param opt  opt=1~4
#' @param md markdown default TRUEz
#' @param intercepts intercept=FALSE semptools
#' @param sizeMan sizeMan = 10
#' @param sizeMan2 sizeMan2 =6
#' @param mar mar = c(1,5,1,5)
#' @param caption caption possible
#'
#' @return result and md
#' @export
#'
#' @examples
#'
#' \dontrun{
#' example(sem)
#' fit %>% summary_sem()
#'
#' }
summary_sem = function(lav_obj, whatLabels = "est",
                       caption="Result of path Coefficient" ,
                       effect = "~", effect2= NULL
                       ,md = TRUE, type = "all", opt=1,
                       sizeMan = 10  , sizeMan2 = 6,
                       mar = c(1,5,1,5),
                       intercepts=FALSE){

  res1 =  lav_obj %>% sem_effect(effect =effect, effect2= effect2)
  res2 =  lav_obj %>% sem_effect(effect =effect, effect2= effect2) %>%
    sem_apa(md = md, caption=caption, print=FALSE )


  res3 = lav_obj %>% sem_plot(opt = opt,
                              whatLabels = whatLabels,
                              sizeMan = sizeMan , sizeMan2 = sizeMan2,
                              intercepts=intercepts,
                              mar=mar)
  # std = lav_obj %>% sem_plot(opt = opt,
  #                             whatLabels = "std",
  #                             intercepts=intercepts)
  # est = lav_obj %>% sem_plot(opt = opt,
  #                             whatLabels = "est",
  #                             intercepts=intercepts)


  res = list(res1, res2, res3)

switch(type,
       effect = res1,
       apa= res2,
       res3 = res3,
       # std = std,
       # est = est,
       all = res )

}
