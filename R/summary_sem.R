#' summary sem result and apa report
#'
#' @param lav_obj lavaan object
#' @param effect select "~"
#' @param effect2 select 2nd
#' @param type all, effect, apa, plot
#' @param md markdown default TRUE
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
summary_sem = function(lav_obj, effect = "~", effect2= NULL,md=TRUE, type="all"){

  res1 =  lav_obj %>% sem_effect(effect =effect, effect2= effect2)
  res2 =  lav_obj %>% sem_effect(effect =effect, effect2= effect2) %>%
    sem_apa(md = md, print=FALSE )
  res3 = lav_obj %>% sem_plot()

res =   list(res1, res2, res3)

switch(type,
       effect = res1,
       apa= res2,
       plot = res3,
       all = res )

}
