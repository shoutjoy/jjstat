#' statistics by group
#'
#' @param formula general formula
#' @param data data.frame
#' @param add_var one sample name, Used to obtain statistics for one variable
#' @param stat TRUE t.test and aov(), thes stat ="t.test", or 'aov'
#' @param agg TRUE, mnay to many variable is showed. This functuion tha makes aggregate() work full
#' @param t transpose  when agg=TRUE
#' @param msdn msdn=TRUE is output mean, sd, N
#' @param gm default FALSE, TRUE Use group data obtained from group descriptive statistics to obtain group-level descriptive statistics (means, standard deviations, etc.) and to analyze a mixed model
#' @return Mea, SD, N, min, max, skew, kurt
#' @export
#'
#' @examples
#' \dontrun{
#' ## Used to obtain statistics for one variable
#' mysummaryBy(mpg ~ 1, data = mtcars)
#' mysummaryBy(mpg ~ 1, data = mtcars, "mpg")
#' mysummaryBy(mpg ~ 1, data = mtcars, add_var = "mpg")
#'
#' ##The group variable is 2 or greater
#' mysummaryBy(mpg ~ vs, data = mtcars)
#' mysummaryBy(mpg ~ vs+am, data = mtcars)
#' mysummaryBy(mpg ~ vs+am+cyl, data = mtcars)
#'
#' ##statistic data output
#' mysummaryBy(mpg ~ 1, data = mtcars, "mpg", stat="t.test")
#' mysummaryBy(mpg ~ vs, data = mtcars, stat="t.test")
#' mysummaryBy(mpg ~ vs+am, data = mtcars, stat="aov")
#' mysummaryBy(mpg ~ vs*cyl, data = mtcars, stat="aov")
#'
#' ##new group mean by group
#'#' group_by(mtcars, am) %>%
#'  summarise(mpg = mean(mpg)) %>%
#'   summarise(mean(mpg),sd(mpg),length(mpg), min(mpg),max(mpg))
#'
#' ### A tibble: 1 × 5
#' ###`mean(mpg)` `sd(mpg)` `length(mpg)` `min(mpg)` `max(mpg)`
#' ###<dbl>     <dbl>         <int>      <dbl>      <dbl>
#' ###  1        20.8      5.12             2       17.1       24.4
#'
#' mysummaryBy(mpg ~ am , data= mtcars) %>%
#'     rename(mpg = Mean) %>%   #change name
#'       mysummaryBy(formula = mpg ~ 1)  # mean of mpg
#'
#' mysummaryBy(mpg ~ am , data= mtcars) %>%
#'     rename(mpg = Mean) %>%   #change name
#'       mysummary("mpg")  # mean of mpg
#'
#'#multi group transpose table
#' mysummaryBy(mpg ~ vs+am, data = mtcars, agg=TRUE)
#'
#' ## transpose
#' mysummaryBy(mpg ~ vs+am, data = mtcars, agg=TRUE, t=TRUE)
#'
#' ##twoway
#' mysummaryBy(mpg ~ vs+am, data = mtcars, agg=TRUE)
#'
#'
#' ## twoway interaction
#' mysummaryBy(mpg ~ vs*am, data = mtcars, agg=TRUE, stat="aov")
#'
#'#add variable
#'mysummaryBy(mpg ~ vs, data = mtcars, gm=T)
#'# A tibble: 1 × 9
#' # grp   dv        N  MEAN    SD   MIN   MAX  Skew  Kurt
#' # <chr> <chr> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#' #  vs    mpg       2  20.6  5.61  16.6  24.6     0 -2.75
#'
#' }
#'
#'
#'
mysummaryBy <- function(
                        data,
                        formula,
                        add_var = NULL,
                        stat = FALSE,
                        agg = FALSE,
                        gm = FALSE,
                        fun = NULL,
                        msdn = FALSE,
                        t = FALSE
) {
  # Make sure the data object is provided
  if (missing(data)) stop("Please provide the data object as an argument.")
  # Import dplyr if needed
  if (requireNamespace("dplyr")) library(dplyr)
  # Aggregate with summary statistics
  func = formula(formula) #formula extraction
  from_all = func[2]%>%as.character()

  #analysis
  if(is.null(fun)){

    #MSDN----
    if(msdn){

      result <- aggregate(formula(formula), data,
                          FUN = function(x) {
                            c(
                              Mean = mean(x, na.rm = TRUE),
                              SD = sd(x, na.rm = TRUE),
                              N = length(x)
                            )
                          })

    }else{

      result <- aggregate(formula(formula), data,
                          FUN = function(x) {
                            c(
                              Mean = mean(x, na.rm = TRUE),
                              SD = sd(x, na.rm = TRUE),
                              N = length(x),
                              Min = min(x, na.rm = TRUE),
                              Max = max(x, na.rm = TRUE),
                              Skew = SKEW(x),
                              Kurt = KURT(x)
                            )
                          })

    } #MSDN


  }else{

    result <- aggregate(formula(formula), data, FUN = fun)
    # colnames(result) = colnames(result)

  }



  # stat data
  if(stat == "t.test"){
    stat_res = t.test(formula, data = data) %>%
      broom::tidy()%>%
      dplyr::select(1:5)

  }else if(stat == "aov"){
    stat_res = aov(formula(formula), data = data) %>%
      summary()
    # stat_res = aov(formula(formula), data = data) #%>% broom::tidy()
  }else{
    stat_res=NULL
  }



  # aggregate data
  if(agg){
    res = result %>% data.frame()

      if(t){  #transpose row
        res <- res %>%
          tibble::tibble()
  #results
        res

      }else{
        res <- res %>%
          # mutate(across(where(is.numeric), round, digits))%>%
          t()%>%
          data.frame() %>%
          tibble::rownames_to_column("stat_var")%>%tibble::tibble()
  #results
        res
      }




  }else{

    if(func[3]!='1()'){


      if(from_all=="."){
             res = result
             colnames(res) = colnames(result)
            }else{
            res = dplyr::bind_cols(var = result[,1: (ncol(result)-1) ],
                             result[[ncol(result)]] ) %>% tibble::tibble()
                 }

              if(is.null(stat_res)){
                  res
                }else{
                res = list(descriptive = res, statistic = stat_res)
                res
                  }

        }else{ #Used to obtain statistics for one variable



        res = dplyr::bind_cols(stat_var = add_var,
                               result[[ncol(result)]] ) %>% tibble::tibble()




      # stat_res is t.test or aov
             if(is.null(stat_res)){
                  res
                     }else{
                      res = list(descriptive = res, statistic = stat_res)
                         res
                       }

        }
    #
    # #summary M, SD, N
    # if(msdn){
    #   res<- res %>% dplyr::select(-Min,-Max,-Skew, -Kurt)
    # }else{
    #   res<- res
    # }
    #


        }  ## agg else }





  Res = res
  #Measure the average of each group - Measure the average of group level with the average of each group

  if(gm){
    Res = Res %>% jjstat::mysummary("Mean")
    Res = bind_cols(
      grp = as.character(func[3]),
      dv = as.character(func[2]),
      Res[,-1])
    Res
  }else{
    Res
  }

}




#' DescribeBy  statistics by group
#'
#' @param formula general formula
#' @param data data.frame
#' @param add_var one sample name, Used to obtain statistics for one variable
#' @param stat TRUE t.test and aov(), thes stat ="t.test", or 'aov'
#' @param agg TRUE, mnay to many variable is showed. This functuion that makes aggregate() work full
#' @param gm default FALSE, TRUE Use group data obtained from group descriptive statistics to obtain group-level descriptive statistics (means, standard deviations, etc.) and to analyze a mixed model
#' @param digits Troundings
#' @return Mea, SD, N, min, max, skew, kurt
#' @export
#'
#' @examples
#' \dontrun{
#' ## Used to obtain statistics for one variable
#' DescribeBy(mpg ~ 1, data = mtcars)
#' DescribeBy(mpg ~ 1, data = mtcars, "mpg")
#' DescribeBy(mpg ~ 1, data = mtcars, add_var = "mpg")
#'
#' ##The group variable is 2 or greater
#' DescribeBy(mpg ~ vs, data = mtcars)
#' DescribeBy(mpg ~ vs+am, data = mtcars)
#' DescribeBy(mpg ~ vs+am+cyl, data = mtcars)
#'
#' ##statistic data output
#' DescribeBy(mpg ~ 1, data = mtcars, "mpg", stat="t.test")
#' DescribeBy(mpg ~ vs, data = mtcars, stat="t.test")
#' DescribeBy(mpg ~ vs+am, data = mtcars, stat="aov")
#' DescribeBy(mpg ~ vs*cyl, data = mtcars, stat="aov")
#'
#' ##new group mean by group
#'#' group_by(mtcars, am) %>%
#'  summarise(mpg = mean(mpg)) %>%
#'   summarise(mean(mpg),sd(mpg),length(mpg), min(mpg),max(mpg))
#'
#' ### A tibble: 1 × 5
#' ###`mean(mpg)` `sd(mpg)` `length(mpg)` `min(mpg)` `max(mpg)`
#' ###<dbl>     <dbl>         <int>      <dbl>      <dbl>
#' ###  1        20.8      5.12             2       17.1       24.4
#'
#' DescribeBy(mpg ~ am , data= mtcars) %>%
#'     rename(mpg = Mean) %>%   #change name
#'       DescribeBy(formula = mpg ~ 1)  # mean of mpg
#' }
#'
#'
#'
DescribeBy <- function(data,formula,
                        add_var = NULL,
                        stat = FALSE,
                        agg = FALSE,
                        gm = FALSE,
                        digits = 2) {
  # Make sure the data object is provided
  if (missing(data)) stop("Please provide the data object as an argument.")
  # Import dplyr if needed
  if (requireNamespace("dplyr")) library(dplyr)
  # Aggregate with summary statistics
  func = formula(formula) #formula extraction

  # analysis SKEW, KURT
  result<- aggregate(formula(formula), data,
                      FUN = function(x) {
                        c(
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      N = length(x),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Skew = SKEW(x),
      Kurt = KURT(x),
      Variance = var(x, na.rm=TRUE),
      SE = mean(x, na.rm = TRUE)/sqrt(sd(x, na.rm = TRUE)),
      LCI = mean(x, na.rm = TRUE) - 1.96* mean(x, na.rm = TRUE)/sqrt(sd(x, na.rm = TRUE)),
      UCI = mean(x, na.rm = TRUE) + 1.96* mean(x, na.rm = TRUE)/sqrt(sd(x, na.rm = TRUE))
                        )
                      })



  if(stat=="t.test"){
    stat_res = t.test(formula, data = data) %>% broom::tidy()%>%select(1:5)
  }else if(stat=="aov"){
    stat_res = aov(formula(formula), data = data) %>% broom::tidy()
  }else{
    stat_res=NULL
  }


  # Use when there are too many independent and dependent variables
  if(agg){
    res = result %>%
      # t() %>%
      data.frame()

    res <- res %>%
      mutate(across(where(is.numeric), round, digits))%>%
      t()%>%
      data.frame() %>%
      tibble::rownames_to_column("stat_var") %>%
      tibble::tibble()

    colnames(res) = c("stat_var",  paste0("grp", 1:ncol(res[,-1])))

    #stat view anova only
    if(is.null(stat_res)){
      res
    }else{
      res = list(descriptive = res, ANOVA = stat_res)
      res
    }

  }else{
    if(func[3]!='1()'){
      res = dplyr::bind_cols(var = result[,1: (ncol(result)-1) ],
                             result[[ncol(result)]] ) %>% tibble::tibble()

      if(is.null(stat_res)){
        res
      }else{
        res = list(descriptive = res,statistic= stat_res)
        res
      }

    }else{ #Used to obtain statistics for one variable
      res = dplyr::bind_cols(stat_var = add_var,
                             result[[ncol(result)]] ) %>% tibble::tibble()
      if(is.null(stat_res)){
        res
      }else{
        res = list(descriptive = res,
                   statistic = stat_res)
        res
      }
    }
  }

  # Measure the average of each group - Measure the average of group level with the average of each group

  if(is.list(res)){
    res

  }else{
  Res = res

  if(gm){
    Res = Res %>% mysummary("Mean")
    Res = bind_cols(
      grp = as.character(func[3]),
      dv = as.character(func[2]),
      Res[,-1])
    Res %>%  mutate_if(is.numeric, round, digits)
  }else{
    Res %>%  mutate_if(is.numeric, round, digits)
  }
  }

}
# mysummaryBy <- function(data,formula,
#                         add_var = NULL,
#                         stat = FALSE,
#                         agg = FALSE,
#                         gm = FALSE,
#                         digits = 2) {
#   # Make sure the data object is provided
#   if (missing(data)) stop("Please provide the data object as an argument.")
#   # Import dplyr if needed
#   if (requireNamespace("dplyr")) library(dplyr)
#   # Aggregate with summary statistics
#   func = formula(formula) #formula extraction
#
#   # analysis SKEW, KURT
#   result <- aggregate(formula(formula), data,
#                       FUN = function(x) {
#                         c(
#                           Mean = mean(x, na.rm = TRUE),
#                           SD = sd(x, na.rm = TRUE),
#                           N = length(x),
#                           Min = min(x, na.rm = TRUE),
#                           Max = max(x, na.rm = TRUE),
#                           Skew = jjstat::SKEW(x),
#                           Kurt = jjstat::KURT(x)
#                         )
#                       })
#
#
#   if(stat=="t.test"){
#     stat_res = t.test(formula, data = data) %>% broom::tidy()%>%select(1:5)
#   }else if(stat=="aov"){
#     stat_res = aov(formula(formula), data = data) %>% broom::tidy()
#   }else{
#     stat_res=NULL
#   }
#
#
#   # Use when there are too many independent and dependent variables
#   if(agg){
#     res = result %>%
#       # t() %>%
#       data.frame()
#
#     res <- res %>%
#       mutate(across(where(is.numeric), round, digits))%>%
#       t()%>%
#       data.frame() %>%
#       tibble::rownames_to_column("stat_var") %>%
#       tibble::tibble()
#     colnames(res) = c("stat_var",  paste0("grp", 1:ncol(res[,-1])))
#
#     #stat view
#     if(is.null(stat_res)){
#       res
#     }else{
#       res = list(descriptive = res, statistic= stat_res)
#       res
#     }
#
#
#
#   }else{
#     if(func[3]!='1()'){
#       res = dplyr::bind_cols(var = result[,1: (ncol(result)-1) ],
#                              result[[ncol(result)]] ) %>%
#         tibble::tibble()
#
#       if(is.null(stat_res)){
#         res
#       }else{
#         res = list(descriptive = res,statistic= stat_res)
#         res
#       }
#
#     }else{ #Used to obtain statistics for one variable
#       res = dplyr::bind_cols(stat_var = add_var,
#                              result[[ncol(result)]] ) %>%
#         tibble::tibble()
#       if(is.null(stat_res)){
#         res
#       }else{
#         res = list(descriptive = res,
#                    ANOVA = stat_res)
#         res
#       }
#     }
#   }
