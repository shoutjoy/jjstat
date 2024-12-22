#' A tool that outputs data to the viewer
#' @export
#' @param data data.frame or etc data, For , is an R object, which is typically a matrix or data frame. For , a list with each element being a returned value from .kable()xkables()kable()
#' @param caption table title
#' @param digits 	Maximum number of digits for numeric columns, passed to round(). This can also be a vector of length ncol(x), to set the number of digits for individual columns.
#' @param font_size table font size
#' @param full_width table wide and fit values
#' @param table tbble type 'paper', 'classic', 'dark', 'minimal'
#' @param show 'markdown' is output to the viewer, and 'data' is output to the console.
#' @param format 'markdown' and 'html'. default 'html'. A character string. Possible values are , , (Pandoc's pipe tables), (Pandoc's simple tables), and . The value of this argument will be automatically determined if the function is called within a knitr document. The value can also be set in the global option . If is a function, it must return a character string.latexhtmlpipesimplerstformatknitr.table.formatformat
#' @param row.names row names in table(Set if necessary), Logical: whether to include row names. By default, row names are included if is neither nor identical to .rownames(x)NULL1:nrow(x)
#' @param col.names column names in table (Set if necessary)
#' @param align Table value align, Column alignment: a character vector consisting of 'l' (left), 'c' (center) and/or 'r' (right). By default or if align = NULL, numeric columns are right-aligned, and other columns are left-aligned. If length(align) == 1L, the string will be expanded to a vector of individual letters, e.g. 'clc' becomes c('c', 'l', 'c'), unless the output format is LaTeX.
#' @param label Table label, he table reference label. By default, the label is obtained from .knitr::opts_current$get('label')
#' @param format.args A list of arguments to be passed to to format table values, e.g. .format()list(big.mark = ',')
#' @param escape 	Boolean; whether to escape special characters when producing HTML or LaTeX tables. When , you have to make sure that special characters will not trigger syntax errors in LaTeX or HTML.escape = FALSE
#' @param table.attr A character string for addition HTML table attributes. This is convenient if you simply want to add a few HTML classes or styles. For example, you can put 'class="table" style="color: red"'.
#' @param booktabs booktabs - T/F for whether to use the longtable format. If you have a table that will span over two or more pages, you will have to turn this on.
#' @param longtable longtable
#' @param valign You probably won't need to adjust this latex option very often. If you are familar with latex tables, this is the optional position for the tabular environment controling the vertical position of the table relative to the baseline of the surrounding text. Possible choices are , and (default)
#' @param position 	kable_styling: A character string determining how to position the table on a page. Possible values include left, center, right, float_left and float_right. Please see the package doc site for demonstrations. For a LaTeX table, if ⁠float_*⁠ is selected, LaTeX package wrapfig will be imported.
#' @param centering Align cener in Table default TRUE
#' @param lightable_options Options to customize lightable. Similar with bootstrap_options in kable_styling. Choices include basic, striped and hover.
#' @param html_font  A string for HTML css font. For example, html_font = '"Arial Narrow", arial, helvetica, sans-serif'.Everything else you need to specify in kable_styling.
#' @param general_title Section header for general footnotes. Default is "Note: ".
#' @param number A vector of footnote texts. Footnotes here will be numbered. There is no upper cap for the number of footnotes here
#' @param alphabet A vector of footnote texts, Footnotes here will be labeled with abc. The vector here should not have more than 26 elements.
#' @description
#' knitr's kable function is the foundation of this package. However, it has many latex/html specific arguments hidden under the ground unless you check its source code. This wrapper function is created to provide better documentation (and auto-complete yay) and at the same time, solve the auto format setting in a better way.
#' kableExtra uses the built-in bootstrap themes by default in kable_styling(). Alternatively, you can use a customized table themes for your table. This lightable table style sheet comes with three formats, namely lightable-minimal, lightable-classic, lightable-material and lightable-material-dark with hover and striped options.
#' @param catout  #p value msg
#' @param general  #p value msg
#'
#' @examples
#' # example code
#'
#'   \dontrun{
#'   markdown_table(mtcars[1:5,], table="minimal")
#'
#'   ###*** : p < .001, ** : p < .01, * : p < .05"
#'   }





#quick markdown_table , kable(format="html) using cfa2()-----------

markdown_table <- function(data,
                           caption = "Table 1. Capiton title input",
                           digits = 3,
                           font_size= 13,
                           full_width= F,
                           table ="paper",
                           show="data",
                           format="html",
                           row.names = NA,
                           col.names = NA,
                           align=NULL,
                           label = NULL,
                           format.args = list(),
                           escape = TRUE,
                           table.attr = "",
                           booktabs = FALSE,
                           longtable = FALSE,
                           valign = "t",
                           position = "center", #kbl_styling
                           centering = TRUE,
                           lightable_options= "basic",
                           html_font = '"Arial Narrow", arial, helvetica, sans-serif',
                           general_title="Note: ",
                           number = NULL,
                           alphabet = NULL,
                           catout=FALSE,
                           general = NULL
){
  # library(tidyverse)
  # library(kableExtra)
  # library(broom)
  # 자동으로 패키지 설치 및 로드하는 함수
  install_if_missing <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }

  # 필요한 패키지 설치 및 로드
  install_if_missing("tidyverse")
  install_if_missing("kableExtra")
  install_if_missing("broom")
  #논문에 넣을때 복사하여 넣을 것
  options(knitr.kable.NA = '')


  if(catout){
  cat(" *** : p < .001, ** : p < .01, * : p < .05")
  }

  if(show =="lm"){
    #논문 테이블 Viewer
    data %>%
      broom::tidy() %>% # tibble data
      dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        "")))) %>%
      kableExtra::kbl(digits = digits,
          caption =  caption) %>%
      kableExtra::kable_classic(full_width = full_width,
                                font_size = font_size,
                                lightable_options= lightable_options)

  }else if(show =="p_add"){
    data %>%as.data.frame() %>%
      dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                          ifelse(p.value < 0.01, "**",
                                 ifelse(p.value < 0.05, "*",
                                        "")))) %>%
      kableExtra::kbl(digits = digits,
          caption =  caption) %>%
      kableExtra::kable_classic(full_width = full_width,
                                font_size = font_size,
                                lightable_options= lightable_options)

  }else if(show =="data"){

    data <- data %>% as.data.frame() %>%
      kableExtra::kbl(digits = digits,
          format= format,
          # caption =  caption,
          row.names = row.names,
          col.names = col.names,
          align = align,
          caption = caption,
          label = label,
          format.args = list(),
          escape = escape,
          table.attr = table.attr,
          booktabs = booktabs,
          longtable = FALSE,
          valign = valign,
          # position = position,
          centering = TRUE
      )

    #re apply
    if(table == "paper"){
      data %>%  kableExtra::kable_paper(full_width = full_width,
                               font_size = font_size,
                            lightable_options = lightable_options) %>%
        kableExtra::footnote(general =general,
                             general_title = general_title,
                             # general = general,
                             number = number,
                             alphabet = alphabet
                             )


    }else if(table == "classic"){
      data %>%  kableExtra::kable_classic(full_width = full_width,
                                font_size =  font_size,
                                lightable_options = lightable_options)%>%
        kableExtra::footnote(general,
                             general_title = general_title,
                             # general = general,
                             number = number,
                             alphabet = alphabet)

    }else if(table == "dark"){
      data %>%  kableExtra::kable_material_dark(full_width = full_width,
                                      font_size =  font_size,
                                      lightable_options = lightable_options)%>%
        kableExtra::footnote(general = general,
                             general_title = general_title,
                             # general = general,
                             number = number,
                             alphabet = alphabet)

    }else if(table == "minimal"){
      data %>%  kableExtra::kable_minimal(full_width = full_width,
                                font_size =  font_size,
                                lightable_options = lightable_options)%>%
        kableExtra::footnote(general = general)

    }
    }else if(table == "basic"){
      data %>%  kableExtra::kable_styling(full_width = full_width,
                                font_size =  font_size,
                                lightable_options = lightable_options,
                                html_font = html_font,
                                position = position)%>%
        kableExtra::footnote(general = general,
                             general_title = general_title,
                             # general = general,
                             number = number,
                             alphabet = alphabet)

    }

  }



#' md: Generate Styled Data Tables for Reports
#'
#' This function generates styled tables for reports, supporting different formats and options for enhanced visual presentation.
#'
#' @param data A data frame or tibble to be styled and displayed.
#' @param caption A character string for the table caption. Default is "Table 1. Capiton title input".
#' @param digits An integer indicating the number of decimal places. Default is 3.
#' @param font_size A numeric value specifying the font size. Default is 13.
#' @param full_width Logical, whether the table should use full width. Default is FALSE.
#' @param table A character string specifying the table style. Options are "paper", "classic", "dark", "minimal", and "basic". Default is "paper".
#' @param show A character string specifying the type of display. Options are "data", "lm", or "p_add". Default is "data".
#' @param format The output format of the table. Default is "html".
#' @param row.names Logical or character vector for row names. Default is NA.
#' @param col.names A character vector specifying column names. Default is NA.
#' @param align A character vector specifying column alignment. Default is NULL.
#' @param label A character string for table label. Default is NULL.
#' @param format.args A list of additional arguments passed to `format()`. Default is an empty list.
#' @param escape Logical, whether to escape special characters. Default is TRUE.
#' @param table.attr A character string for additional HTML attributes. Default is an empty string.
#' @param booktabs Logical, whether to use LaTeX booktabs style. Default is FALSE.
#' @param longtable Logical, whether to use LaTeX longtable style. Default is FALSE.
#' @param valign A character string specifying vertical alignment. Default is "t".
#' @param position A character string for table position. Default is "center".
#' @param centering Logical, whether to center align the table. Default is TRUE.
#' @param lightable_options A character string for lightable styling options. Default is "basic".
#' @param html_font A character string specifying the font for HTML output. Default is '"Arial Narrow", arial, helvetica, sans-serif'.
#' @param general_title A character string specifying the title for footnotes. Default is "Note: ".
#' @param number A numeric vector specifying footnote numbering. Default is NULL.
#' @param alphabet A character vector specifying footnote alphabet markers. Default is NULL.
#' @param catout Logical, whether to output significance markers (e.g., **, *). Default is FALSE.
#' @param general A character string specifying a general footnote. Default is NULL.
#' @param pass Logical, whether to process the table styling. If TRUE, returns the original data. Default is FALSE.
#'
#' @return A styled table if `pass = TRUE`, otherwise returns the original data.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(A = c(1.234, 2.345, 3.456), B = c(4.567, 5.678, 6.789))
#'   md(data, caption = "Example Table", table = "classic")
#' }
md <- function(data,
               caption = "Table 1. Capiton title input",
               digits = 3,
               font_size = 13,
               full_width = FALSE,
               table = "paper",
               show = "data",
               format = "html",
               row.names = NA,
               col.names = NA,
               align = NULL,
               label = NULL,
               format.args = list(),
               escape = TRUE,
               table.attr = "",
               booktabs = FALSE,
               longtable = FALSE,
               valign = "t",
               position = "center",
               centering = TRUE,
               lightable_options = "basic",
               html_font = '"Arial Narrow", arial, helvetica, sans-serif',
               general_title = "Note: ",
               number = NULL,
               alphabet = NULL,
               catout = FALSE,
               general = NULL,
               pass = FALSE) {

  # library(tidyverse)
  # library(kableExtra)
  # library(broom)
  options(knitr.kable.NA = '')

  if (pass) {
    return(data)
  }

  if (catout) {
    cat(" *** : p < .001, ** : p < .01, * : p < .05")
  }

  if (show == "lm") {
    data %>%
      broom::tidy() %>%
      dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                                 ifelse(p.value < 0.01, "**",
                                        ifelse(p.value < 0.05, "*", "")))) %>%
      kableExtra::kbl(digits = digits, caption = caption) %>%
      kableExtra::kable_classic(full_width = full_width,
                                font_size = font_size,
                                lightable_options = lightable_options)
  } else if (show == "p_add") {
    data %>%
      as.data.frame() %>%
      dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                                 ifelse(p.value < 0.01, "**",
                                        ifelse(p.value < 0.05, "*", "")))) %>%
      kableExtra::kbl(digits = digits, caption = caption) %>%
      kableExtra::kable_classic(full_width = full_width,
                                font_size = font_size,
                                lightable_options = lightable_options)
  } else if (show == "data") {
    styled_table <- data %>%
      as.data.frame() %>%
      kableExtra::kbl(digits = digits, format = format,
                      caption = caption, row.names = row.names,
                      col.names = col.names, align = align, label = label,
                      format.args = format.args, escape = escape,
                      table.attr = table.attr, booktabs = booktabs,
                      longtable = longtable, valign = valign,
                      centering = centering)

    if (table == "paper") {
      styled_table %>%
        kableExtra::kable_paper(full_width = full_width,
                                font_size = font_size,
                                lightable_options = lightable_options) %>%
        kableExtra::footnote(general = general, general_title = general_title,
                             number = number, alphabet = alphabet)
    } else if (table == "classic") {
      styled_table %>%
        kableExtra::kable_classic(full_width = full_width,
                                  font_size = font_size,
                                  lightable_options = lightable_options) %>%
        kableExtra::footnote(general = general, general_title = general_title,
                             number = number, alphabet = alphabet)
    } else if (table == "dark") {
      styled_table %>%
        kableExtra::kable_material_dark(full_width = full_width,
                                        font_size = font_size,
                                        lightable_options = lightable_options) %>%
        kableExtra::footnote(general = general, general_title = general_title,
                             number = number, alphabet = alphabet)
    } else if (table == "minimal") {
      styled_table %>%
        kableExtra::kable_minimal(full_width = full_width,
                                  font_size = font_size,
                                  lightable_options = lightable_options) %>%
        kableExtra::footnote(general = general)
    }
  } else if (table == "basic") {
    data %>%
      kableExtra::kable_styling(full_width = full_width,
                                font_size = font_size,
                                lightable_options = lightable_options,
                                html_font = html_font,
                                position = position) %>%
      kableExtra::footnote(general = general, general_title = general_title,
                           number = number, alphabet = alphabet)
  }
}


