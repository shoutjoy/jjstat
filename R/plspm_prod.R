#' Create a multiply variable to create a moderator variable
#'
#' @param data data
#' @param blocks latent and measurement blocks
#' @param ... interation terms
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Using example data
#' data(satisfaction)
#' sat_blocks1 <- list(
#'   IMAG = paste0("imag", 1:5),
#'   EXPE = paste0("expe", 1:5),
#'   QUAL = paste0("qual", 1:5),
#'   VAL = paste0("val", 1:4),
#'   SAT = paste0("sat", 1:4),
#'   LOY = paste0("loy", 1:4)
#' )

#' # Partially select to create an interaction term
#' satisfaction%>% plspm_prod(sat_blocks1,
#'                            intIS = interlist(c("IMAG", 1, 2, 3), c("SAT", 1, 2, 3))) %>% str()
#'
#' # Partially select to create an interaction term : repeated
#' satisfaction%>% plspm_prod(sat_blocks1,
#'                            intIS = interlist(c("IMAG", 1, 2, 3), c("SAT", 1, 2, 3))
#' )%>% plspm_prod(sat_blocks1,
#'                 intIS = interlist(c("IMAG", 1, 2, 3), c("EXPE", 1, 2))
#' ) %>%str()
#'
#'
#' # Create an interaction variable
#' plspm_prod(satisfaction, sat_blocks1, intIS = interlist("IMAG", "SAT"))    %>%str()
#' # Create a variable that interacts with multiple variables
#' plspm_prod(satisfaction, sat_blocks1, intIS = interlist("IMAG", "SAT"),
#'            intIE = interlist("IMAG", "EXPE"))    %>%str()
#'
#' satisfaction%>% plspm_prod(sat_blocks1,
#'                            interlist(c("IMAG", 1, 2, 3), c("SAT", 1, 2, 3))) %>%str()
#'
#' satisfaction%>% plspm_prod(sat_blocks1,
#'                            interlist("IMAG", 1, 2, 3, "SAT", 1, 2, 3) ) %>%str()
#'
#' # plspm_prod(satisfaction, sat_blocks1, list("IMAG", "SAT"),
#' #                                        list("IMAG", "EXPE")) %>%str()
#'
#'
#' #  Use vector
#' plspm_prod(satisfaction, sat_blocks1,
#'            intIS = list(c("IMAG", 1, 2 ), c("SAT", 3,4)),
#'            intIE = list(c("IMAG", 1, 2, 3), c("EXPE" ,4,5))
#' ) %>%str()
#'
#' #interlist use use list
#' plspm_prod(satisfaction,
#'            sat_blocks1,
#'            intIS = interlist(list("IMAG", 1, 2 ,"SAT", 3,4)),
#'            intIE = interlist(list("IMAG", 1, 2, 3, "EXPE" ,4,5)) ) %>%str()
#'
#'
#' #interlist use now use list
#' plspm_prod(satisfaction,
#'            sat_blocks1,
#'            intIS = interlist("IMAG", 1, 2 ,"SAT", 3,4),
#'            intIE = interlist("IMAG", 1, 2, 3, "EXPE" ,4,5)) %>%str()

#' }
#'
#'
#'
plspm_prod <- function(data, blocks, ..., intname="int", n_name = 2) {
  interactions <- list(...)

  #prefix names
  for (k in seq_along(interactions)) {
    interaction <- interactions[[k]]
    # interaction_name <- paste0(intname, k)
    interaction_name <- intname

    # first block
    block1_info <- interaction[[1]]
    if (is.character(block1_info[1])) {
      block1_name <- block1_info[1]
      if (length(block1_info) == 1) {
        block1_vars <- blocks[[block1_name]]
        block1_indices <- seq_along(block1_vars)
      } else {
        block1_indices <- as.integer(block1_info[-1])
        block1_vars <- blocks[[block1_name]][block1_indices]
      }
    }

    # 2nd blocks
    block2_info <- interaction[[2]]
    if (is.character(block2_info[1])) {
      block2_name <- block2_info[1]
      if (length(block2_info) == 1) {
        block2_vars <- blocks[[block2_name]]
        block2_indices <- seq_along(block2_vars)
      } else {
        block2_indices <- as.integer(block2_info[-1])
        block2_vars <- blocks[[block2_name]][block2_indices]
      }
    }

    # Create interaction terms with variable names
    for (i in seq_along(block1_vars)) {
      for (j in seq_along(block2_vars)) {
        new_var_name <- paste0(interaction_name, "_",
                               substring(block1_name, 1, n_name),
                               substring(block2_name, 1, n_name),
                               "_",
                               substring(block1_vars[i], 1, 1),
                               substring(block2_vars[j], 1, 1),
                               block1_indices[i],
                               block2_indices[j])

        data[[new_var_name]] <- data[[block1_vars[i]]] * data[[block2_vars[j]]]
      }
    }
  }

  return(data)
}



#' Functions that compensate for the method of input
#'
#' @param ... LONG LIST
#'
#' @return list
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' # want list
#' intbasic = list(c("IMAG", 1, 2), c("SAT", 1, 2))
#' intbasic
#'
#' inta = interlist(list(c("IMAG", 1, 2, 3), c("SAT", 1, 2)));inta
#' intc = interlist(list("IMAG", 1, 2, 3, "SAT", 1, 2));intc
#'
#' intb = interlist(c("IMAG", 1, 2, 3), c("SAT", 1, 2));intb
#' intd = interlist("IMAG", 1, 2, 3, "SAT", 1, 2);intd
#' #'
#'
#' # Partially select to create an interaction term
#' satisfaction%>% plspm_prod(sat_blocks1,
#'                            list(c("IMAG", 1, 2, 3), c("SAT", 1, 2, 3))) %>%str()
#'
#' satisfaction%>% plspm_prod(sat_blocks1, inta) %>%str()
#' satisfaction%>% plspm_prod(sat_blocks1, intb) %>%str()
#' satisfaction%>% plspm_prod(sat_blocks1, intc) %>%str()
#'
#'
#'
#' satisfaction%>%
#' plspm_prod(sat_blocks1, interlist(list("IMAG", 1, 2, 3, "SAT", 1, 2))) %>%str()
#' satisfaction%>%
#' plspm_prod(sat_blocks1, interlist(list("IMAG", 1, "SAT", 1, 2))) %>%str()
#' satisfaction%>%
#' plspm_prod(sat_blocks1, interlist(list("IMAG", 1,3,  "SAT", 2, 3))) %>%str()
#'
#'
#'
#'
#' }
#'
#'
#'
interlist <- function(...) {
  i_list = list(...)

  # Convert input to a list
  if (length(i_list) == 1 && is.list(i_list[[1]])) {
    input_list = i_list[[1]]
  } else {
    input_list = i_list
  }


  result <- list()
  current_block <- NULL
  current_indices <- c()

  for (item in input_list) {
    if (is.character(item)) {
      if (!is.null(current_block)) {
        result <- append(result, list(c(current_block, current_indices)))
      }
      current_block <- item
      current_indices <- c()
    } else {
      current_indices <- c(current_indices, as.integer(item))
    }
  }

  if (!is.null(current_block)) {
    result <- append(result, list(c(current_block, current_indices)))
  }

  return(result)
}
