
#' plspm block draw graph
#'
#' @param data blocked data
#' @param latent_size 20
#' @param layout nicely , kk, fr, drl, mds
#' @param edge_width 1
#' @param edge.alpha 0.7
#' @param edge_colour darkgreen
#' @param text.colour gray10
#' @param nudge_y 0.1
#' @param nudge_x -0.1
#' @param title  [Figure 1] Latent and items
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' edu_blocks_named <- blocks(
#'   Support = item("sup.help", "sup.under", "sup.safe", "sup.conc"),
#'   Advising = item("adv.comp", "adv.acces", "adv.comm", "adv.qual"),
#'   Tutoring= item( "tut.prof","tut.sched", "tut.stud", "tut.qual" ),
#'   Value = item("val.devel", "val.deci","val.meet", "val.info"),
#'   Satisfaction = item("sat.glad","sat.expe","sat.over"),
#'   Loyalty =item("loy.proud", "loy.recom","loy.asha","loy.back")
#' )
#'
#' edu_blocks_named
#' draw_blocks(edu_blocks_named)
#'
#' sat_blocks1 <- blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#' sat_blocks1
#'
#' draw_blocks(sat_blocks)

#'
#'
#' }
#'
 plspm_drawblocks <- function(data,
                         latent_size = 20,
                         edge_width = 1,
                         edge.alpha = 0.7,
                         edge_colour = "darkgreen",
                         text.colour = "gray10",
                         nudge_y = 0.1,
                         nudge_x = -0.1,
                         title = " [Figure 1] Latent and items ") {
  library(igraph)
  library(ggraph)

  # Create a graph
  graph <- graph_from_data_frame(
    d = do.call(rbind, lapply(names(data), function(latent_var) {
      data.frame(from = latent_var, to = data[[latent_var]])
    })),
    directed = TRUE
  )

  # Set node types and labels
  V(graph)$type <- ifelse(V(graph)$name %in% unlist(data), "item", "latent")
  V(graph)$label <- V(graph)$name

  # Plot the graph using ggraph with igraph's default layout
  gg <- ggraph(graph, layout = 'igraph') +
    geom_node_point(aes(color = type),
                    size = ifelse(V(graph)$type == "latent",
                                  latent_size, 5),
                    show.legend = TRUE) +
    geom_edge_link(aes(start_cap = label_rect(node1.type == 'latent'),
                       end_cap = label_rect(node2.type == 'latent')),
                   edge_width = edge_width,
                   alpha = edge.alpha,
                   arrow = grid::arrow(length = unit(3, 'mm'), ends = 'last'),
                   edge_colour = edge_colour) +
    geom_node_text(aes(label = label), repel = TRUE,
                   colour = text.colour,
                   fontface = 'bold',
                   nudge_y = nudge_y,
                   nudge_x = nudge_x) +
    theme_void() +
    theme(legend.position = 'bottom') +
    ggtitle(title)

  res <- list(graph, V(graph)$type, V(graph)$label, gg)
  res
}
