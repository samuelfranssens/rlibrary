#' Save graph
#'
#' @param filename filename
#' @param h height
#' @param w width
#' @keywords save a graph in dimensions that are perfect for powerpoint
#' @export


savegraph <- function(filename, h=1, w=1) ggsave(filename = paste0(filename,".png"), height = 4.76*h, width = 11.5*w, units = "in", dpi = 300)
