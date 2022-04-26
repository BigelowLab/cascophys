#' Plot element mesh
#' 
#' @export
#' @param x the CascoBayPhysics object
#' @param border charcater color for drawing outline
#' @param ... other arguments for plot.sfc
plot_mesh <- function(x,
                                 border = "#999999",
                                 ...){
  plot(sf::st_geometry(x$M),
       border = border,
       ...)
}