#' @title Logo plot for given co-ordinates and fill values
#'
#' @description Plots logo given the co-ordinates and fill values of the symbols.
#'
#' @param x  The X co-ordinates of the symbol
#' @param y The Y co-ordinates of the symbol
#' @param id The id of the different points based on how it will be grouped
#' @param fill The color vector used for filling the logo.
#' @param colfill The color label for the borders of the plot.
#' @param lwd Specifies the border width of the symbol. Defaults to 10.
#' @param fill_symbol A binary. If TRUE, the function would fill the symbol by
#'        the color represented in \code{colfill}, else colors the boundary
#'        of the symbol by \code{colfill}. Defaults to TRUE.
#'
#' @return Returns a plot of the symbol with the input co-ordinates and fill 
#'         colors
#' @import grid
#' @keywords internal
#' @export
#' @examples
#' angle2 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))
#' x1 <- 0.5 + 0.3*cos(angle2)
#' y1 <- 0.5 + 0.5*sin(angle2)
#' x2 <- 0.5 + 0.15*cos(angle2)
#' y2 <- 0.5 + 0.35*sin(angle2)
#' x <- c(x1, x2)
#' y <- c(y1, y2)
#' colfill <- "green"
#' id <- c(rep(1, length(x1)), rep(2, length(x2)))
#' fill <- c(colfill, "white")
#' get_plot(x, y, id, fill, colfill, lwd=10, fill_symbol=TRUE)


get_plot <- function(x, y, id, fill, colfill, lwd=10, fill_symbol=TRUE){
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                    clip=TRUE))
  if(fill_symbol){
    grid::grid.polygon(x, y,
                       default.unit="native",
                       id=id,
                       gp=grid::gpar(fill=fill,
                                     lwd=lwd))
  }else{
    grid::grid.polygon(x, y,
                       default.unit="native",
                       id=id,
                       gp=grid::gpar(col=colfill,
                                     lwd=lwd))
  }
}

