#' @title Logo plot for dot punctuation
#'
#' @description Plots the symbol or logo for dot punctuation
#'
#' @param plot A binary. If FALSE, only outputs grid co-ordinates for the logo,
#'        along with color labels. If TRUE, also plots the logo in a new grid
#'        window. Defaults to FALSE.
#' @param colfill  The color used to highlight the symbol.  Defaults to "green".
#' @param y_pos The y-position of the center of the comma in [0,1] X [0,1] window.
#'        Defaults to 0.1.
#' @param x_pos The x-postition of the center of the comma in [0,1] X [0,1] window.
#'        Defaults to 0.5.
#' @param lwd Specifies boundary of the comma. Defaults 10.
#'
#' @return Returns a list with the following items.
#'         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{id}{id vector representing blocks in the logo co-ordinates}
#'         \item{fill}{a vector equal to the number of distinct ids or blocks in
#'                    the logo, whose elements correspond to colors of these blocks}
#' @keywords internal
#' @import grid
#' @export
#' @examples
#' out <- commaletter(plot=TRUE)

commaletter <- function(plot = FALSE,
                        colfill="green",
                        y_pos = 0.1,
                        x_pos = 0.5,
                        lwd=10){

  x <- x_pos + c(-0.05, -.05, 0.05, 0.05, -0.03, 0.01)
  y <- y_pos + c(0, 0.1, 0.1, 0, -0.1, 0)

  id <- c(rep(1, length(x)))
  fill <- c(colfill)

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    grid::grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=grid::gpar(fill=fill,
                               lwd=lwd))
  }
  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

## out <- commaletter(plot = TRUE)
