#' @title Logo plot for dot punctuation
#'
#' @description Plots the symbol or logo for dot punctuation
#'
#' @param plot A binary. If FALSE, only outputs grid co-ordinates for the logo,
#'        along with color labels. If TRUE, also plots the logo in a new grid
#'        window. Defaults to FALSE.
#' @param colfill  The color used to highlight the symbol.  Defaults to "green".
#' @param y_pos The y-position of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.1.
#' @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.5.
#' @param rad The radius of the dot. Defaults to 0.1.
#'
#' @return Returns a list with the following items.
#'         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{id}{id vector representing blocks in the logo co-ordinates}
#'         \item{fill}{a vector equal to the number of distinct ids or blocks in
#'                    the logo, whose elements correspond to colors of these blocks}
#'
#' @export
#' @examples
#' out <- dotletter(plot=TRUE)

dotletter <- function(plot = FALSE,
                      colfill="green",
                      y_pos = 0.1,
                      x_pos = 0.5,
                      rad = 0.1){

  angle2 <- seq(0,2*pi,length=200)
  x <- x_pos + rad*cos(angle2)
  y <- y_pos + rad*sin(angle2)

  id <- rep(1, length(x))

  fill <- colfill
  if(plot){
    grid.newpage()
    pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=gpar(fill=fill,
                         lwd=1))
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}


