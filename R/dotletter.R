#' @description Plots the symbol or logo for dot punctuation
#'
#' @param y_pos The y-position of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.1.
#' @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.5.
#' @param rad The radius of the dot. Defaults to 0.1.
#'
#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- dotletter(plot=TRUE)

dotletter <- function(plot = FALSE,
                      fill_symbol = FALSE,
                      colfill="green",
                      y_pos = 0.1,
                      x_pos = 0.5,
                      rad = 0.1){

  if(fill_symbol){fill_symbol = FALSE}

  angle2 <- seq(0,2*pi,length=200)
  x <- x_pos + rad*cos(angle2)
  y <- y_pos + rad*sin(angle2)

  id <- rep(1, length(x))

  fill <- colfill
  colfill <- rep(colfill, length(unique(id)))

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    grid::grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=grid::gpar(fill=fill,
                               lwd=1))
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill,
             "colfill" = colfill)

  return(ll)
}


