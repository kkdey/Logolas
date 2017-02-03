#' @description Plots the symbol or logo for a colon.
#'
#' @param y_pos_1 The y-position of the center of the lower dot in [0,1] X [0,1]
#'        window. Defaults to 0.1.
#' @param y_pos_2  The y-position of the center of the upper dot in [0,1] X [0,1] window.
#'        Defaults to 0.7.
#' @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.5.
#' @param rad The radius of the indivudual symbols in the semicolon.
#' @param lwd Specifies the border width of the symbol. Defaults to 10.
#' @keywords internal
#' @import grid
#' @rdname letters
#'
#' @export
#' @examples
#' out <- colonletter(plot=TRUE)

colonletter <- function(plot = FALSE,
                        colfill="green",
                        y_pos_1 = 0.3,
                        y_pos_2 = 0.7,
                        x_pos = 0.5,
                        rad = 0.1,
                        lwd=10){

  angle2 <- seq(0,2*pi,length=200)

  x1 <- x_pos + rad*cos(angle2)
  y1 <- y_pos_1 + rad*sin(angle2)

  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)

  x <- c(x1, x2)
  y <- c(y1, y2)


  id <- c(rep(1, length(x1)), rep(2, length(x2)))
  fill <- c(colfill, colfill)

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
