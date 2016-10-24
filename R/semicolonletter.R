#' @title Logo plot for a semi-colon
#'
#' @description Plots the symbol or logo for a semi-colon.
#'
#' @param plot A binary. If FALSE, only outputs grid co-ordinates for the logo,
#'        along with color labels. If TRUE, also plots the logo in a new grid
#'        window. Defaults to FALSE.
#' @param fill_symbol A binary. If TRUE, the function would fill the symbol by
#'        the color represented in \Rcode{colfill}, else colors the boundary
#'        of the symbol by \Rcode{colfill}. Defaults to TRUE.
#' @param colfill  The color used to highlight the symbol.  Defaults to "green".
#' @param y_pos_1 The y-position of the center of the lower dot in [0,1] X [0,1]
#'        window. Defaults to 0.1.
#' @param y_pos_1 The y-position of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.1.
#' @param y_pos_2  The y-position of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.7.
#' @param x_pos The x-postition of the center of the dot in [0,1] X [0,1] window.
#'        Defaults to 0.5.
#' @param rad The radius of the indivudual symbols in the semicolon.
#' @param lwd Specifies the border width of the symbol. Defaults to 10.
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
#' out <- semicolonletter(plot=TRUE)

semicolonletter <- function(plot = FALSE,
                            colfill="green",
                            y_pos_1 = 0.1,
                            y_pos_2 = 0.4,
                            x_pos = 0.5,
                            rad = 0.1,
                            lwd=10){

  angle2 <- seq(0,2*pi,length=200)

  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)

  x1 <- x_pos + c(-0.05, -.05, 0.05, 0.05, -0.03, 0.01)
  y1 <- y_pos_1 + c(0, 0.1, 0.1, 0, -0.1, 0)

  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)

  x <- c(x1, x2)
  y <- c(y1, y2)


  id <- c(rep(1, length(x1)), rep(2, length(x2)))
  fill <- c(colfill, colfill)

  if(plot){
    grid.newpage()
    pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=gpar(fill=fill,
                         lwd=lwd))
  }

  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

