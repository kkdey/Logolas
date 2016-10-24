#' @title Logo plot for number 8
#'
#' @description Plots the symbol or logo for numeric 8.
#'
#' @param plot A binary. If FALSE, only outputs grid co-ordinates for the logo,
#'        along with color labels. If TRUE, also plots the logo in a new grid
#'        window. Defaults to FALSE.
#' @param fill_symbol A binary. If TRUE, the function would fill the symbol by
#'        the color represented in \Rcode{colfill}, else colors the boundary
#'        of the symbol by \Rcode{colfill}. Defaults to TRUE.
#' @param colfill  The color used to highlight the symbol.  Defaults to "green".
#' @param lwd Specifies the border width of the symbol. Defaults to 10.
#'
#' @return Returns a list with the following items.
#'         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{id}{id vector representing blocks in the logo co-ordinates}
#'         \item{fill}{a vector equal to the number of distinct ids or blocks in
#'                    the logo, whose elements correspond to colors of these blocks}
#'
#' @keywords internal
#' @export
#' @examples
#' out <- eightletter(plot=TRUE)


eightletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  angle1 <- c(seq(-(pi/2) - 0.35, -2*pi, length.out=100), seq(0, -(pi/2)+0.35, length.out=100))

  y.l1 <- 0.75 + 0.25*sin(angle1)
  x.l1 <- 0.5 + 0.35*cos(angle1)

  angle2 <- c(seq(pi/2 - 0.35, 0, length.out=100), seq(0, -(3*pi/2) + 0.35, length.out=100))

  y.l2 <- 0.25 + 0.25*sin(angle2)
  x.l2 <- 0.5 + 0.35*cos(angle2)


  x <- c(x.l4, x.l2)
  y <- c(y.l4, y.l2)

  angle3 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  y.l3 <- 0.25 + 0.15*sin(angle3)
  x.l3 <- 0.5 + 0.15*cos(angle3)

  y.l4 <- 0.75 + 0.15*sin(angle3)
  x.l4 <- 0.5 + 0.15*cos(angle3)

  x <- c(x.l2, x.l1, x.l3, x.l4)
  y <- c(y.l2, y.l1, y.l3, y.l4)

  id <- c(rep(1, length(x.l1)+length(x.l2)), rep(2, length(x.l3)), rep(3, length(x.l4)))
  fill <- c(colfill, "white", "white")

  if(plot){
    grid.newpage()
    pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    if(fill_symbol){
      grid.polygon(x, y,
                   default.unit="native",
                   id=id,
                   gp=gpar(fill=fill,
                           lwd=lwd))
    }else{
      grid.polygon(x, y,
                   default.unit="native",
                   id=id,
                   gp=gpar(col=colfill,
                           lwd=lwd))
    }
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}








