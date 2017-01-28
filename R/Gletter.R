#' @title Logo plot for alphabet G
#' @title Logo plot for alphabet G
#'
#' @description Plots the symbol or logo for alphabet G (uppercase).
#'
#' @param plot A binary. If FALSE, only outputs grid co-ordinates for the logo,
#'        along with color labels. If TRUE, also plots the logo in a new grid
#'        window. Defaults to FALSE.
#' @param fill_symbol A binary. If TRUE, the function would fill the symbol by
#'        the color represented in \code{colfill}, else colors the boundary
#'        of the symbol by \code{colfill}. Defaults to TRUE.
#' @param colfill  The color used to highlight the symbol.  Defaults to "green".
#' @param lwd Specifies the border width of the symbol. Defaults to 10.
#'
#' @return Returns a list with the following items.
#'         \item{x}{X co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{y}{Y co-ordinates of the logo in the [0,1] X [0,1] grid window}
#'         \item{id}{id vector representing blocks in the logo co-ordinates}
#'         \item{fill}{a vector equal to the number of distinct ids or blocks in
#'                    the logo, whose elements correspond to colors of these blocks}
#' @import grid
#' @keywords internal
#' @export
#' @examples
#' out <- Gletter(plot=TRUE)

Gletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle1 <- seq(0.3+pi/2,pi,length=100)
      angle2 <- seq(pi,1.5*pi,length=100)
      x.l1 <- 0.5 + 0.5*sin(angle1)
      y.l1 <- 0.5 + 0.5*cos(angle1)
      x.l2 <- 0.5 + 0.5*sin(angle2)
      y.l2 <- 0.5 + 0.5*cos(angle2)

      x.l <- c(x.l1,x.l2)
      y.l <- c(y.l1,y.l2)

      x <- c(x.l,rev(x.l))
      y <- c(y.l,1-rev(y.l))

      x.i1 <- 0.5 +0.30*sin(angle1)
      y.i1 <- 0.5 +0.30*cos(angle1)
      x.i1 <- x.i1[y.i1<=max(y.l1)]
      y.i1 <- y.i1[y.i1<=max(y.l1)]
      y.i1[1] <- max(y.l1)

      x.i2 <- 0.5 +0.30*sin(angle2)
      y.i2 <- 0.5 +0.30*cos(angle2)

      x.i <- c(x.i1,x.i2)
      y.i <- c(y.i1,y.i2)

      x1 <- c(x.i,rev(x.i))
      y1 <- c(y.i,1-rev(y.i))

      x <- c(x,rev(x1))
      y <- c(y,rev(y1))

      h1 <- max(y.l1)
      r1 <- max(x.l1)

      h1 <- 0.4
      x.add <- c(r1,0.5,0.5,r1-0.2,r1-0.2,r1,r1)
      y.add <- c(h1,h1,h1-0.1,h1-0.1,0,0,h1)

      id <- c(rep(1,length(x)),rep(2,length(x.add)))

      x <- c(rev(x),x.add)
      x <- 0.05 + 0.90*x
      y <- c(rev(y),y.add)

      fill <- c(colfill, colfill)


      if(plot){
        get_plot(x, y, id, fill)
      }

      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}

