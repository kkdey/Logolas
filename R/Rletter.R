#' @title Logo plot for alphabet R
#'
#' @description Plots the symbol or logo for alphabet R (uppercase).
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
#' @export
#' @examples
#' out <- Rletter(plot=TRUE)


Rletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
      y.l1 <- 0.75 + 0.25*sin(angle)
      x.l1 <- 0.5 + 0.30*cos(angle)

      y.l2 <- 0.75 + 0.15*sin(angle)
      x.l2 <- 0.4 + 0.20*cos(angle)

      inner_x <- c(0.25, 0.40, x.l2, 0.25)
      inner_y <- c(0.90, 0.90, y.l2, 0.60)


      x <- c(0, 0, 0.5, x.l1, 0.3, 0.9, 0.6, 0.2, 0.2, inner_x)
      x <- 0.05 + 0.90*x
      y <- c(0, 1,  1,  y.l1, 0.5, 0,  0,  0.4,  0, inner_y)

      id <- c(rep(1, length(x)-length(inner_x)), rep(2, length(inner_x)))

      fill <- c(colfill, "white")

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
