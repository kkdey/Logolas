#' @title Logo plot for alphabet U
#'
#' @description Plots the symbol or logo for alphabet U (uppercase).
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
#' @keywords internal
#' @import grid
#' @export
#' @examples
#' out <- Uletter(plot=TRUE)

Uletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(pi, 3*(pi/2), length.out=100), seq(3*(pi/2), 2*pi, length.out=100))

      y.l1 <- 0.5 + 0.5*sin(angle)
      x.l1 <- 0.5 + 0.5*cos(angle)

      y.l2 <- 0.5 + 0.3*sin(angle)
      x.l2 <- 0.5 + 0.3*cos(angle)


      x <- c(0, x.l1, 1, 0.8, rev(x.l2), 0.2)
      x <- 0.1 + 0.80*x
      y <- c(1, y.l1, 1, 1, rev(y.l2), 1)

      id <- c(rep(1, length(x)))
      fill <- colfill


      if(plot){
        get_plot(x, y, id, fill, colfill, lwd, fill_symbol)
      }

      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}



