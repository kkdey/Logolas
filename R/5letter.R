#' @title Logo plot for number 5
#'
#' @description Plots the symbol or logo for numeric 5.
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
#' out <- fiveletter(plot=TRUE)

fiveletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))

  y.l2 <- 0.25 + 0.25*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  y.l4 <- 0.25 + 0.10*sin(angle)
  x.l4 <- 0.5 + 0.10*cos(angle)


  outer_x2 <- c(x.l2, 0.25)
  outer_y2 <- c(y.l2, 0)

  inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
  inner_y2 <- c(0.35, 0.35, y.l4, 0.15)

  x_curve <- c(outer_x2, rev(inner_x2))
  y_curve <- c(outer_y2, rev(inner_y2))

  x.l3 <- c(0.25, 0.8, 0.8, 0.45, 0.45)
  y.l3 <- c(1, 1, 0.8, 0.8, 0.5)

  x <- c(x_curve, x.l3)
  y <- c(y_curve, y.l3)

  id <- rep(1, length(x))
  fill <- colfill

  if(plot){
    get_plot(x, y, id, fill)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

