#' @title Logo plot for alphabet B
#'
#' @description Plots the symbol or logo for alphabet B (uppercase).
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
#' out <- Bletter(plot=TRUE)
#'

Bletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

    angle <- c(seq((pi/2), 0, length.out=100), seq(0, -(pi/2), length.out=100))

    y.l1 <- 0.75 + 0.25*sin(angle)
    x.l1 <- 0.5 + 0.15*cos(angle)

    y.l2 <- 0.75 + 0.10*sin(angle)
    x.l2 <- 0.5 + 0.1*cos(angle)


    y.l3 <- 0.25 + 0.25*sin(angle)
    x.l3 <- 0.5 + 0.15*cos(angle)

    y.l4 <- 0.25 + 0.10*sin(angle)
    x.l4 <- 0.5 + 0.1*cos(angle)

    x <- c(x.l1, x.l2, x.l3, x.l4)
    y <- c(y.l1, y.l2, y.l3, y.l4)

    angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
    y.l1 <- 0.75 + 0.25*sin(angle)
    x.l1 <- 0.5 + 0.30*cos(angle)

    y.l2 <- 0.25 + 0.25*sin(angle)
    x.l2 <- 0.5 + 0.30*cos(angle)


    y.l3 <- 0.75 + 0.15*sin(angle)
    x.l3 <- 0.4 + 0.20*cos(angle)

    inner_x1 <- c(0.25, 0.40, x.l3, 0.25)
    inner_y1 <- c(0.90, 0.90, y.l3, 0.60)

    y.l4 <- 0.25 + 0.15*sin(angle)
    x.l4 <- 0.4 + 0.20*cos(angle)

    inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
    inner_y2 <- c(0.40, 0.40, y.l4, 0.10)


    x <- c(0, 0, 0.5, x.l1, x.l2, inner_x1, inner_x2)
    x <- 0.15 + 0.90*x
    y <- c(0, 1, 1, y.l1, y.l2, inner_y1, inner_y2)

    id <- c(rep(1, length(x)-length(inner_x1) - length(inner_x2)),
            rep(2, (length(inner_x1))), rep(3, length(inner_x2)))
    fill <- c(colfill, "white", "white")

    if(plot){
      get_plot(x, y, id, fill)
    }

    ll <- list("x"= x,
               "y"= y,
               "id" = id,
               "fill" = fill)
    return(ll)
}



