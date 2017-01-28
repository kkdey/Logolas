#' @title Logo plot for alphabet J
#'
#' @description Plots the symbol or logo for alphabet J (uppercase).
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
#' out <- Jletter(plot=TRUE)

Jletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0.1, 0.1, 0.25, 0.25, 0.40, 0.40, 0.10, 0.10, 0.85, 0.85, 0.57, 0.57)
      x <- 0.15 + 0.80*x
      y <- c(0, 0.35, 0.35, 0.17, 0.17, 0.85, 0.85, 1, 1, 0.85, 0.85, 0)

      id <- rep(1,length(x))

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

