#' @title Logo plot for number 1
#'
#' @description Plots the symbol or logo for numeric 1.
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
#' out <- oneletter(plot=TRUE)


oneletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  x <- c(0.15, 0.15, 0.45, 0.45, 0.28, 0.15, 0.45, 0.6, 0.6, 0.9, 0.9)
  y <- c(0, 0.2, 0.2, 0.75, 0.60, 0.60, 1, 1, 0.2, 0.2, 0)

  id <- rep(1, length(x))
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


