#' @title Logo plot for punctuation dash
#'
#' @description Plots the symbol or logo for punctuation dash (uppercase).
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
#' out <- dashletter(plot=TRUE)

dashletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  x <- c(0.25, 0.25, 0.75, 0.75)
  y <- c(0.45, 0.6, 0.6, 0.45)

  id <- rep(1, length(x))

  fill <- colfill

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                      clip=TRUE))
    if(fill_symbol){
      grid::grid.polygon(x, y,
                         default.unit="native",
                         id=id,
                         gp=grid::gpar(fill=fill,
                                       lwd=lwd))
    }else{
      grid::grid.polygon(x, y,
                         default.unit="native",
                         id=id,
                         gp=grid::gpar(col=colfill,
                                       lwd=lwd))
    }
  }




  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}



