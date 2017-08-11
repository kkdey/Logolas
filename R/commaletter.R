#' @description Plots the symbol or logo for comma punctuation
#'
#' @param y_pos The y-position of the center of the comma in [0,1] X [0,1] window.
#'        Defaults to 0.1.
#' @param x_pos The x-postition of the center of the comma in [0,1] X [0,1] window.
#'        Defaults to 0.5.
#' @param lwd Specifies boundary of the comma. Defaults 10.
#'
#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- commaletter(plot=TRUE)

commaletter <- function(plot = FALSE,
                        fill_symbol = FALSE,
                        colfill="green",
                        y_pos = 0.1,
                        x_pos = 0.5,
                        lwd=10){

  if(fill_symbol){fill_symbol = FALSE}

  x <- x_pos + c(-0.05, -.05, 0.05, 0.05, -0.03, 0.01)
  y <- y_pos + c(0, 0.1, 0.1, 0, -0.1, 0)

  id <- c(rep(1, length(x)))
  fill <- c(colfill)

  if(plot){
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    grid::grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=grid::gpar(fill=fill,
                               lwd=lwd))
  }
  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

## out <- commaletter(plot = TRUE)
