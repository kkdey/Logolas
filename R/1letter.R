#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- oneletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
#' out <- oneletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")


oneletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  x <- c(0.15, 0.15, 0.45, 0.45, 0.28, 0.15, 0.45, 0.6, 0.6, 0.9, 0.9)
  y <- c(0, 0.2, 0.2, 0.75, 0.60, 0.60, 1, 1, 0.2, 0.2, 0)

  id <- rep(1, length(x))
  fill <- colfill

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}


