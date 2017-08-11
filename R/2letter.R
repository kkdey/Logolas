#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- twoletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")
#' out <- twoletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")

twoletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  x <- c(0.9, 0.1, 0.1, 0.7, 0.5, 0.25, 0.1, 0.5, 0.95, 0.35, 0.9)
  y <- c(0, 0, 0.2, 0.65, 0.80, 0.6, 0.7, 1, 0.65, 0.2, 0.2)
  x <- 0.05 + 0.9*x

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


