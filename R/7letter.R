#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- sevenletter(plot=TRUE, fill_symbol = FALSE, colfill = "orange")
#' out <- sevenletter(plot=TRUE, fill_symbol = TRUE, colfill = "orange")

sevenletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  x <- c(0.35, 0.65, 0.90, 0.90, 0.70, 0.9, 0.1, 0.1, 0.67, 0.55, 0.30, 0.30, 0.50, 0.2)
  y <- c(0, 0.5, 0.5, 0.65, 0.65, 1, 1, 0.85, 0.85, 0.65, 0.65, 0.5, 0.5, 0)

  fill <- colfill
  id <- rep(1, length(x))

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}


