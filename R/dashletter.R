#' @keywords internal
#' @import grid
#' @rdname letters
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
    get_plot(x, y, id, fill)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}



