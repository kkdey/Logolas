#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- leftarrowletter(plot=TRUE)

leftarrowletter <- function(plot = FALSE,
                            fill_symbol = TRUE,
                            colfill="green",
                            lwd =10){

  x <- 1 - c(0.25, 0.25, 0.7, 0.7, 0.85, 0.7, 0.7)
  y <- c(0.45, 0.6, 0.6, 0.7, 0.53, 0.36, 0.45)

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

