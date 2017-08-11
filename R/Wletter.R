#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- Wletter(plot=TRUE)

Wletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
      x <- c( 0.25, 0, 0.15, 0.3, 0.5, 0.7, 0.85, 1, 0.75, 0.5)
      x <- 0.10 + 0.80*x
      y <- c( 0, 1, 1, 0.35, 0.8, 0.35, 1, 1, 0, 0.4)


      id <- rep(1,length(x))

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


