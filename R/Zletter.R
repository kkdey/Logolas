#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- Zletter(plot=TRUE)

Zletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 1, 1, 0.25, 1, 1, 0, 0, 0.75, 0)
      x <- 0.15 + 0.75*x
      y <- c(1, 1, 0.8, 0.2, 0.2, 0, 0, 0.25, 0.8, 0.8)

      id <- c(rep(1, length(x)))

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


