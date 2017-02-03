#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Hletter(plot=TRUE)


Hletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 0, 0.25, 0.25, 0.75, 0.75, 1, 1, 0.75, 0.75, 0.25, 0.25)
      x <- 0.15 + 0.70*x
      y <- c(0, 1, 1, 0.62, 0.62, 1, 1, 0, 0, 0.38, 0.38, 0)

      id <- rep(1,length(x))
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


