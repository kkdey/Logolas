#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Lletter(plot=TRUE)


Lletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 0, 0.2, 0.2, 0.8, 0.8)
      x <- 0.15 + 0.80*x
      y <- c(0, 1, 1, 0.2, 0.2, 0)


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

