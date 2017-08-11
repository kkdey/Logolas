#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Mletter(plot=TRUE, fill_symbol = TRUE, colfill = "green")
#' out <- Mletter(plot=TRUE, fill_symbol = FALSE, colfill = "green")

Mletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 0, 0.2, 0.5, 0.8, 1, 1, 0.8, 0.8, 0.5, 0.2, 0.2)
      x <- 0.12 + 0.75*x
      y <- c(0, 1, 1, 0.6, 1, 1, 0, 0, 0.6, 0.2, 0.6, 0)


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


