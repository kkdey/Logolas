#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- Tletter(plot=TRUE)

Tletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c( 0.4, 0.4, 0, 0, 1, 1, 0.6, 0.6)
      x <- 0.10 + 0.80*x
      y <- c( 0, 0.80, 0.80, 1, 1, 0.80, 0.80, 0)

      id <- rep(1,length(x))
      fill <- colfill
      colfill <- rep(colfill, length(unique(id)))

      if(plot){
        get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
      }

      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill,
                 "colfill" = colfill)
      return(ll)
}
