#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Fletter(plot=TRUE)

Fletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 0, 0.7, 0.7, 0.15, 0.15, 0.4, 0.4, 0.15, 0.15)
      x <- 0.25 + 0.90*x
      y <- c(0,1,1,0.8,0.8,0.59,0.59,0.40,0.40,0)

      id <- rep(1,10)
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


