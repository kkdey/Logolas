#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Jletter(plot=TRUE)

Jletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0.1, 0.1, 0.25, 0.25, 0.40, 0.40, 0.10, 0.10, 0.85, 0.85, 0.57, 0.57)
      x <- 0.15 + 0.80*x
      y <- c(0, 0.35, 0.35, 0.17, 0.17, 0.85, 0.85, 1, 1, 0.85, 0.85, 0)

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

