#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Kletter(plot=TRUE)


Kletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

    x <- c(0, 0, 0.19, 0.19, 0.50, 0.75, 0.25, 0.75, 0.50, 0.19, 0.19)
    x <- 0.20 + 0.80*x
    y <- c(0, 1, 1, 0.70, 1, 1, 0.5, 0, 0, 0.30, 0)

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



