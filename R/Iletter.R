#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Iletter(plot=TRUE)

Iletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

    x <- c(0, 0, 0.4, 0.4, 0, 0, 1, 1, 0.6, 0.6, 1, 1)
    x <- 0.15 + 0.70*x
    y <- c(0, 0.15, 0.15, 0.85, 0.85, 1, 1, 0.85, 0.85, 0.15, 0.15, 0)

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
