#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- Vletter(plot=TRUE)

Vletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

        x <- c( 0.5, 0, 0.2, 0.5, 0.8, 1)
        x <- 0.10 + 0.80*x
        y <- c( 0, 1, 1, 0.35, 1, 1)

        id <- rep(1,length(x))

        fill <- colfill
        colfill <- rep(colfill, length(unique(id)))

        if(plot){
          get_plot(x, y, id, fill, colfill, lwd = lwd , fill_symbol = fill_symbol)
        }

        ll <- list("x"= x,
                   "y"= y,
                   "id" = id,
                   "fill" = fill,
                   "colfill" = colfill)
        return(ll)
}
