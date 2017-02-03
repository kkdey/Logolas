#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Aletter(plot=TRUE)

Aletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

    x <- c(0,4,6,10,8,6.8,3.2,2,0,3.6,5,6.4,3.6)
    y <- c(0,10,10,0,0,3,3,0,0,4,7.5,4,4)
    x <- 0.1*x
    x <- 0.10 + 0.80*x
    y <- 0.1*y

    id <- c(rep(1,9),rep(2,4))
    fill <- c(colfill,"white")


    if(plot){
      get_plot(x, y, id, fill)
    }

    ll <- list("x"= x,
               "y"= y,
               "id" = id,
               "fill" = fill)
    return(ll)
}

