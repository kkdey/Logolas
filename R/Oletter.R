#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Oletter(plot=TRUE)

Oletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(0, 2*pi, length.out=100))

      y.l1 <- 0.5 + 0.5*sin(angle)
      x.l1 <- 0.5 + 0.5*cos(angle)


      y.l2 <- 0.5 + 0.30*sin(angle)
      x.l2 <- 0.5 + 0.30*cos(angle)

      id <- c(rep(1, length(x.l1)), rep(2, length(x.l2)))

      x <- c(x.l1, x.l2)
      x <- 0.10 + 0.80*x
      y <- c(y.l1, y.l2)

      fill <- c(colfill,"white")

      if(plot){
        get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
      }

      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}



