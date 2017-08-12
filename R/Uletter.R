#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- Uletter(plot=TRUE)

Uletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(pi, 3*(pi/2), length.out=100), seq(3*(pi/2), 2*pi, length.out=100))

      y.l1 <- 0.5 + 0.5*sin(angle)
      x.l1 <- 0.5 + 0.5*cos(angle)

      y.l2 <- 0.5 + 0.3*sin(angle)
      x.l2 <- 0.5 + 0.3*cos(angle)


      x <- c(0, x.l1, 1, 0.8, rev(x.l2), 0.2)
      x <- 0.1 + 0.80*x
      y <- c(1, y.l1, 1, 1, rev(y.l2), 1)

      id <- c(rep(1, length(x)))
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



