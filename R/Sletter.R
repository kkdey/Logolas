#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- Sletter(plot=TRUE)


Sletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle1 <- c(seq((pi/2), 3*(pi/2), length.out=100))

      y.l1 <- 0.70 + 0.15*sin(angle1)
      x.l1 <- 0.5 + 0.30*cos(angle1)

      x_out <- c(1, x.l1)
      y_out <- c(1, y.l1)

      angle2 <- c(seq(-(pi/2), (pi/2), length.out=100))

      y.l2 <- 0.275 + 0.275*sin(angle2)
      x.l2 <- 0.5 + 0.45*cos(angle2)

      y.l3 <- 0.27 + 0.13*sin(angle2)
      x.l3 <- 0.5 + 0.25*cos(angle2)

      y.l4 <- 0.70 + 0.30*sin(angle1)
      x.l4 <- 0.5 + 0.5*cos(angle1)

      x <- c(0.85, x.l1, rev(x.l2), 0.1, 0.1, 0.5, x.l3, rev(x.l4), 0.85)
      x <- 0.10 + 0.80*x
      y <- c(0.85, y.l1, rev(y.l2), 0, 0.14, 0.14, y.l3, rev(y.l4), 1)

      id <- c(rep(1, length(x)))

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






