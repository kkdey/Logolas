#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Pletter(plot=TRUE)

Pletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
      y.l1 <- 0.75 + 0.25*sin(angle)
      x.l1 <- 0.4 + 0.30*cos(angle)

      y.l2 <- 0.75 + 0.10*sin(angle)
      x.l2 <- 0.4 + 0.15*cos(angle)

      inner_x <- c(0.25, 0.40, x.l2, 0.25)
      inner_y <- c(0.85, 0.85, y.l2, 0.65)


      x <- 0.2+c(0, 0, 0.5, x.l1, 0.25, 0.25, inner_x)
      x <- 0.05 + 0.90*x
      y <- c(0, 1,  1,  y.l1, 0.5, 0, inner_y)

      id <- c(rep(1, length(x)-length(inner_x)), rep(2, length(inner_x)))

      fill <- c(colfill, "white")

      if(plot){
        get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
      }

      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}
