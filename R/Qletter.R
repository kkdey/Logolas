#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Qletter(plot=TRUE)


Qletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(0, 2*pi, length.out=100))

      y.l1 <- 0.5 + 0.5*sin(angle)
      x.l1 <- 0.5 + 0.5*cos(angle)


      y.l2 <- 0.5 + 0.30*sin(angle)
      x.l2 <- 0.5 + 0.30*cos(angle)

      x.l3 <- c(0.6, 0.8, 1, 0.8)
      y.l3 <- c(0.3, 0.3, 0, 0)


      x <- c(x.l1, x.l2, x.l3)
      x <- 0.1 + 0.80*x
      y <- c(y.l1, y.l2, y.l3)

      id <- c(rep(1, length(x.l1)), rep(2, length(x.l2)), rep(3, length(x.l3)))
      fill=c(colfill,"white", colfill)



      if(plot){
        get_plot(x, y, id, fill, colfill, lwd = lwd, fill_symbol = fill_symbol)
      }


      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}



