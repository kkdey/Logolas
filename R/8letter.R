#' @keywords internal
#' @import grid
#' @rdname letters
#' @export
#' @examples
#' out <- eightletter(plot=TRUE)


eightletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  angle1 <- c(seq(-(pi/2) - 0.35, -2*pi, length.out=100), seq(0, -(pi/2)+0.35, length.out=100))

  y.l1 <- 0.75 + 0.25*sin(angle1)
  x.l1 <- 0.5 + 0.35*cos(angle1)

  angle2 <- c(seq(pi/2 - 0.35, 0, length.out=100), seq(0, -(3*pi/2) + 0.35, length.out=100))

  y.l2 <- 0.25 + 0.25*sin(angle2)
  x.l2 <- 0.5 + 0.35*cos(angle2)

  angle3 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  y.l3 <- 0.25 + 0.15*sin(angle3)
  x.l3 <- 0.5 + 0.15*cos(angle3)

  y.l4 <- 0.75 + 0.15*sin(angle3)
  x.l4 <- 0.5 + 0.15*cos(angle3)

  x <- c(x.l2, x.l1, x.l3, x.l4)
  y <- c(y.l2, y.l1, y.l3, y.l4)

  id <- c(rep(1, length(x.l1)+length(x.l2)), rep(2, length(x.l3)), rep(3, length(x.l4)))
  fill <- c(colfill, "white", "white")

  if(plot){
    get_plot(x, y, id, fill, colfill, lwd, fill_symbol)
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}





