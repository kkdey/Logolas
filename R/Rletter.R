#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Rletter(plot=TRUE)


Rletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
      y.l1 <- 0.75 + 0.25*sin(angle)
      x.l1 <- 0.5 + 0.30*cos(angle)

      y.l2 <- 0.75 + 0.10*sin(angle)
      x.l2 <- 0.4 + 0.20*cos(angle)

      inner_x <- c(0.25, 0.40, x.l2, 0.25)
      inner_y <- c(0.85, 0.85, y.l2, 0.65)


      x <- c(0, 0, 0.5, x.l1, 0.3, 0.9, 0.6, 0.2, 0.2, inner_x)
      x <- 0.15 + 0.80*x
      y <- c(0, 1,  1,  y.l1, 0.5, 0,  0,  0.4,  0, inner_y)

      id <- c(rep(1, length(x)-length(inner_x)), rep(2, length(inner_x)))

      fill <- c(colfill, "white")

      if(plot){
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                          clip=TRUE))
        if(fill_symbol){
          grid::grid.polygon(x, y,
                             default.unit="native",
                             id=id,
                             gp=grid::gpar(fill=fill,
                                           lwd=lwd))
        }else{
          grid::grid.polygon(x, y,
                             default.unit="native",
                             id=id,
                             gp=grid::gpar(col=colfill,
                                           lwd=lwd))
        }
      }


      ll <- list("x"= x,
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}
