#' @import grid
#' @keywords internal
#' @rdname letters
#' @export
#' @examples
#' out <- Cletter(plot=TRUE, fill_symbol = TRUE, colfill = "green")
#' out <- Cletter(plot=TRUE, fill_symbol = FALSE, colfill = "green")


Cletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

    angle1 <- seq(0.3+pi/2,pi,length=100)
    angle2 <- seq(pi,1.5*pi,length=100)
    x.l1 <- 0.5 + 0.5*sin(angle1)
    y.l1 <- 0.5 + 0.5*cos(angle1)
    x.l2 <- 0.5 + 0.5*sin(angle2)
    y.l2 <- 0.5 + 0.5*cos(angle2)

    x.l <- c(x.l1,x.l2)
    y.l <- c(y.l1,y.l2)

    x <- c(x.l,rev(x.l))
    y <- c(y.l,1-rev(y.l))

    x.i1 <- 0.5 +0.30*sin(angle1)
    y.i1 <- 0.5 +0.30*cos(angle1)
    x.i1 <- x.i1[y.i1<=max(y.l1)]
    y.i1 <- y.i1[y.i1<=max(y.l1)]
    y.i1[1] <- max(y.l1)

    x.i2 <- 0.5 +0.30*sin(angle2)
    y.i2 <- 0.5 +0.30*cos(angle2)

    x.i <- c(x.i1,x.i2)
    y.i <- c(y.i1,y.i2)

    x1 <- c(x.i,rev(x.i))
    y1 <- c(y.i,1-rev(y.i))

    x <- c(x,rev(x1))
    x <- 0.05 + 0.90*x
    y <- c(y,rev(y1))

    id <- rep(1, length(x))
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


