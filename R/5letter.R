

###############   5 letter   ##############################

fiveletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))

  y.l2 <- 0.25 + 0.25*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  y.l4 <- 0.25 + 0.10*sin(angle)
  x.l4 <- 0.5 + 0.10*cos(angle)


  outer_x2 <- c(x.l2, 0.25)
  outer_y2 <- c(y.l2, 0)

  inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
  inner_y2 <- c(0.35, 0.35, y.l4, 0.15)

  x_curve <- c(outer_x2, rev(inner_x2))
  y_curve <- c(outer_y2, rev(inner_y2))

  x.l3 <- c(0.25, 0.8, 0.8, 0.45, 0.45)
  y.l3 <- c(1, 1, 0.8, 0.8, 0.5)

  x <- c(x_curve, x.l3)
  y <- c(y_curve, y.l3)

  id <- rep(1, length(x))
  fill <- colfill

  if(plot){
    grid.newpage()
    pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    if(fill_symbol){
      grid.polygon(x, y,
                   default.unit="native",
                   id=id,
                   gp=gpar(fill=fill,
                           lwd=lwd))
    }else{
      grid.polygon(x, y,
                   default.unit="native",
                   id=id,
                   gp=gpar(col=colfill,
                           lwd=lwd))
    }
  }


  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

## out <- fiveletter(plot=TRUE)
