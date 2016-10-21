

###############   6 letter   ##############################

sixletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  angle <- c(seq((2*pi/3) - 0.2, 0, length.out=100), seq(0, -(pi) , length.out=100))

  y.l2 <- 0.27 + 0.27*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)

  angle2 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  y.l4 <- 0.27 + 0.12*sin(angle2)
  x.l4 <- 0.5 + 0.12*cos(angle2)


  # x.l <- c(x.l2, x.l2[length(x.l2)], 0.65, 0.65, 0.50, 0.50, x.l2[1])
  #  y.l <- c(y.l2, 1, 1, 0.7, 0.7, 0.85, 0.85)

  x.l <- c(x.l2, x.l2[length(x.l2)], 0.40, 0.60)
  y.l <- c(y.l2, 0.5, 1, 1)

  inner_x2 <- c(0.3, 0.40, x.l4, 0.30)
  inner_y2 <- c(0.35, 0.35, y.l4, 0.15)

  x <- c(x.l, x.l4)
  y <- c(y.l, y.l4)


  id <- c(rep(1, length(x.l)), rep(2, length(x.l4)))
  fill <- c(colfill, "white")

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

## out <- sixletter(plot=TRUE)

