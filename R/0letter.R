

############  letter 0   #############################

zeroletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  angle2 <- c(seq(pi/2, 0, length.out=100), seq(0, -(3*pi/2), length.out=100))

  x1 <- 0.5 + 0.3*cos(angle2)
  y1 <- 0.5 + 0.5*sin(angle2)

  x2 <- 0.5 + 0.15*cos(angle2)
  y2 <- 0.5 + 0.35*sin(angle2)

  x <- c(x1, x2)
  y <- c(y1, y2)

  id <- c(rep(1, length(x1)), rep(2, length(x2)))
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

## out <- zeroletter(plot=TRUE)



