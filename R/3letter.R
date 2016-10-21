
##################   letter 3    ###########################################

threeletter <- function(plot = FALSE,
                         fill_symbol = TRUE,
                         colfill="green",
                         lwd =10){



  angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
  y.l1 <- 0.75 + 0.25*sin(angle)
  x.l1 <- 0.5 + 0.30*cos(angle)

  y.l2 <- 0.25 + 0.25*sin(angle)
  x.l2 <- 0.5 + 0.30*cos(angle)


  y.l3 <- 0.75 + 0.15*sin(angle)
  x.l3 <- 0.4 + 0.20*cos(angle)

  outer_x1 <- c(0.25, x.l1)
  outer_y1 <- c(1, y.l1)

  outer_x2 <- c(x.l2, 0.25)
  outer_y2 <- c(y.l2, 0)

  plot(c(outer_x1, outer_x2), c(outer_y1, outer_y2))

  inner_x1 <- c(0.25, 0.40, x.l3, 0.25)
  inner_y1 <- c(0.90, 0.90, y.l3, 0.60)

  y.l4 <- 0.25 + 0.15*sin(angle)
  x.l4 <- 0.4 + 0.20*cos(angle)

  inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
  inner_y2 <- c(0.40, 0.40, y.l4, 0.10)

  #plot(c(outer_x1, outer_x2, rev(inner_x2), rev(inner_x1)),
  #     c(outer_y1, outer_y2, rev(inner_y2), rev(inner_y1)))



  x <- c(outer_x1, outer_x2, rev(inner_x2), rev(inner_x1))
  x <- 0.05 + 0.90*x
  y <- c(outer_y1, outer_y2, rev(inner_y2), rev(inner_y1))

  id <- rep(1, length(x))
  fill <- c(colfill)

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

## out <- threeletter(plot=TRUE)
