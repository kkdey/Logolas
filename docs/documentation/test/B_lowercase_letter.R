
#############   Letter  lowrcase b   ###########################


bletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

  x1 <- c(0.2, 0.7, 0.7, 0.4, 0.4, 0.2)
  y1 <- c(0, 0, 0.4, 0.4, 0.8, 0.8)

  x2 <- c(0.30, 0.5, 0.5, 0.30)
  y2 <- c(0.1, 0.1, 0.3, 0.3)

  x <- c(x1, x2)
  y <- c(y1, y2)

  id <- c(rep(1,length(x1)),rep(2,length(x2)))
  fill <- c(colfill,"white")


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

## out <- bletter(plot=TRUE)
## out <- Bletter(plot=TRUE)


