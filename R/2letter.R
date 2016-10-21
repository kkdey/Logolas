
#############  Letter  2  ##################################

twoletter <- function(plot = FALSE,
                       fill_symbol = TRUE,
                       colfill="green",
                       lwd =10){

  x <- c(0.9, 0.1, 0.1, 0.7, 0.5, 0.25, 0.1, 0.5, 0.95, 0.35, 0.9)
  y <- c(0, 0, 0.2, 0.65, 0.80, 0.6, 0.7, 1, 0.65, 0.2, 0.2)

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

## out <- twoletter(plot=TRUE)

