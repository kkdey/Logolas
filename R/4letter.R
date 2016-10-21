

##################   letter 4    ###########################################

fourletter <- function(plot = FALSE,
                         fill_symbol = TRUE,
                         colfill="green",
                         lwd =10){

  x <- c(0.3, 0.15, 0.6, 0.6, 0.75, 0.75, 0.85, 0.85, 0.75, 0.75, 0.6, 0.6, 0.33, 0.45)
  y <- c(1, 0.25, 0.25, 0, 0, 0.25, 0.25, 0.40, 0.40, 0.55, 0.55, 0.40, 0.40, 1)


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

out <- fourletter(plot=TRUE)

