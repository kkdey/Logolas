

###############   7 letter   ##############################

sevenletter <- function(plot = FALSE,
                        fill_symbol = TRUE,
                        colfill="green",
                        lwd =10){

  x <- c(0.35, 0.65, 0.90, 0.90, 0.70, 0.9, 0.1, 0.1, 0.67, 0.55, 0.30, 0.30, 0.50, 0.2)
  y <- c(0, 0.5, 0.5, 0.6, 0.6, 1, 1, 0.85, 0.85, 0.6, 0.6, 0.5, 0.5, 0)

  fill <- colfill
  id <- rep(1, length(x))

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

## out <- sevenletter(plot=TRUE)

