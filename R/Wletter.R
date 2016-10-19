#############   Letter  W   ###########################


Wletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
      x <- c( 0.25, 0, 0.15, 0.30, 0.5, 0.70, 0.85, 1, 0.75, 0.5)
      y <- c( 0, 1, 1, 0.3, 0.7, 0.3, 1, 1, 0, 0.4)


      id <- rep(1,length(x))

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

## out <- Wletter(plot = TRUE)

