
#############   Letter  H   ###########################

Hletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 0, 0.25, 0.25, 0.75, 0.75, 1, 1, 0.75, 0.75, 0.25, 0.25)
      y <- c(0, 1, 1, 0.62, 0.62, 1, 1, 0, 0, 0.38, 0.38, 0)

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

## out <- Hletter(plot = TRUE)


