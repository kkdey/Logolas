
#############   Letter  F   ###########################

#rm(list=ls())
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

Fletter <- function(fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  
      x <- c(0, 0, 0.8, 0.8, 0.15, 0.15, 0.5, 0.5, 0.15, 0.15)
      y <- c(0,1,1,0.85,0.85,0.575,0.575,0.425,0.425,0)

      id <- rep(1,10)
      fill <- colfill

      if(fill_symbol){
            grid.polygon(x, y,
                         default.unit="native",
                         id=id,
                         gp=gpar(fill=colfill, 
                                  lwd=lwd))
        }else{
            grid.polygon(x, y,
                         default.unit="native",
                         id=id,
                         gp=gpar(col=colfill, 
                                 lwd=lwd))
        }
      
      ll <- list("x"= x, 
                 "y"= y,
                 "id" = id,
                 "fill" = fill)
      return(ll)
}

out <- Fletter()
      
      
