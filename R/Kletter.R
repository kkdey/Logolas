


#############   Letter  K   ###########################


grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

Kletter <- function(fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  
    x <- c(0, 0, 0.2, 0.2, 0.60, 0.90, 0.2, 0.90, 0.60, 0.2, 0.2)
    y <- c(0, 1, 1, 0.70, 1, 1, 0.5, 0, 0, 0.30, 0)

    id <- rep(1,length(x))
    fill <- colfill

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
    
    ll <- list("x"= x, 
               "y"= y,
               "id" = id,
               "fill" = fill)
    return(ll)
}

out <- Kletter()


