grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

leftarrowletter <- function(fill_symbol = TRUE,
                             colfill="green",
                             lwd =10){
  
  x <- 1 - c(0.25, 0.25, 0.7, 0.7, 0.85, 0.7, 0.7)
  y <- c(0.45, 0.6, 0.6, 0.7, 0.53, 0.36, 0.45)
  
  id <- rep(1, length(x))
  
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

out <- leftarrowletter()

