

###########   dot letter  ######################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

dotletter <- function(colfill="green",
                      y_pos = 0.1,
                      x_pos = 0.5,
                      rad = 0.1){
  
  angle2 <- seq(0,2*pi,length=200)
  x <- x_pos + rad*cos(angle2)
  y <- y_pos + rad*sin(angle2)
  
  id <- rep(1, length(x))
  
  fill <- colfill
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(fill=fill, 
                       lwd=1))
  ll <- list("x"= x, 
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

out <- dotletter()

  
  