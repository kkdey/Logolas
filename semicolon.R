

###########   semicolon letter  ######################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

colonletter <- function(colfill="green",
                        y_pos_1 = 0.3,
                        y_pos_2 = 0.7,
                        x_pos = 0.5,
                        rad = 0.1){
  
  angle2 <- seq(0,2*pi,length=200)
  
  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)
  
  x1 <- x_pos + c(0, 0, 0.05, 0.05, 0, 0.035)
  y1 <- y_pos_1 + c(0, 0.1, 0.1, 0, -0.1, 0)
  
  x2 <- x_pos + rad*cos(angle2)
  y2 <- y_pos_2 + rad*sin(angle2)
  
  x <- c(x1, x2)
  y <- c(y1, y2)
  
  
  id <- c(rep(1, length(x1)), rep(2, length(x2)))
  fill <- c(colfill, colfill)
  
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

out <- semicolonletter()
  
  