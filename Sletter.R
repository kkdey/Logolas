

#############  letter  S   ####################################


grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))

Sletter <- function(fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  
      angle1 <- c(seq((pi/2), 3*(pi/2), length.out=100))

      y.l1 <- 0.70 + 0.20*sin(angle1)
      x.l1 <- 0.5 + 0.35*cos(angle1)

      x_out <- c(1, x.l1)
      y_out <- c(1, y.l1)

      angle2 <- c(seq(-(pi/2), (pi/2), length.out=100))

      y.l2 <- 0.25 + 0.25*sin(angle2)
      x.l2 <- 0.5 + 0.5*cos(angle2)

      y.l3 <- 0.25 + 0.15*sin(angle2)
      x.l3 <- 0.5 + 0.3*cos(angle2)

      y.l4 <- 0.70 + 0.30*sin(angle1)
      x.l4 <- 0.5 + 0.5*cos(angle1)

      x <- c(0.85, x.l1, rev(x.l2), 0.1, 0.1, 0.5, x.l3, rev(x.l4), 0.8)
      y <- c(0.9, y.l1, rev(y.l2), 0, 0.1, 0.1, y.l3, rev(y.l4), 1)

      id <- c(rep(1, length(x)))

      fill <- c(colfill, "white")
      
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

out <- Sletter()






