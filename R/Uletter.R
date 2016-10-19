

###############  U  letter   ###########################


grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

Uletter <- function(fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){
  
      angle <- c(seq(pi, 3*(pi/2), length.out=100), seq(3*(pi/2), 2*pi, length.out=100))

      y.l1 <- 0.5 + 0.5*sin(angle)
      x.l1 <- 0.5 + 0.5*cos(angle)

      y.l2 <- 0.5 + 0.3*sin(angle)
      x.l2 <- 0.5 + 0.3*cos(angle)


      x <- c(0, x.l1, 1, 0.8, rev(x.l2), 0.2)
      y <- c(1, y.l1, 1, 1, rev(y.l2), 1)

      id <- c(rep(1, length(x)))
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

out <- Uletter()



