

##################  letter  D   ############################
#rm(list=ls())
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))

Dletter <- function(fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){
  
    angle <- c(seq((pi/2), 0, length.out=100), seq(0, -(pi/2), length.out=100))

    y.l1 <- 0.5 + 0.5*sin(angle)
    x.l1 <- 0.5 + 0.5*cos(angle)

    y.l2 <- 0.5 + 0.3*sin(angle)
    x.l2 <- 0.5 + 0.3*cos(angle)

    x_out <- c(0, x.l1, 0) 
    y_out <- c(1, y.l1, 0)

    x_in <- c(0.2, 0.5, rev(x.l2), 0.2, 0.2)
    y_in <- c(0.2, 0.2, rev(y.l2), 0.8, 0.2)

    id <- c(rep(1, length(x_out)), rep(2, length(x_in)))

    x <- c(x_out, x_in)
    y <- c(y_out, y_in)

    fill_symbol <- TRUE
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

out <- Dletter()


