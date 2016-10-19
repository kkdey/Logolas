

#################  letter  B   ############################

Bletter <- function(plot=FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd=10){

    angle <- c(seq((pi/2), 0, length.out=100), seq(0, -(pi/2), length.out=100))

    y.l1 <- 0.75 + 0.25*sin(angle)
    x.l1 <- 0.5 + 0.15*cos(angle)

    y.l2 <- 0.75 + 0.10*sin(angle)
    x.l2 <- 0.5 + 0.1*cos(angle)


    y.l3 <- 0.25 + 0.25*sin(angle)
    x.l3 <- 0.5 + 0.15*cos(angle)

    y.l4 <- 0.25 + 0.10*sin(angle)
    x.l4 <- 0.5 + 0.1*cos(angle)

    x <- c(x.l1, x.l2, x.l3, x.l4)
    y <- c(y.l1, y.l2, y.l3, y.l4)

    angle <- c(seq(pi/2, 0, length.out=100), seq(0, -(pi/2), length.out=100))
    y.l1 <- 0.75 + 0.25*sin(angle)
    x.l1 <- 0.5 + 0.30*cos(angle)

    y.l2 <- 0.25 + 0.25*sin(angle)
    x.l2 <- 0.5 + 0.30*cos(angle)


    y.l3 <- 0.75 + 0.15*sin(angle)
    x.l3 <- 0.4 + 0.20*cos(angle)

    inner_x1 <- c(0.25, 0.40, x.l3, 0.25)
    inner_y1 <- c(0.90, 0.90, y.l3, 0.60)

    y.l4 <- 0.25 + 0.15*sin(angle)
    x.l4 <- 0.4 + 0.20*cos(angle)

    inner_x2 <- c(0.25, 0.40, x.l4, 0.25)
    inner_y2 <- c(0.40, 0.40, y.l4, 0.10)


    x <- c(0, 0, 0.5, x.l1, x.l2, inner_x1, inner_x2)
    y <- c(0, 1, 1, y.l1, y.l2, inner_y1, inner_y2)

    id <- c(rep(1, length(x)-length(inner_x1) - length(inner_x2)),
            rep(2, (length(inner_x1))), rep(3, length(inner_x2)))
    fill <- c(colfill, "white", "white")

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

## out <- Bletter(plot=TRUE)



