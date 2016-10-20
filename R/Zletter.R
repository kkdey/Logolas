

###############  Z  letter   ###########################

Zletter <- function(plot = FALSE,
                    fill_symbol = TRUE,
                    colfill="green",
                    lwd =10){

      x <- c(0, 1, 1, 0.25, 1, 1, 0, 0, 0.75, 0)
      x <- 0.05 + 0.90*x
      y <- c(1, 1, 0.8, 0.2, 0.2, 0, 0, 0.25, 0.8, 0.8)

      id <- c(rep(1, length(x)))

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

## out <- Zletter(plot = TRUE)


