

###########   comma letter  ######################

commaletter <- function(plot = FALSE,
                        colfill="green",
                        y_pos_1 = 0.1,
                        x_pos = 0.5,
                        lwd=10){

  x <- x_pos + c(-0.05, -.05, 0.05, 0.05, -0.03, 0.01)
  y <- y_pos_1 + c(0, 0.1, 0.1, 0, -0.1, 0)

  id <- c(rep(1, length(x)))
  fill <- c(colfill)

  if(plot){
    grid.newpage()
    pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                          clip=TRUE))
    grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=gpar(fill=fill,
                         lwd=lwd))
  }
  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

## out <- commaletter(plot = TRUE)
