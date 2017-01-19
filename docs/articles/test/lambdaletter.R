

lambdaletter <- function(colfill="green"){
  
  x <- c(0.15, 0.5, 0.85, 0.75, 0.5, 0.25)
  y <- c(0, 1, 0, 0, 0.8, 0)
  
  fill <- colfill
  id <- rep(1, length(x))
  
  ll <- list("x"= x,
             "y"= y,
             "id" = id,
             "fill" = fill)
  return(ll)
}

lambda <- lambdaletter()
grid::grid.newpage()
grid::pushViewport(grid::viewport(x=0.5,y=0.5,width=1, height=1,
                                  clip=TRUE))
grid::grid.polygon(lambda$x, lambda$y,
                     default.unit="native",
                     id=lambda$id,
                     gp=grid::gpar(fill=lambda$fill,
                                   lwd=10))
  
  