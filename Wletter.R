#############   Letter  W   ###########################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))


x <- c( 0.25, 0, 0.15, 0.30, 0.5, 0.70, 0.85, 1, 0.75, 0.5)
y <- c( 0, 1, 1, 0.3, 0.7, 0.3, 1, 1, 0, 0.4)
#x <- 0.1*x
#y <- 0.1*y

id <- rep(1,10)

fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(fill="green", 
                       lwd=1))
}else{
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(col="green", 
                       lwd=10))
}
