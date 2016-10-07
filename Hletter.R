
#############   Letter  H   ###########################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))


x <- c(0, 0, 0.25, 0.25, 0.75, 0.75, 1, 1, 0.75, 0.75, 0.25, 0.25)
y <- c(0, 1, 1, 0.62, 0.62, 1, 1, 0, 0, 0.38, 0.38, 0)
#x <- 0.1*x
#y <- 0.1*y

id <- rep(1,12)

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
