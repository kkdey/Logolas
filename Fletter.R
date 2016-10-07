
#############   Letter  F   ###########################


grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))


x <- c(0, 0, 0.8, 0.8, 0.15, 0.15, 0.5, 0.5, 0.15, 0.15)
y <- c(0,1,1,0.85,0.85,0.575,0.575,0.425,0.425,0)
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
