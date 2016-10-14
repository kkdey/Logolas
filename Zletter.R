

###############  Z  letter   ###########################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))

x <- c(0, 1, 1, 0.25, 1, 1, 0, 0, 0.75, 0)
y <- c(1, 1, 0.8, 0.2, 0.2, 0, 0, 0.25, 0.8, 0.8)

id <- c(rep(1, length(x)))

fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(fill=c("green"), 
                       lwd=10))
}else{
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(col="green", 
                       lwd=10))
}


