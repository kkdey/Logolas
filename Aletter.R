

#############   Letter  A   ###########################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))


x <- c(0,4,6,10,8,6.8,3.2,2,0,3.6,5,6.4,3.6)
y <- c(0,10,10,0,0,3,3,0,0,4,7.5,4,4)
x <- 0.1*x
y <- 0.1*y

id <- c(rep(1,9),rep(2,4))

fill_symbol <- TRUE

if(fill_symbol){
    grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=gpar(fill=c("green","white"), 
                 lwd=10))
  }else{
    grid.polygon(x, y,
                 default.unit="native",
                 id=id,
                 gp=gpar(col="green", 
                 lwd=10))
  }
