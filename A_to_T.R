

#############   Letter  T  ###########################
library(grid)
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))


x <- 0.1* (c(0,4,6,10,8,6.8,3.2,2,0,3.6,5,6.4,3.6))
y <- 0.1*(c(0,10,10,0,0,3,3,0,0,4,7.5,4,4))
x1 <- 0.4*x
y1 <- 1*y


x <- c( 0.4, 0.4, 0, 0, 1, 1, 0.6, 0.6)
y <- c( 0, 0.85, 0.85, 1, 1, 0.85, 0.85, 0)
x2 <- 0.6 + 0.4*x
y2 <- 1*y


x3 <- c(0.42, 0.42, 0.55, 0.55, 0.60, 0.55, 0.55)
y3 <- c(0.45, 0.55, 0.55, 0.60, 0.50, 0.40, 0.45)

xpool <- c(x1, x2, x3)
ypool <- c(y1, y2, y3)

id <- c(c(rep(1,9),rep(2,4)), rep(3, length(x2)), rep(4, length(x3)))

fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=id,
               gp=gpar(fill=c("green","white", "red", "grey80"), 
                       lwd=1))
}else{
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=id,
               gp=gpar(col="green", 
                       lwd=10))
}






