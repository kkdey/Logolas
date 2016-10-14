

#############   Letter  C to A  ###########################
library(grid)
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))

angle1 <- seq(0.3+pi/2,pi,length=100)
angle2 <- seq(pi,1.5*pi,length=100)
x.l1 <- 0.5 + 0.5*sin(angle1)
y.l1 <- 0.5 + 0.5*cos(angle1)
x.l2 <- 0.5 + 0.5*sin(angle2)
y.l2 <- 0.5 + 0.5*cos(angle2)

x.l <- c(x.l1,x.l2)
y.l <- c(y.l1,y.l2)

x <- c(x.l,rev(x.l))
y <- c(y.l,1-rev(y.l))

x.i1 <- 0.5 +0.35*sin(angle1)
y.i1 <- 0.5 +0.35*cos(angle1)
x.i1 <- x.i1[y.i1<=max(y.l1)]
y.i1 <- y.i1[y.i1<=max(y.l1)]
y.i1[1] <- max(y.l1)

x.i2 <- 0.5 +0.35*sin(angle2)
y.i2 <- 0.5 +0.35*cos(angle2)

x.i <- c(x.i1,x.i2)
y.i <- c(y.i1,y.i2)

x1 <- c(x.i,rev(x.i))
y1 <- c(y.i,1-rev(y.i))

x <- c(x,rev(x1))
y <- c(y,rev(y1))
x1 <- 0.4*x
y1 <- 1*y


x <- 0.1* (c(0,4,6,10,8,6.8,3.2,2,0,3.6,5,6.4,3.6))
y <- 0.1*(c(0,10,10,0,0,3,3,0,0,4,7.5,4,4))
x2 <- 0.6 + 0.4*x
y2 <- 1*y

x3 <- c(0.42, 0.42, 0.55, 0.55, 0.60, 0.55, 0.55)
y3 <- c(0.45, 0.55, 0.55, 0.60, 0.50, 0.40, 0.45)

xpool <- c(x1, x2, x3)
ypool <- c(y1, y2, y3)

id <- c(rep(1,length(x1)), c(rep(2,9),rep(3,4)), rep(4, length(x3)))

fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=id,
               gp=gpar(fill=c("blue","green", "white", "grey80"), 
                       lwd=1))
}else{
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=id,
               gp=gpar(col="green", 
                       lwd=10))
}