

################   G letter  ################################

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

h1 <- max(y.l1)
r1 <- max(x.l1)

h1 <- 0.4
x.add <- c(r1,0.5,0.5,r1-0.2,r1-0.2,r1,r1)
y.add <- c(h1,h1,h1-0.1,h1-0.1,0,0,h1)



id <- c(rep(1,length(x)),rep(2,length(x.add)))


x <- c(rev(x),x.add)
y <- c(rev(y),y.add)

fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(fill=c("orange","orange"), 
                       lwd=1))
}else{
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(col="orange", 
                       lwd=10))
}

