

#############   Letter  T to G  ###########################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))

x <- c( 0.4, 0.4, 0, 0, 1, 1, 0.6, 0.6)
y <- c( 0, 0.85, 0.85, 1, 1, 0.85, 0.85, 0)
x1_pool <- 0.4*x
y1_pool <- 1*y
id1_pool <- rep(1,length(x1_pool))


x3_pool <- c(0.42, 0.42, 0.55, 0.55, 0.60, 0.55, 0.55)
y3_pool <- c(0.45, 0.55, 0.55, 0.60, 0.50, 0.40, 0.45)


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



id2_pool <- c(rep(2,length(x)),rep(3,length(x.add)))


x2_pool <- 0.6 + 0.4*c(rev(x),x.add)
y2_pool <- c(rev(y),y.add)


id <- c(id1_pool, id2_pool, rep(4, length(x3_pool)))

xpool <- c(x1_pool, x2_pool, x3_pool)
ypool <- c(y1_pool, y2_pool, y3_pool)


fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=id,
               gp=gpar(fill=c("red","orange","orange", "grey80"), 
                       lwd=1))
}else{
  grid.polygon(xpool, ypool,
               default.unit="native",
               id=id,
               gp=gpar(col=c("red","orange","orange", "grey80"),
                       lwd=10))
}



