

##############   letter   Q   ################################

grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))

angle <- c(seq(0, 2*pi, length.out=100))

y.l1 <- 0.5 + 0.45*sin(angle)
x.l1 <- 0.5 + 0.45*cos(angle)


y.l2 <- 0.5 + 0.3*sin(angle)
x.l2 <- 0.5 + 0.3*cos(angle)

x.l3 <- c(0.6, 0.8, 1, 0.8)
y.l3 <- c(0.3, 0.3, 0, 0)

id <- c(rep(1, length(x.l1)), rep(2, length(x.l2)), rep(3, length(x.l3)))

x <- c(x.l1, x.l2, x.l3)
y <- c(y.l1, y.l2, y.l3)

fill_symbol <- TRUE

if(fill_symbol){
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(fill=c("green","white", "green"), 
                       lwd=10))
}else{
  grid.polygon(x, y,
               default.unit="native",
               id=id,
               gp=gpar(col="green", 
                       lwd=10))
}


