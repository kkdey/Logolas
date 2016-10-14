

#########  read A  #############################

rm(list=ls())
require('EBImage')
Image <- readImage('A_draw.png')
colorMode(Image) <- Grayscale
y <- resize(Image, w = 500, h = 100)
display(y)
co_ordinates <- which(y == 0, arr.ind=T)[,1:2]

x_coord <- (co_ordinates[,1] - min(co_ordinates[,1]))/(max(co_ordinates[,1]) - min(co_ordinates[,1]))
y_coord <- (co_ordinates[,2] - min(co_ordinates[,2]))/(max(co_ordinates[,2]) - min(co_ordinates[,2]))

plot(x_coord, y_coord, col="red")

library(grid)
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))
grid.polygon(x_coord, y_coord, default.unit="native", gp=gpar(fill=c("green")))


grid.text("TRUMP")

library(grid)
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))
grid.text("A", just = "centre", x = unit(0.5, "native"), y = unit(0.5, "native"),
          gp = gpar(fontsize = 350, fontface = "bold"))


library(grid)
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))
grid.text("C", just = "centre", x = unit(0.5, "native"), y = unit(0.5, "native"),
          gp = gpar(fontsize = 350, fontface = "bold"))

library(grid)
grid.newpage()
pushViewport(viewport(x=0.5,y=0.5,width=1, height=1,
                      clip=TRUE))
grid.rect(gp=gpar(col="grey"))
grid.text("MARK", just = "centre", x = unit(0.5, "native"), y = unit(0.5, "native"),
          gp = gpar(fontsize = 50, fontface = "bold"))
