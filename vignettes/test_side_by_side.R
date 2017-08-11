
plot_list <- list()
par(mfrow = c(3, 1))  # 3 rows and 2 columns
for (i in c("Sturges", "st", "Scott")) {
  plot_list[[i]] <- hist(cars$speed, breaks = i, main = paste("method is", i, split = ""))
}


pushViewport(viewport(layout=grid.layout(1,2)))
print(plot_list[[1]], vp=viewport(layout.pos.col = 1))
print(plot_list[[2]], vp=viewport(layout.pos.col = 2))

a <- qplot(1:10, rnorm(10), main = "a")
b <- qplot(1:10, rnorm(10), main = "b")
c <- qplot(1:10, rnorm(10), main = "c")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1:2))  # key is to define vplayout
print(b, vp = vplayout(2, 1))
print(c, vp = vplayout(2, 2))



grid.newpage()

top.vp <- viewport(layout=grid.layout(3, 3,
                    widths=unit(rep(5,3), rep("null", 1)),
                    heights=unit(rep(5,3), rep("null", 1))))

plot_reg <- vpList()
l <- 1
for(i in 1:3){
  for(j in 1:3){
    plot_reg[[l]] <- viewport(layout.pos.col = i, layout.pos.row = j, name = paste0("plotlogo", l))
    l <- l+1
  }
}

splot <- vpTree(top.vp, plot_reg)




pushViewport(splot)
for(l in 1:9){

  x <- runif(10)
  y <- runif(10)

  seekViewport(paste0("plotlogo", l))
  grid.points(x,y)
  grid.xaxis()
  grid.yaxis()
}

