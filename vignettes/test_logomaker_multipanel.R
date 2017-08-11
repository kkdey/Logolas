

###########  test logomaker multi panel plots   ######################

m = matrix(rep(0,48),4,12)
m[1,] = c(0,0,2.5,7,0,0,0,0,0,0,1,2)
m[2,] = c(4,6,3,1,0,0,0,0,0,5,0,2)
m[3,] = c(0,0,0,0,0,1,8,0,0,1,1,2)
m[4,] = c(4,2,2.5,0,8,7,0,8,8,2,6,2)
rownames(m) = c("A", "C", "G", "T")
colnames(m) = 1:12
m=m/8

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))



grid.newpage()
layout.rows <- 1
layout.cols <- 2
top.vp <- viewport(layout=grid.layout(layout.rows, layout.cols,
                                      widths=unit(rep(6,layout.cols), rep("null", 2)),
                                      heights=unit(c(20,50), rep("lines", 2))))

plot_reg <- vpList()
l <- 1
for(i in 1:layout.rows){
  for(j in 1:layout.cols){
    plot_reg[[l]] <- viewport(layout.pos.col = j, layout.pos.row = i, name = paste0("plotlogo", l))
    l <- l+1
  }
}


plot_tree <- vpTree(top.vp, plot_reg)

pushViewport(plot_tree)
seekViewport(paste0("plotlogo", 1))
logomaker(m,xlab = 'position',color_profile = color_profile,
          bg = c(0.28, 0.22, 0.24, 0.26),
          frame_width = 1,
          newpage = FALSE,
          control = list(viewport.margin.left = 5))


seekViewport(paste0("plotlogo", 2))
nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1,
           control = list(logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5,
                          viewport.margin.left = 4),
           newpage = FALSE)



grid.newpage()
layout.rows <- 2
layout.cols <- 2
top.vp <- viewport(layout=grid.layout(layout.rows, layout.cols,
                                      widths=unit(rep(5,layout.cols), rep("null", 2)),
                                      heights=unit(rep(5,layout.rows), rep("null", 1))))

plot_reg <- vpList()
l <- 1
for(i in 1:layout.rows){
  for(j in 1:layout.cols){
    plot_reg[[l]] <- viewport(layout.pos.col = j, layout.pos.row = i, name = paste0("plotlogo", l))
    l <- l+1
  }
}


plot_tree <- vpTree(top.vp, plot_reg)

pushViewport(plot_tree)

for(l in 1:(layout.rows*layout.cols)){
  seekViewport(paste0("plotlogo", l))
  logomaker(m,xlab = 'position',color_profile = color_profile,
            bg = c(0.28, 0.22, 0.24, 0.26),
            frame_width = 1,
            newpage = FALSE)
}

