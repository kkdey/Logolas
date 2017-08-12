

###############  test logomaker with border and solid fill  ###############################

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


######## entropy based logo making

logomaker(m,xlab = 'position',color_profile = color_profile,
          bg = c(0.28, 0.22, 0.24, 0.26),
          frame_width = 1,
          control = list(tofill = TRUE, lwd = 10))

logomaker(m,xlab = 'position',color_profile = color_profile,
          bg = c(0.28, 0.22, 0.24, 0.26),
          frame_width = 1,
          control = list(tofill = FALSE, lwd = 3))




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
          control = list(tofill = TRUE, lwd = 10),
          newpage = FALSE)


seekViewport(paste0("plotlogo", 2))
logomaker(m,xlab = 'position',color_profile = color_profile,
          bg = c(0.28, 0.22, 0.24, 0.26),
          frame_width = 1,
          control = list(tofill = FALSE, lwd = 3),
          newpage = FALSE)



cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))
makemylogo("Evening", plot=TRUE, tofill = FALSE, colfill=col_vector)
makemylogo("Evening", plot = TRUE, tofill=FALSE, colfill = "orange")
makemylogo("ag", plot=TRUE, tofill = FALSE, colfill=col_vector)

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

seekViewport(paste0("plotlogo", 1))
nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1,
           control = list(tofill_pos = TRUE, tofill_neg=FALSE,
                          logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5),
           newpage = FALSE)

seekViewport(paste0("plotlogo", 2))
nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1,
           control = list(tofill_pos = FALSE, tofill_neg=TRUE,
                          logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5),
           newpage = FALSE)

seekViewport(paste0("plotlogo", 3))
nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1,
           control = list(tofill_pos = TRUE, tofill_neg=TRUE,
                          logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5),
           newpage = FALSE)

seekViewport(paste0("plotlogo", 4))
nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1,
           control = list(tofill_pos = FALSE, tofill_neg=FALSE,
                          logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5),
           newpage = FALSE)




