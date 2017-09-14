
##########################  Fix multipanel plot fix    #############################

pwm1=cbind(c(1,9,5,5),c(1,16,2,1),c(1,1,18,0),c(1,0,2,17),c(18,1,0,1),c(1,18,0,0),c(1,2,16,1),c(6,4,7,3),c(2,12,1,5),c(8,5,5,2))
rownames(pwm1)=c('A','C','G','T')
colnames(pwm1)=1:ncol(pwm1)
pwm2=pwm1
pwm3=pwm1
library(grid)
grid.newpage()
layout.rows <- 2
layout.cols <- 2
top.vp <- viewport(layout=grid.layout(layout.rows, layout.cols,
                                      widths=unit(rep(6,layout.cols), rep("null", 2)),
                                      heights=unit(c(15,15), rep("lines", 2))))
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
logomaker(pwm1,color_profile = color_profile,pop_name = 'With penalty',newpage = F,control = list(single_panel=T))
seekViewport(paste0("plotlogo", 2))
logomaker(pwm2,color_profile = color_profile,pop_name = 'Without penalty',newpage = F,control = list(single_panel=T))
seekViewport(paste0("plotlogo", 3))
logomaker(pwm3,color_profile = color_profile,pop_name = 'Not applying dash',newpage = F,control = list(single_panel=T))
