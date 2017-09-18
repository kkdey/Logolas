

################  Test get_logo_heights functions   #########################

m = matrix(rep(0,48),4,12)
m[1,] = c(0,0,2.5,7,0,0,0,0,0,0,1,0)
m[2,] = c(4,6,3,1,0,0,0,0,0,5,0,5)
m[3,] = c(0,0,0,0,0,1,8,0,0,1,1,2)
m[4,] = c(4,2,2.5,0,8,7,0,8,8,2,6,1)
rownames(m) = c("A", "C", "G", "T")
colnames(m) = 1:12
m=m/8
get_logo_heights_log(m)
get_logo_heights_diff(m)
get_logo_heights_ratio(m)
get_logo_heights_ic_diff(m)
get_logo_heights_ic_log(m)
get_logo_heights_ic_log_odds(m)
get_logo_heights_ic_ratio(m)
get_logo_heights_log_odds(m)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))


nlogomaker(m,xlab = 'position',logoheight = "ic_log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "ic_diff",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "ic_ratio",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "ic_log_odds",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "log_odds",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "diff",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)

nlogomaker(m,xlab = 'position',logoheight = "ratio",
           color_profile = color_profile,
           bg = c(0.28, 0.22, 0.25, 0.25),
           frame_width = 1)




