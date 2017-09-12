

#################   Height discrepancy in logolas plots  ##################


library(Logolas)
pwm=matrix(c(0.8,0.1,0.1,0,0.5,0.4,0,0.1,0.6,0.4,0,0,0.4,0.4,0.1,0.1,0.5,0,0.2,0.3,0.35,0.35,0.06,0.24,0.4,0.3,0.2,0.1,0.4,0.2,0.2,0.2,0.28,0.24,0.24,0.24),
                           nrow = 4,byrow = F)
rownames(pwm)=c('A','C','G','T')
colnames(pwm)=1:ncol(pwm)

ll <- get_logo_heights_log(pwm,depletion_weight = 0)

color_profile <-list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))
nlogomaker(pwm,color_profile = color_profile,logoheight ='log',
           frame_width = 1,control =list(depletion_weight=0))


ll <- get_logo_heights_log(pwm,depletion_weight = 0.7)

nlogomaker(pwm,color_profile = color_profile,
           logoheight ='log',frame_width = 1,control =list(depletion_weight=0.7))
