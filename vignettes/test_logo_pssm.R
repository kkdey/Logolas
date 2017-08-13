

##################   Test Logo_PSSM plots in Logolas   ##################

library(Logolas)


##################  Preparing an example pwm matrix  ######################

m = matrix(rep(0,28),4,7)
m[1,] = c(-10,-6, -5, 4, -10, 4, 5)
m[2,] = c(-2,4, -6, 3, -3, 3, 0)
m[3,] = c(1,-5, -2, -1, 20, -3, -12)
m[4,] = c(10,1, 20, 0, 0, -4, 8)
rownames(m) = c("A", "C", "G", "T")
colnames(m) = 1:7



color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))


logo_pssm(m,xlab = 'position',
           color_profile = color_profile,
           frame_width = 1,
           control = list(viewport.margin.bottom = 3,
                          viewport.margin.left = 4,
                          viewport.margin.top = 2,
                          viewport.margin.right = 2))



logo_pssm(m,xlab = 'position',
          color_profile = color_profile,
          frame_width = 1)

logo_pssm(m,xlab = 'position',
          color_profile = color_profile,
          frame_width = 1,
          control = list(tofill_pos = TRUE, tofill_neg=FALSE))

