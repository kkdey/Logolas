
########  test script for Logols and negative Logolas  ##################


devtools::install_github("kkdey/Logolas")
library(Logolas)


##################  Preparing an example pwm matrix  ######################

m = matrix(rep(0,48),4,12)
m[1,] = c(0,0,2.5,7,0,0,0,0,0,0,1,2)
m[2,] = c(4,6,3,1,0,0,0,0,0,5,0,2)
m[3,] = c(0,0,0,0,0,1,8,0,0,1,1,2)
m[4,] = c(4,2,2.5,0,8,7,0,8,8,2,6,2)
rownames(m) = c("A", "C", "G", "T")
colnames(m) = 1:12
m=m/8

##############  check if the column sums are one  #####################

colSums(m)


################  setting up the color profile in Logolas  ##############
######### see vignette in https://bioconductor.org/packages/release/bioc/vignettes/Logolas/inst/doc/logolas.pdf


color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))


######## entropy based logo making

logomaker(m,xlab = 'position',color_profile = color_profile, frame_width = 1)

####### enrichment logo building

nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.25, 0.25, 0.25, 0.25),
           frame_width = 1,
           control = list(logscale = 0.2, quant = 0.5,
           depletion_weight = 0.5))

nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.4, 0.1, 0.1, 0.4),
           frame_width = 1,
           control = list(logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5))

nlogomaker(m,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           bg = c(0.1, 0.1, 0.4, 0.4),
           frame_width = 1,
           control = list(logscale = 0.2, quant = 0.5,
                          depletion_weight = 0.5))


nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           bg = c(0.28, 0.24, 0.22, 0.26),
           frame_width = 1, control = list(logscale = 2))

nlogomaker(m,xlab = 'position',logoheight = "log_odds",
           color_profile = color_profile,
           bg = c(0.28, 0.24, 0.22, 0.26),
           frame_width = 1, control = list(log_odds_scale=0.2,
           depletion_weight = 0.50))

#######  enrichment heights scaling  ################

ll1a <- get_logo_heights_ic(m, bg = c(0.4, 0.1, 0.1, 0.4))
ll1b <- get_logo_heights_ic(m)
ll1c <- get_logo_heights_ic(m, bg = c(0.25, 0.25, 0.25, 0.25))
ll1d <- get_logo_heights_ic(m, bg = c(0, 0.5, 0, 0.5))

ll2a <- get_logo_heights_log(m, bg = c(0.3, 0.2, 0.2, 0.3))
ll2b <- get_logo_heights_log(m, bg = c(0.25, 0.25, 0.25, 0.25))
ll2c <- get_logo_heights_log(m, bg = c(0, 0.5, 0, 0.5))

ll3 <- get_logo_heights_log_odds(m)

ic2 <- neg_ic_computer(m)

ic2$ic_pos
ic2$ic_neg
ic2$scales

# R version 3.3.3 (2017-03-06)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: macOS Sierra 10.12
#
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] Logolas_1.1.2
#
# loaded via a namespace (and not attached):
#   [1] crayon_1.3.2         assertthat_0.2.0     digest_0.6.12        withr_1.0.2
# [5] grid_3.3.3           R6_2.2.1             git2r_0.18.0         BiocInstaller_1.24.0
# [9] httr_1.2.1           debugme_1.0.2        rlang_0.1.1.9000     curl_2.6
# [13] callr_1.0.0.9000     devtools_1.13.1.9000 RColorBrewer_1.1-2   tools_3.3.3
# [17] processx_1.0.0.9000  pkgload_0.0.0.9000   pkgbuild_0.0.0.9000  memoise_1.1.0
# [21] knitr_1.15.1
