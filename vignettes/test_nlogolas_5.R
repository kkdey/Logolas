

######################   How to use dash   #################################

###################  We take an example from HOCOMOCO ######################


x1 <- rbind(c(1,	0, 	0, 	0, 	0, 	0, 	2, 	0, 	0, 	0, 	1, 	0, 	0, 	1),
            c(2,	0,	3,	5,	3,	0,	1,	0,	0,	4,	1,	4,	1,	1),
            c(1,	5,	1,	0,	0,	5,	2,	5,	5,	1,	2,	1,	4,	0),
            c(1,	0,	1,	0,	2,	0,	0,	0,	0,	0,	1,	0,	0,	3))

rownames(x1) <- c("A", "C", "G", "T")
colnames(x1) <- 1:dim(x1)[2]

#devtools::install_github("kkdey/dash")
library(dash)

library(SQUAREM)
ll <- dash(t(x1), optmethod = "mixEM",
           weight = list("center" = 10, "null" = 1, "corner" = 1),
           bf=FALSE)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))

m <- t(ll$posmean)
rownames(m) <- c("A", "C", "G", "T")
colnames(m) <- 1:dim(m)[2]
nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.05))

nlogomaker(x1,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.05))



x1 <- rbind(c(17,	12,	8,	33,	16,	6,	35,	3,	1,	20,	3,	3,	7,	13),
            c(10,	10,	2,	7,	1,	43,	2,	1,	28,	0,	1,	17,	7,	8),
            c(34,	25,	48,	33,	63,	2,	34,	72,	49,	17,	70,	47,	37,	32),
            c(19,	33,	22,	7,	0,	29,	9,	4,	2,	43,	6,	13,	29,	27))


rownames(x1) <- c("A", "C", "G", "T")
colnames(x1) <- 1:dim(x1)[2]

#devtools::install_github("kkdey/dash")
library(dash)

library(SQUAREM)
ll <- dash(t(x1), optmethod = "mixEM",
           weight = list("center" = 10, "null" = 1, "corner" = 1),
           bf=FALSE)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))

m <- t(ll$posmean)
rownames(m) <- c("A", "C", "G", "T")
colnames(m) <- 1:dim(m)[2]
nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.05))

nlogomaker(x1,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.05))


x1 <- rbind(c(2,	0,	6,	4,	0,	13,	14,	0,	0,	5),
            c(2,	6,	2,	2,	0,	0,	0,	0,	1,	0),
            c(1,	2,	4,	0,	0,	1,	0,	0,	6,	7),
            c(9,	6,	2,	8,	14,	0,	0,	14,	7,	2))

rownames(x1) <- c("A", "C", "G", "T")
colnames(x1) <- 1:dim(x1)[2]

#devtools::install_github("kkdey/dash")
library(dash)

library(SQUAREM)
ll <- dash(t(x1), optmethod = "mixEM",
           weight = list("center" = 100, "null" = 1, "corner" = 1),
           bf=TRUE)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))

m <- t(ll$posmean)
rownames(m) <- c("A", "C", "G", "T")
colnames(m) <- 1:dim(m)[2]
nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))

nlogomaker(x1,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))

x1 <- rbind(c(1,	2,	3,	1,	0,	1,	2,	3,	2,	4,	1,	3,	0,	1,	0,	0,	1,	4,	3,	3),
            c(1,	2,	1,	2,	0,	2,	0,	0,	0,	1,	4,	0,	6,	0,	0,	0,	0,	1,	1,	1),
            c(5,	5,	2,	3,	6,	5,	6,	2,	6,	4,	4,	5,	2,	8,	6,	9,	1,	3,	4,	3),
            c(2,	0,	3,	3,	3,	1,	1,	4,	1,	0,	0,	1,	1,	0,	3,	0,	7,	1,	1, 2))

rownames(x1) <- c("A", "C", "G", "T")
colnames(x1) <- 1:dim(x1)[2]

#devtools::install_github("kkdey/dash")
library(dash)

library(SQUAREM)
ll <- dash(t(x1), optmethod = "mixEM",
           weight = list("center" = 100, "null" = 1, "corner" = 1),
           bf=TRUE)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))

m <- t(ll$posmean)
rownames(m) <- c("A", "C", "G", "T")
colnames(m) <- 1:dim(m)[2]
nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))

nlogomaker(x1,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))


x1 <- rbind(c(4,	0,	5,	4,	0,	0,	3,	1),
            c(1,	0,	0,	0,	0,	0,	0,	1),
            c(0,	0,	0,	1,	0,	0,	2,	1),
            c(0,	5,	0,	0,	5,	5,	0,	2))

rownames(x1) <- c("A", "C", "G", "T")
colnames(x1) <- 1:dim(x1)[2]

#devtools::install_github("kkdey/dash")
library(dash)

library(SQUAREM)
ll <- dash(t(x1), optmethod = "mixEM",
           weight = list("center" = 10, "null" = 1, "corner" = 1),
           bf=TRUE)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))

m <- t(ll$posmean)
rownames(m) <- c("A", "C", "G", "T")
colnames(m) <- 1:dim(m)[2]
nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))

nlogomaker(x1,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))

x1 <- rbind(c(6,	14,	0,	33,	34,	0,	0,	18,	5,	15),
            c(4,	14,	1,	0,	0,	0,	0,	1,	18,	 5),
            c(22,	6,	0,	1,	0,	0,	2,	12,	9,	4),
            c(2,	0,	33,	0,	0,	34,	32,	3,	2,	10))

rownames(x1) <- c("A", "C", "G", "T")
colnames(x1) <- 1:dim(x1)[2]

#devtools::install_github("kkdey/dash")
library(dash)

library(SQUAREM)
ll <- dash(t(x1), optmethod = "mixEM",
           weight = list("center" = 100, "null" = 1, "corner" = 1),
           bf=TRUE)

color_profile = list("type" = "per_row",
                     "col" = RColorBrewer::brewer.pal(4,name ="Spectral"))

m <- t(ll$posmean)
rownames(m) <- c("A", "C", "G", "T")
colnames(m) <- 1:dim(m)[2]
nlogomaker(m,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))

nlogomaker(x1,xlab = 'position',logoheight = "ic",
           color_profile = color_profile,
           frame_width = 1,
           control = list(logscale = 1, quant = 0.5,
                          depletion_weight = 0.5))
