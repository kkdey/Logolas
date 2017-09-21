

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




library(Logolas)

mFile <- system.file("Exfiles/pwm1", package="seqLogo")
m <- read.table(mFile)
p <- seqLogo::makePWM(m)
pwm_mat <- slot(p,name = "pwm")
mat1 <- cbind(pwm_mat[,c(3,4)], rep(NA,4), pwm_mat[,c(5,6)]);
colnames(mat1) <- c("-2", "-1", "0", "1", "2")
mat2 <- cbind(rep(NA,6), rep(NA,6),
              c(0.5, 0.2, 0.2, 0.05, 0.05, 0),
              rep(NA,6), rep(NA,6))
rownames(mat2) <- c("C>T", "C>A", "C>G",
                    "T>A", "T>C", "T>G")

table <- rbind(mat1, mat2)

color_profile <- list("type" = "per_symbol",
                      "col" = RColorBrewer::brewer.pal(dim(table)[1],name ="Spectral"))


get_logo_heights_log(table)
get_logo_heights_diff(table)
get_logo_heights_ratio(table)
get_logo_heights_ic_diff(table)
get_logo_heights_ic_log(table)
get_logo_heights_ic_log_odds(table)
get_logo_heights_ic_ratio(table)
get_logo_heights_log_odds(table)

nlogomaker(table,xlab = 'position',logoheight = "ratio",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "diff",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "log_odds",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "ic_ratio",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "ic_log",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "log",
           color_profile = color_profile,
           frame_width = 1)


nlogomaker(table,xlab = 'position',logoheight = "ic_log_odds",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "ic_diff",
           color_profile = color_profile,
           frame_width = 1)

nlogomaker(table,xlab = 'position',logoheight = "log_odds",
           color_profile = color_profile,
           frame_width = 1)



alpha = 1
epsilon = 0.01
bg = NULL
opt = 1
hist = FALSE
quant = 0.5

logoheight <- "log_odds"
total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
bg = NULL
frame_width=NULL
yscale_change=TRUE
pop_name = NULL
addlogos = NULL
addlogos_text = NULL
newpage = TRUE
yrange = NULL
xaxis=TRUE
yaxis=TRUE
xaxis_fontsize=10
xlab_fontsize=15
y_fontsize=15
main_fontsize=16
start=0.001
xlab = "X"
ylab = "Enrichment Score"
col_line_split="grey80"
control = list()

