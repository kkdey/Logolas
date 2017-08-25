

#############  Logolas on mutation signature data  ##########################

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

logomaker(table,
          color_profile = color_profile,
          frame_width = 1,
          ic.scale = TRUE,
          yscale_change=TRUE,
          xlab = "Position",
          ylab = "Information content")

logomaker(table,color_profile = color_profile,frame_width = 1)





ic=NULL
table <- table
total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
bg = NULL
frame_width=NULL
ic.scale=TRUE
xaxis=TRUE
yaxis=TRUE
xaxis_fontsize=10
xlab_fontsize=15
y_fontsize=15
main_fontsize=16
start=0.001
yscale_change=TRUE
pop_name = NULL
xlab = "X"
ylab = "Information content"
col_line_split="grey80"
addlogos = NULL
addlogos_text = NULL
newpage = TRUE
control = list()



