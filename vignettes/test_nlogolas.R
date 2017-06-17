

############  test nlogomaker()  ##############################
#####################  Example 1  #################################

counts_mat <- rbind(c(0.3, 0.8, 0.05, 0.05, 0.1),
                    c(0.3, 0.1, 0.15, 0.45, 0.1),
                    c(0.3, 0.05, 0.2, 0.45, 0.1),
                    c(0.1, 0.05, 0.6, 0.05, 0.7))
colnames(counts_mat) <- c("1", "2", "3", "4", "5")
rownames(counts_mat) <- c("C", "T", "A", "G")

col_vector <- c("blue", "green", "red", "orange")
color_profile <- list("type" = "per_row",
                      "col" = col_vector[1:4])

nlogomaker(counts_mat,
           color_profile = color_profile)


###################  Example  2  ###############################

mFile <- system.file("Exfiles/pwm1", package="seqLogo")
m <- read.table(mFile)
p <- seqLogo::makePWM(m)
pwm_mat <- slot(p,name = "pwm")
pwm_mat[,4] <- c(0.3, 0.3, 0.35, 0.05)

mat1 <- cbind(pwm_mat[,c(3,4)], rep(NA,4), pwm_mat[,c(5,6)]);
colnames(mat1) <- c("-2", "-1", "0", "1", "2")
mat2 <- cbind(rep(NA,6), rep(NA,6),
              c(0.8, 0.10, 0.03, 0.03, 0.0, 0),
              rep(NA,6), rep(NA,6))
rownames(mat2) <- c("C>T", "C>A", "C>G",
                    "T>A", "T>C", "T>G")

table <- rbind(mat1, mat2)

color_profile <- list("type" = "per_row",
                      "col" = RColorBrewer::brewer.pal(dim(table)[1],name ="Spectral"))

scale0=0.01
scale1=0.99


nlogomaker(table,
           color_profile = color_profile,
           ylimit = 1.2)


cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))

total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")

set.seed(20)
col_vector[c(1,3,7, 20, 43)] <- c("red", "blue", "orange", "green", "gray")
color_profile <- list("type" = "per_symbol",
                      "col" = col_vector)

nlogomaker(table,
           color_profile = color_profile,
           ylimit = 1.2)


color_profile <- list("type" = "per_column",
                      "col" = RColorBrewer::brewer.pal(dim(table)[2],name ="Spectral"))

nlogomaker(table,
           color_profile = color_profile)


########################  adjusting table   #######################

tab1 <- cbind(c(0.24, 0.28, 0.23, 0.25),
              rep(NA, 4),
              c(0.29, 0.28, 0.08, 0.35))

tab2 <- cbind(rep(NA, 6), c(0.92, 0.054, 0.007, 0.007, 0.007, 0.005),
              rep(NA, 6))

tab_pooled <- rbind(tab1, tab2)
rownames(tab_pooled) <- c("A", "C", "G", "T",
                          "C>T", "T>C", "T>G", "C>A", "C>G", "T>A")

colnames(tab_pooled) <- c("left \n flank", "mismatch", "right \n flank")

cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))

total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")
col_vector[c(1,3,7, 20, 43)] <- c("green", "blue", "orange", "red", "gray")
color_profile <- list("type" = "per_symbol",
                      "col" = col_vector)


nlogomaker(tab_pooled,
           color_profile = color_profile,
           ylimit = 2.1)





