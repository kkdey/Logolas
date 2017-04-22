

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

table
table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x[!is.na(x)])))
npos <- ncol(table_mat_norm)
chars <- as.character(rownames(table_mat_norm))



table_mat_adj <- apply(table_mat_norm, 2, function(x)
                                          {
                                              indices <- which(is.na(x))
                                              if(length(indices) == 0){
                                                y = x
                                                z <- y - median(y)
                                                return(z)
                                              }else{
                                                y <- x[!is.na(x)]
                                                z <- y - median(y)
                                                zext <- array(0, length(x))
                                                zext[indices] <- 0
                                                zext[-indices] <- z
                                                return(zext)
                                          }
})

table_mat_pos <- table_mat_adj
table_mat_pos[table_mat_pos<= 0] = 0
table_mat_pos_norm  <- apply(table_mat_pos, 2, function(x) return(x/sum(x)))
table_mat_pos_norm[table_mat_pos_norm == "NaN"] = 0

table_mat_neg <- table_mat_adj
table_mat_neg[table_mat_neg >= 0] = 0
table_mat_neg_norm  <- apply(table_mat_neg, 2, function(x) return(x/sum(x)))
table_mat_neg_norm[table_mat_neg_norm == "NaN"] = 0

table_mat_norm <- replace(table_mat_norm, is.na(table_mat_norm), 0)

