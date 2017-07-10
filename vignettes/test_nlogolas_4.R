
##########   Testing nlogolas with a different scaling configuration     ###############

library(Logolas)


##################  Preparing an example pwm matrix  ######################

m = matrix(rep(0,48),4,12)
m[1,] = c(0,0,2.5,7,0,0,0,0,0,0,1,0)
m[2,] = c(4,6,3,1,0,0,0,0,0,5,0,5)
m[3,] = c(0,0,0,0,0,1,8,0,0,1,1,2)
m[4,] = c(4,2,2.5,0,8,7,0,8,8,2,6,1)
rownames(m) = c("A", "C", "G", "T")
colnames(m) = 1:12
m=m/8


ll <- get_logo_heights_ic(m)
ll <- get_logo_heights_log(m)

table <- m
normalize = function(x){return(x/sum(x[!is.na(x)]))}

table <- apply(table+0.0001,2,normalize)

if (class(table) == "data.frame"){
  table <- as.matrix(table)
}else if (class(table) != "matrix"){
  stop("the table must be of class matrix or data.frame")
}

table_mat_norm <-  apply(table, 2, function(x) return(x/sum(x[!is.na(x)])))
npos <- ncol(table_mat_norm)
chars <- as.character(rownames(table_mat_norm))

table_mat_adj <- apply(table_mat_norm, 2, function(x)
{
  indices <- which(is.na(x))
  if(length(indices) == 0){
    y = log(x+1)
    z <- y - median(y)
    return(z)
  }else{
    w <- x[!is.na(x)]
    y <- log(w+1)
    z <- y - median(y)
    zext <- array(0, length(x))
    zext[indices] <- 0
    zext[-indices] <- z
    return(zext)
  }
})

table_mat_pos <- table_mat_adj
table_mat_pos[table_mat_pos<= 0] = 0

table_mat_neg <- table_mat_adj
table_mat_neg[table_mat_neg >= 0] = 0

table_mat_norm <- replace(table_mat_norm, is.na(table_mat_norm), 0)

for(j in 1:dim(table_mat_neg_norm)[2]){
  if(sum(table_mat_neg_norm[,j]) == 0){
    table_mat_neg_norm[,j] <- normalize(table_mat_neg_norm[,j]+1e-3)
  }
}

for(j in 1:dim(table_mat_pos_norm)[2]){
  if(sum(table_mat_pos_norm[,j]) == 0){
    table_mat_pos_norm[,j] <- normalize(table_mat_pos_norm[,j]+1e-3)
  }
}

