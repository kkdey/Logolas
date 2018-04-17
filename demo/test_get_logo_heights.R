
#############  test get_logo_heights()   ########################

m = matrix(rep(0,48),4,12)
m[1,] = c(0,0,2.5,7,0,0,0,0,0,0,1,0)
m[2,] = c(4,6,3,1,0,0,0,0,0,5,0,5)
m[3,] = c(0,0,0,0,0,1,8,0,0,1,1,2)
m[4,] = c(4,2,2.5,0,8,7,0,8,8,2,6,1)
rownames(m) = c("A", "C", "G", "T")
colnames(m) = 1:12
m=m/8

get_logo_heights(m, score = "log")
get_logo_heights(m, score = "log", ic = TRUE)
get_logo_heights(m, score = "wKL")
get_logo_heights(m, score = "probKL", ic = TRUE)
