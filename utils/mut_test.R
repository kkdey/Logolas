

# load("../Logolas-pages/data/Param_ind5_dir/Lymphoma-B-cell.6.Rdata")
# i <- 2
# mat=resultForSave[[1]]@signatureFeatureDistribution[i,,]
# mat1=cbind(t(mat[2:3,1:4]),rep(NA,4),t(mat[4:5,1:4]))
# rownames(mat1)=c('A','C','G','T')
# colnames(mat1) = c("-2", "-1", "0", "1", "2")
# mat2=cbind(rep(NA,6),rep(NA,6),(mat[1,]),rep(NA,6),rep(NA,6))
# colnames(mat2) = c("-2", "-1", "0", "1", "2")
# rownames(mat2) = c("C>A", "C>G", "C>T", "T>A", "T>C", "T>G")
# table = rbind(mat1, mat2)

data("mutation_sig")
table <- mutation_sig

cols = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category ==
                                       'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, cols$maxcolors, rownames(cols)))
col_vector = col_vector[-c(4,5)]
total_chars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O",
                "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "zero", "one", "two",
                "three", "four", "five", "six", "seven", "eight", "nine", "dot", "comma",
                "dash", "colon", "semicolon", "leftarrow", "rightarrow")

set.seed(20)
Logolas::logomaker(table,
                   type = "EDLogo",
                   color_type = "per_symbol",
                   color_seed = 2000,
                   pseudocount = 0.01,
                   logo_control = list(score = "log",
                                       y_fontsize=20,
                                       control= list(quant=0.5,
                                                     gap_ylab = 3,
                                                     round_off=0,
                                                     posbins = 2,
                                                     negbins = 3)))