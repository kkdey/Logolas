
data("N_Glycosyl_sequences")
sequences <- N_Glycosyl_sequences
bg <- apply(sequences, 1, function(x) return(median(x)))
bg <- bg/sum(bg)
sequences
bgmat <- bg %*% t(rep(1, dim(sequences)[2]))
bgmat[which(is.na(sequences))] <- NA

nP = min(colSums(sequences))
nQ = 1e05
bgmat_counts <- floor(nQ*bgmat)

logit_mat <- matrix(0, nrow(sequences), ncol(sequences))
se_logit_mat <- matrix(0, nrow(sequences), ncol(sequences))
for(m in 1:ncol(sequences)){
  total = sequences[,m] + bgmat_counts[,m]
  zero_ids <- which(sequences[,m] == 0)
  full_ids <- which((total - sequences[,m]) == 0)
  normal_ids <- setdiff(1:nrow(sequences), union(zero_ids, full_ids))
  if(length(zero_ids) > 0){
    logit_mat[zero_ids, m] <- log((sequences[zero_ids,m] + 0.5)/ (bgmat_counts[zero_ids, m] + 0.5)) - 0.5
  }
  if(length(full_ids) > 0){
    logit_mat[full_ids, m] <- log((sequences[full_ids,m] + 0.5)/ (bgmat_counts[full_ids, m] + 0.5)) + 0.5
  }
  if(length(normal_ids) > 0){
    logit_mat[normal_ids, m] <- log(sequences[normal_ids,m]/bgmat_counts[normal_ids, m])
  }
  Vthree <- (total+1)/(total) * (1/(sequences[,m]+1) + 1/(bgmat_counts[,m]+1))
  Vstar <- Vthree *  (1 - (2/total) + Vthree/2)
  se_logit_mat[, m] <- sqrt(Vstar - 0.5*Vthree^2*(Vthree - (4/total)))
}

ash_logit <- matrix(0, nrow(logit_mat), ncol(logit_mat))
for (m in 1:ncol(sequences)){
  fit <- ashr::ash(logit_mat[,m], se_logit_mat[,m], mode = "estimate")
  ash_logit[,m] <- fit$result$PosteriorMean
}

ash_adjust_median <- apply(ash_logit, 2, function(x) return(x - median(x)))
logit_adjust_median <- apply(logit_mat, 2, function(x) return(x - median(x)))
